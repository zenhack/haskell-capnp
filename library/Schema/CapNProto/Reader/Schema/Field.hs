{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Field
    (module Schema.CapNProto.Reader.Schema.Field)
  where

import Data.Bits
import Data.Word
import Language.CapNProto.TH

import qualified Data.CapNProto.Untyped                      as U
import qualified Schema.CapNProto.Reader.Schema              as S
import qualified Schema.CapNProto.Reader.Schema.Field.Union_ as Union_

noDiscriminant :: Word16
noDiscriminant = 0xffff

$(mkListReaders 'S.Field
    [ ("annotations", 1, 'U.ListStruct, ''S.Annotation, [| S.Annotation |])
    ])

$(mkWordReader WordReaderSpec
    { name = "codeOrder"
    , parentConName = 'S.Field
    , start = 0
    , rawTyp = ''Word16
    , typ = const [t| Word16 |]
    , defaultVal = 0
    , transform = [| id |]
    })

$(mkWordReader WordReaderSpec
    { name = "discriminantValue"
    , parentConName = 'S.Field
    , start = 0
    , rawTyp = ''Word16
    , typ = const [t| Word16 |]
    , defaultVal = 0xffff
    , transform = [| id |]
    })

$(mkTextReader "name" 'S.Field 0)

union_ :: U.ReadCtx m b => S.Field b -> m (Union_ b)
union_ (S.Field struct) = do
    let dataSec = U.dataSection struct
    let dataIndex = 64 `div` 64
    let bitOffset = 64 `mod` 64
    word <- U.index dataIndex dataSec
    let tag = fromIntegral (word `shiftR` bitOffset) :: Word16
    return $ case tag of
        0 -> Slot (Union_.Slot struct)
        1 -> Group (Union_.Group struct)
        _ -> Unknown tag

data Union_ b
    = Slot (Union_.Slot b)
    | Group (Union_.Group b)
    | Unknown !Word16
