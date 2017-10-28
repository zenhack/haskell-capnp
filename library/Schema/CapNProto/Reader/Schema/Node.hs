{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Node
    (module Schema.CapNProto.Reader.Schema.Node)
  where

import Prelude hiding (id)

import Data.Bits
import Data.CapNProto.Bits   (Word1, word1ToBool)
import Data.Word
import Language.CapNProto.TH

import qualified Data.CapNProto.Untyped                     as U
import qualified Prelude
import qualified Schema.CapNProto.Reader.Schema             as S
import qualified Schema.CapNProto.Reader.Schema.Node.Union_ as Union_

$(mkStructWrappers ["Parameter", "NestedNode"])

$(mkListReaders 'S.Node
    [ ("parameters",  5, 'U.ListStruct, ''Parameter,    [| Parameter    |])
    , ("nestedNodes", 1, 'U.ListStruct, ''NestedNode,   [| NestedNode   |])
    , ("annotations", 2, 'U.ListStruct, ''S.Annotation, [| S.Annotation |])
    ])

$(mkWordReader WordReaderSpec
    { name = "id"
    , parentConName = 'S.Node
    , start = 0
    , rawTyp = ''Word64
    , typ = const [t| Word64 |]
    , defaultVal = 0
    , transform = [| Prelude.id |]
    })

$(mkWordReader WordReaderSpec
    { name = "displayNamePrefixLength"
    , parentConName = 'S.Node
    , start = 64
    , rawTyp = ''Word32
    , typ = const [t| Word32 |]
    , defaultVal = 0
    , transform = [| Prelude.id |]
    })

$(mkWordReader WordReaderSpec
    { name = "scopeId"
    , parentConName = 'S.Node
    , start = 128
    , rawTyp = ''Word64
    , typ = const [t| Word64 |]
    , defaultVal = 0
    , transform = [| Prelude.id |]
    })

$(mkBoolReader "isGeneric" 'S.Node 288 False)

$(mkTextReader "displayName" 'S.Node 0)

union_ :: U.ReadCtx m b => S.Node b -> m (Union_ b)
union_ (S.Node struct) = do
    dataSec <- U.dataSection struct
    let dataIndex = 96 `div` 64
    let bitOffset = 96 `mod` 64
    word <- U.index dataIndex dataSec
    let tag = fromIntegral (word `shiftR` bitOffset) :: Word16
    return $ case tag of
        0 -> File
        1 -> Struct (Union_.Struct struct)
        2 -> Enum (Union_.Enum struct)
        3 -> Interface (Union_.Interface struct)
        4 -> Const (Union_.Const struct)
        5 -> Annotation (Union_.Annotation struct)
        _ -> Unknown tag

type Id = Word64

data Union_ b
    = File
    | Struct (Union_.Struct b)
    | Enum (Union_.Enum b)
    | Interface (Union_.Interface b)
    | Const (Union_.Const b)
    | Annotation (Union_.Annotation b)
    | Unknown Word16
