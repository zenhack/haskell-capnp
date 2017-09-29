{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Node.Union_
    (module Schema.CapNProto.Reader.Schema.Node.Union_)
  where

import Data.CapNProto.Bits   (Word1, word1ToBool)
import Data.Word
import Language.CapNProto.TH

import qualified Data.CapNProto.Untyped as U
import qualified Schema.CapNProto.Reader.Schema as S

$(mkStructWrappers
    [ "Struct"
    , "Enum"
    , "Interface"
    , "Const"
    , "Annotation"
    ])

$(mkWordReaders 'Struct
    [ ("dataWordCount", 112, ''Word16, const [t| Word16 |], 0, [| id |])
    , ("pointerCount",  192, ''Word16, const [t| Word16 |], 0, [| id |])

    , ( "preferredListEncoding"
      , 208
      , ''Word16
      , const [t| S.ElementSize |]
      , 0
      , [| S.ElementSize |]
      )

    , ("isGroup", 224, ''Word1, const [t| Bool |], 0, [| word1ToBool |])

    , ("disciriminantCount",  240, ''Word16, const [t| Word16 |], 0, [| id |])
    , ("disciriminantOffset", 256, ''Word32, const [t| Word32 |], 0, [| id |])
    ])

$(mkListReaders 'Struct
    [ ("fields", 3, 'U.ListStruct, ''S.Field, [| S.Field |])
    ])
