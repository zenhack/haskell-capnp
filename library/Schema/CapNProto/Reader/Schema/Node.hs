{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Node where

import Data.Word
import Language.CapNProto.TH
import qualified Schema.CapNProto.Reader.Schema as S
import qualified Data.CapNProto.Untyped as U
import Data.CapNProto.Bits (Word1, word1ToBool)

$(mkStructWrappers ["Parameter", "NestedNode"])

$(mkListReaders 'S.Node
    [ ("parameters",  5, 'U.ListStruct, 'Parameter)
    , ("nestedNodes", 1, 'U.ListStruct, 'NestedNode)
    , ("annotations", 2, 'U.ListStruct, 'S.Annotation)
    ])

$(mkWordReaders 'S.Node
    [ ("id",                        0, ''Word64, 0, [| id          |])
    , ("displayNamePrefixLength",  64, ''Word32, 0, [| id          |])
    , ("scopeId",                 128, ''Word64, 0, [| id          |])
    , ("isGeneric",               288, ''Word1,  0, [| word1ToBool |])
    ])
