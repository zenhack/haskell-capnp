{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Node
    (module Schema.CapNProto.Reader.Schema.Node)
  where

import           Data.CapNProto.Bits            (Word1, word1ToBool)
import qualified Data.CapNProto.Untyped         as U
import           Data.Word
import           Language.CapNProto.TH
import qualified Schema.CapNProto.Reader.Schema as S

$(mkStructWrappers ["Parameter", "NestedNode"])

$(mkListReaders 'S.Node
    [ ("parameters",  5, 'U.ListStruct, ''Parameter,    [| Parameter    |])
    , ("nestedNodes", 1, 'U.ListStruct, ''NestedNode,   [| NestedNode   |])
    , ("annotations", 2, 'U.ListStruct, ''S.Annotation, [| S.Annotation |])
    ])

$(mkWordReaders 'S.Node
    [ ("id",                        0, ''Word64, const [t| Word64 |], 0, [| id          |])
    , ("displayNamePrefixLength",  64, ''Word32, const [t| Word32 |], 0, [| id          |])
    , ("scopeId",                 128, ''Word64, const [t| Word64 |], 0, [| id          |])
    , ("isGeneric",               288, ''Word1,  const [t| Bool   |], 0, [| word1ToBool |])
    ])
