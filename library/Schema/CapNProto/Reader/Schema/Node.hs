{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Node where

import Data.Word
import Language.CapNProto.TH
import qualified Schema.CapNProto.Reader.Schema as S
import qualified Data.CapNProto.Untyped as U

$(mkStructWrappers ["Parameter", "NestedNode"])

$(mkListReaders 'S.Node
    [ ("parameters",  'Parameter,    5, 'U.ListStruct)
    , ("nestedNodes", 'NestedNode,   1, 'U.ListStruct)
    , ("annotations", 'S.Annotation, 2, 'U.ListStruct)
    ])

$(mkWordReaders 'S.Node
    [ ("id",                        0, ''Word64, 0, 'id)
    , ("displayNamePrefixLength",  64, ''Word32, 0, 'id)
    , ("scopeId",                 128, ''Word64, 0, 'id)
    ])
