{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Node where

import Language.CapNProto.TH
import qualified Schema.CapNProto.Reader.Schema as S
import qualified Data.CapNProto.Untyped as U

$(mkStructWrappers ["Parameter", "NestedNode"])

$(mkListReaders 'S.Node
    [ ("parameters",  'Parameter,    5, 'U.ListStruct)
    , ("nestedNodes", 'NestedNode,   1, 'U.ListStruct)
    , ("annotations", 'S.Annotation, 2, 'U.ListStruct)
    ])
