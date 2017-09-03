{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Node where

import Data.CapNProto.TH
import qualified Schema.CapNProto.Reader.Schema as S

$(mkStructWrappers ["Parameter", "NestedNode"])

$(mkListReaders 'S.Node
    [ ("parameters",  'Parameter,    5)
    , ("nestedNodes", 'NestedNode,   1)
    , ("annotations", 'S.Annotation, 2)
    ])
