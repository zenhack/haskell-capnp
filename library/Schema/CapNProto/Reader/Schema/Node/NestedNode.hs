{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Node.NestedNode
    (module Schema.CapNProto.Reader.Schema.Node.NestedNode)
  where

import Data.Word
import Language.CapNProto.TH

import qualified Schema.CapNProto.Reader.Schema.Node as N

$(mkTextReader "name" 'N.NestedNode 0)

$(mkWordReaders 'N.NestedNode
    [ ("id", 0, ''Word64, const [t| Word64 |], 0, [| id |])
    ])
