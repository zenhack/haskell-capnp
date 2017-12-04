{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Node.NestedNode
    (module Schema.CapNProto.Reader.Schema.Node.NestedNode)
  where

import Data.Word
import Language.CapNProto.TH hiding (name)

import qualified Language.CapNProto.TH               as TH
import qualified Schema.CapNProto.Reader.Schema.Node as N

$(mkTextReader "name" 'N.NestedNode 0)

$(mkWordReader WordReaderSpec
    { TH.name = "id"
    , parentConName = 'N.NestedNode
    , start = 0
    , rawTyp = ''Word64
    , typ = const [t| Word64 |]
    , defaultVal = 0
    , transform = [| id |]
    })
