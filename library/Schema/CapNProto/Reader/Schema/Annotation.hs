{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Annotation
    (module Schema.CapNProto.Reader.Schema.Annotation)
  where

import Prelude hiding (id)

import Data.Word
import Language.CapNProto.TH

import qualified Prelude
import qualified Schema.CapNProto.Reader.Schema as S

$(mkWordReaders 'S.Annotation
    [ ("id", 0, ''Word64, const [t| Word64 |], 0, [| Prelude.id |])
    ])
