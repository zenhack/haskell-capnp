{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Superclass
    (module Schema.CapNProto.Reader.Schema.Superclass)
  where

import Prelude hiding (id)

import Data.Word
import Language.CapNProto.TH

import qualified Prelude
import qualified Schema.CapNProto.Reader.Schema as S

$(mkWordReaders 'S.Superclass
    [ ("id", 0, ''Word64, const [t| Word64 |], 0, [| Prelude.id |])
    ])
