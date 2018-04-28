{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Superclass
    (module Schema.CapNProto.Reader.Schema.Superclass)
  where

import Prelude hiding (id)

import Data.Word
import Language.CapNProto.TH

import qualified Prelude
import qualified Schema.CapNProto.Reader.Schema as S

$(mkWordReader WordReaderSpec
    { name = "id"
    , parentConName = 'S.Superclass
    , start = 0
    , rawTyp = ''Word64
    , typ = const [t| Word64 |]
    , defaultVal = 0
    , transform = [| Prelude.id |]
    })
