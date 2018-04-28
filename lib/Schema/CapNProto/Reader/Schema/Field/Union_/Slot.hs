{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Field.Union_.Slot where

import Data.Word

import Language.CapNProto.TH

import qualified Data.CapNProto.Untyped                      as U
import qualified Schema.CapNProto.Reader.Schema              as S
import qualified Schema.CapNProto.Reader.Schema.Field.Union_ as Union_

$(mkWordReader WordReaderSpec
    { name = "offset"
    , parentConName = 'Union_.Slot
    , start = 0
    , rawTyp = ''Word32
    , typ = const [t| Word32 |]
    , defaultVal = 0
    , transform = [| Prelude.id |]
    })

$(mkBoolReader "hadExplicitDefault" 'Union_.Slot 128 False)
