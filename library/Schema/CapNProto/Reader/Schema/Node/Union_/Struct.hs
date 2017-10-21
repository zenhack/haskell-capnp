{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Node.Union_.Struct where

import Language.CapNProto.TH

import qualified Data.CapNProto.Untyped as U
import qualified Schema.CapNProto.Reader.Schema as S
import qualified Schema.CapNProto.Reader.Schema.Node.Union_ as Union_

$(mkListReaders
    'Union_.Struct
    [ ("fields", 3, 'U.ListStruct, ''S.Field, [| S.Field |])
    ])
