{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Brand
    (module Schema.CapNProto.Reader.Schema.Brand)
  where

import Language.CapNProto.TH

import qualified Data.CapNProto.Untyped         as U
import qualified Schema.CapNProto.Reader.Schema as S

$(mkStructWrappers
    [ "Scope"
    , "Binding"
    ])

$(mkListReaders 'S.Brand
    [ ("scopes", 0, 'U.ListStruct, ''Scope, [| Scope |])
    ])
