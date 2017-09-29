{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Node.Union_
    (module Schema.CapNProto.Reader.Schema.Node.Union_)
  where

import Language.CapNProto.TH

import qualified Schema.CapNProto.Reader.Schema as S

$(mkStructWrappers
    [ "Struct"
    , "Enum"
    , "Interface"
    , "Const"
    , "Annotation"
    ])
