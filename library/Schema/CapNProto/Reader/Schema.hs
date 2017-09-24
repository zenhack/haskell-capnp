{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema
    (module Schema.CapNProto.Reader.Schema)
  where

import Data.Word
import Language.CapNProto.TH

type Id = Word64

$(mkStructWrappers
    [ "Node"
    , "Field"
    , "Enumerant"
    , "Superclass"
    , "Method"
    , "Type"
    , "Brand"
    , "Value"
    , "Annotation"
    , "CodeGeneratorRequest"
    ])

newtype ElementSize b = ElementSize Word16
