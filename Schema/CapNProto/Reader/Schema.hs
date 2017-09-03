{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema where

import Data.CapNProto.TH
import Data.Word

type Id = Word64

$(mkStructWrappers
    [ "Node"
    , "Field"
    , "Superclass"
    , "Method"
    , "Type"
    , "Brand"
    , "Value"
    , "Annotation"
    , "CodeGeneratorRequest"
    ])

newtype ElementSize b = ElementSize Word16
