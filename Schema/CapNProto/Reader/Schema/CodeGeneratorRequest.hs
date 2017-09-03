{-# LANGUAGE ConstraintKinds, TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.CodeGeneratorRequest where

import qualified Schema.CapNProto.Reader.Schema as S

import Data.CapNProto.TH

$(mkStructWrappers ["RequestedFile"])

$(mkListReaders
    'S.CodeGeneratorRequest
    [ ("nodes",          'S.Node, 0)
    , ("requestedFiles", 'RequestedFile, 1)
    ])
