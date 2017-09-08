{-# LANGUAGE ConstraintKinds, TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.CodeGeneratorRequest where

import qualified Schema.CapNProto.Reader.Schema as S
import qualified Data.CapNProto.Untyped as U

import Language.CapNProto.TH

$(mkStructWrappers ["RequestedFile"])

$(mkListReaders
    'S.CodeGeneratorRequest
    [ ("nodes",          'S.Node,        0, 'U.ListStruct)
    , ("requestedFiles", 'RequestedFile, 1, 'U.ListStruct)
    ])
