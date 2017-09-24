{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.CodeGeneratorRequest
    (module Schema.CapNProto.Reader.Schema.CodeGeneratorRequest)
  where

import qualified Data.CapNProto.Untyped         as U
import qualified Schema.CapNProto.Reader.Schema as S

import Language.CapNProto.TH

$(mkStructWrappers ["RequestedFile"])

$(mkListReaders
    'S.CodeGeneratorRequest
    [ ("nodes",          0, 'U.ListStruct, ''S.Node,        [| S.Node        |])
    , ("requestedFiles", 1, 'U.ListStruct, ''RequestedFile, [| RequestedFile |])
    ])
