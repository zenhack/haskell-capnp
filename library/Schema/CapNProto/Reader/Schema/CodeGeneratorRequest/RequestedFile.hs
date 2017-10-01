{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.CodeGeneratorRequest.RequestedFile
    (module Schema.CapNProto.Reader.Schema.CodeGeneratorRequest.RequestedFile)
  where

import Data.Word
import Language.CapNProto.TH

import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest as CGR
import qualified Data.CapNProto.Untyped as U

$(mkStructWrappers ["Import"])

$(mkTextReader "filename" 'CGR.RequestedFile 0)

$(mkListReaders 'CGR.RequestedFile
    [ ("imports", 1, 'U.ListStruct, ''Import, [| Import |])
    ])

$(mkWordReaders 'CGR.RequestedFile
    [ ("id", 0, ''Word64, const [t| Word64 |], 0, [| id |])
    ])
