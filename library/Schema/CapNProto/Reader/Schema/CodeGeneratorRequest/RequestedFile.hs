{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.CodeGeneratorRequest.RequestedFile
    (module Schema.CapNProto.Reader.Schema.CodeGeneratorRequest.RequestedFile)
  where

import Data.Word
import Language.CapNProto.TH

import qualified Data.CapNProto.Untyped                              as U
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest as CGR

$(mkStructWrappers ["Import"])

$(mkTextReader "filename" 'CGR.RequestedFile 0)

$(mkListReaders 'CGR.RequestedFile
    [ ("imports", 1, 'U.ListStruct, ''Import, [| Import |])
    ])

$(mkWordReader WordReaderSpec
    { name = "id"
    , parentConName = 'CGR.RequestedFile
    , start = 0
    , rawTyp = ''Word64
    , typ = const [t| Word64 |]
    , defaultVal = 0
    , transform = [| id |]
    })
