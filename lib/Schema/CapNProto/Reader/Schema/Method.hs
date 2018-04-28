{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Method
    (module Schema.CapNProto.Reader.Schema.Method)
  where

import Data.Word
import Language.CapNProto.TH hiding (name)

import qualified Data.CapNProto.Untyped              as U
import qualified Language.CapNProto.TH               as TH
import qualified Schema.CapNProto.Reader.Schema      as S
import qualified Schema.CapNProto.Reader.Schema.Node as Node

noDiscriminant :: Word16
noDiscriminant = 0xffff

$(mkListReaders 'S.Method
    [ ("annotations",        1, 'U.ListStruct, ''S.Annotation,   [| S.Annotation   |])
    , ("implicitParameters", 7, 'U.ListStruct, ''Node.Parameter, [| Node.Parameter |])
    ])

$(mkWordReader WordReaderSpec
    { TH.name = "codeOrder"
    , parentConName = 'S.Method
    , start = 0
    , rawTyp = ''Word16
    , typ = const [t| Word16 |]
    , defaultVal = 0
    , transform = [| id |]
    })

$(mkWordReader WordReaderSpec
    { TH.name = "paramStructType"
    , parentConName = 'S.Method
    , start = 64
    , rawTyp = ''Word64
    , typ = const [t| Word64 |]
    , defaultVal = 0
    , transform = [| id |]
    })

$(mkWordReader WordReaderSpec
    { TH.name = "resultStructType"
    , parentConName = 'S.Method
    , start = 128
    , rawTyp = ''Word64
    , typ = const [t| Word64 |]
    , defaultVal = 0
    , transform = [| id |]
    })

$(mkTextReader "name" 'S.Method 0)
