{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Method
    (module Schema.CapNProto.Reader.Schema.Method)
  where

import Data.Word
import Language.CapNProto.TH

import qualified Data.CapNProto.Untyped              as U
import qualified Schema.CapNProto.Reader.Schema      as S
import qualified Schema.CapNProto.Reader.Schema.Node as Node

noDiscriminant :: Word16
noDiscriminant = 0xffff

$(mkListReaders 'S.Method
    [ ("annotations",        1, 'U.ListStruct, ''S.Annotation,   [| S.Annotation   |])
    , ("implicitParameters", 7, 'U.ListStruct, ''Node.Parameter, [| Node.Parameter |])
    ])

$(mkWordReaders 'S.Method
    [ ("codeOrder",          0, ''Word16, const [t| Word16 |], 0, [| id |])
    , ("paramStructType",   64, ''Word64, const [t| Word64 |], 0, [| id |])
    , ("resultStructType", 128, ''Word64, const [t| Word64 |], 0, [| id |])
    ])

$(mkTextReader "name" 'S.Method 0)
