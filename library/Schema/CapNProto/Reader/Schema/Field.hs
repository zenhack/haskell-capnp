{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Field
    (module Schema.CapNProto.Reader.Schema.Field)
  where

import Data.Word
import Language.CapNProto.TH

import qualified Data.CapNProto.Untyped         as U
import qualified Schema.CapNProto.Reader.Schema as S

noDiscriminant :: Word16
noDiscriminant = 0xffff

$(mkListReaders 'S.Field
    [ ("annotations", 1, 'U.ListStruct, ''S.Annotation, [| S.Annotation |])
    ])

$(mkWordReaders 'S.Field
    [ ("codeOrder",          0, ''Word16, const [t| Word16 |], 0,      [| id |])
    , ("discriminantValue", 16, ''Word16, const [t| Word16 |], 0xffff, [| id |])
    ])

$(mkTextReader "name" 'S.Field 0)
