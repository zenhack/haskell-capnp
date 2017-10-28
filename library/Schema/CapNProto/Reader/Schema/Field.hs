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

$(mkWordReader WordReaderSpec
    { name = "codeOrder"
    , parentConName = 'S.Field
    , start = 0
    , rawTyp = ''Word16
    , typ = const [t| Word16 |]
    , defaultVal = 0
    , transform = [| id |]
    })

$(mkWordReader WordReaderSpec
    { name = "discriminantValue"
    , parentConName = 'S.Field
    , start = 0
    , rawTyp = ''Word16
    , typ = const [t| Word16 |]
    , defaultVal = 0xffff
    , transform = [| id |]
    })

$(mkTextReader "name" 'S.Field 0)
