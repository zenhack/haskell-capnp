{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Enumerant
    (module Schema.CapNProto.Reader.Schema.Enumerant)
  where

import Data.Word
import Language.CapNProto.TH

import qualified Data.CapNProto.Untyped         as U
import qualified Schema.CapNProto.Reader.Schema as S

$(mkListReaders 'S.Enumerant
    [ ("annotations", 1, 'U.ListStruct, ''S.Annotation, [| S.Annotation |])
    ])

$(mkWordReader WordReaderSpec
    { name = "codeOrder"
    , parentConName = 'S.Enumerant
    , start = 0
    , rawTyp = ''Word16
    , typ = const [t| Word16 |]
    , defaultVal = 0
    , transform = [| id |]
    })

$(mkTextReader "name" 'S.Enumerant 0)
