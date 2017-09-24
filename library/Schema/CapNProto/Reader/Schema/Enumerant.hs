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

$(mkWordReaders 'S.Enumerant
    [ ("codeOrder", 0, ''Word16, const [t| Word16 |], 0, [| id |])
    ])

$(mkTextReader "name" 'S.Enumerant 0)
