{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Node.Union_
    (module Schema.CapNProto.Reader.Schema.Node.Union_)
  where

import Data.CapNProto.Bits   (Word1, word1ToBool)
import Data.Word
import Language.CapNProto.TH

import qualified Data.CapNProto.Untyped         as U
import qualified Schema.CapNProto.Reader.Schema as S

$(mkStructWrappers
    [ "Struct"
    , "Enum"
    , "Interface"
    , "Const"
    , "Annotation"
    ])

$(mkWordReader WordReaderSpec
    { name = "dataWordCount"
    , parentConName = 'Struct
    , start = 112
    , rawTyp = ''Word16
    , typ = const [t| Word16 |]
    , defaultVal = 0
    , transform = [| id |]
    })

$(mkWordReader WordReaderSpec
    { name = "pointerCount"
    , parentConName = 'Struct
    , start = 192
    , rawTyp = ''Word16
    , typ = const [t| Word16 |]
    , defaultVal = 0
    , transform = [| id |]
    })

$(mkWordReader WordReaderSpec
    { name = "preferredListEncoding"
    , parentConName = 'Struct
    , start = 208
    , rawTyp = ''Word16
    , typ = const [t| Word16 |]
    , defaultVal = 0
    , transform = [| id |]
    })

$(mkWordReader WordReaderSpec
    { name = "discriminantCount"
    , parentConName = 'Struct
    , start = 240
    , rawTyp = ''Word16
    , typ = const [t| Word16 |]
    , defaultVal = 0
    , transform = [| id |]
    })

$(mkWordReader WordReaderSpec
    { name = "discriminantOffset"
    , parentConName = 'Struct
    , start = 256
    , rawTyp = ''Word16
    , typ = const [t| Word16 |]
    , defaultVal = 0
    , transform = [| id |]
    })

$(mkBoolReader "isGroup" 'S.Node 224 False)

$(mkListReaders 'Struct
    [ ("fields", 3, 'U.ListStruct, ''S.Field, [| S.Field |])
    ])
