{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Node
    (module Schema.CapNProto.Reader.Schema.Node)
  where

import Data.CapNProto.Bits   (Word1, word1ToBool)
import Data.Bits
import Data.Word
import Language.CapNProto.TH

import qualified Data.CapNProto.Untyped                     as U
import qualified Schema.CapNProto.Reader.Schema             as S
import qualified Schema.CapNProto.Reader.Schema.Node.Union_ as Union_

$(mkStructWrappers ["Parameter", "NestedNode"])

$(mkListReaders 'S.Node
    [ ("parameters",  5, 'U.ListStruct, ''Parameter,    [| Parameter    |])
    , ("nestedNodes", 1, 'U.ListStruct, ''NestedNode,   [| NestedNode   |])
    , ("annotations", 2, 'U.ListStruct, ''S.Annotation, [| S.Annotation |])
    ])

$(mkWordReaders 'S.Node
    [ ("id",                        0, ''Word64, const [t| Word64 |], 0, [| id          |])
    , ("displayNamePrefixLength",  64, ''Word32, const [t| Word32 |], 0, [| id          |])
    , ("scopeId",                 128, ''Word64, const [t| Word64 |], 0, [| id          |])
    ])

$(mkBoolReader "isGeneric" 'S.Node 288 False)

$(mkTextReader "displayName" 'S.Node 0)

union_ :: U.ReadCtx m b => S.Node b -> m (Union_ b)
union_ (S.Node struct) = do
    dataSec <- U.dataSection struct
    let dataIndex = 96 `div` 64
    let bitOffset = 96 `mod` 64
    word <- U.index dataIndex dataSec
    let tag = fromIntegral (word `shiftR` bitOffset) :: Word16
    return $ case tag of
        0 -> File
        1 -> Struct (Union_.Struct struct)
        2 -> Enum (Union_.Enum struct)
        3 -> Interface (Union_.Interface struct)
        4 -> Const (Union_.Const struct)
        5 -> Annotation (Union_.Annotation struct)
        _ -> Unknown tag


data Union_ b
    = File
    | Struct (Union_.Struct b)
    | Enum (Union_.Enum b)
    | Interface (Union_.Interface b)
    | Const (Union_.Const b)
    | Annotation (Union_.Annotation b)
    | Unknown Word16
