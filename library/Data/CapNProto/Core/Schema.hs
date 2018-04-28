{-# LANGUAGE DuplicateRecordFields #-}
module Data.CapNProto.Core.Schema where

import Prelude hiding (id)

import Data.CapNProto.Untyped.ADT
import Data.Int
import Data.Word

type Id = Word64

data Node = Node
    { id                      :: Id
    , displayName             :: Maybe Text
    , displayNamePrefixLength :: Word32
    , scopeId                 :: Id
    , parameters              :: Maybe (List Parameter)
    , isGeneric               :: Bool
    , nestedNodes             :: Maybe (List Node'NestedNode)
    , union'                  :: Node'Union'
    }

data Node'NestedNode = Node'NestedNode
    { name :: Maybe Text
    , id   :: Id
    }

data Node'Union'
    = Node'File
    | Node'Struct Node'Struct'
    | Node'Enum Node'Enum'
    | Node'Interface Node'Interface'
    | Node'Const Node'Const'
    | Node'Annotation Node'Annotation'
    | Node'Unknown' Word16


field'noDiscriminant :: Word16
field'noDiscriminant = 0xffff

data Field = Field
    { name              :: Maybe Text
    , codeOrder         :: Word16
    , annotations       :: Maybe (List Annotation)
    , discriminantValue :: Word16
    , union'            :: Field'Union'
    , ordinal           :: Field'Ordinal
    }

data Field'Union'
    = Field'Slot Field'Slot'
    | Field'Group Field'Group'

data Field'Slot' = Field'Slot'
    { offset            :: Word32
    , type'             :: Type
    , defaultValue      :: Value
    , hadExplcitDefault :: Bool
    }

data Field'Group' = Field'Group'
    { typeId :: Id
    }

data Field'Ordinal
    = Field'Oridinal'Implicit
    | Field'Oridinal'Excplicit Word16
    | Field'Oridinal'Unknown' Word16

data Value = Value
    { union' :: Value'Union'
    }

data Value'Union'
    = Value'Void
    | Value'Bool Bool
    | Value'Int8 Int8
    | Value'Int16 Int16
    | Value'Int32 Int32
    | Value'Int64 Int64
    | Value'Uint8 Word8
    | Value'Uint16 Word16
    | Value'Uint32 Word32
    | Value'Uint64 Word64
    | Value'Float32 Float
    | Value'Float64 Double
    | Value'Text Text
    | Value'Data Data
    | Value'List (Maybe PtrType)
    | Value'Enum Word16
    | Value'Struct (Maybe PtrType)
    | Value'Interface
    | Value'AnyPointer (Maybe PtrType)

-- Still need to implement these, but put them here so the other stuff at least
-- builds.
data Type
data Annotation
data Parameter
data Node'Struct'
data Node'Interface'
data Node'Const'
data Node'Enum'
data Node'Annotation'
