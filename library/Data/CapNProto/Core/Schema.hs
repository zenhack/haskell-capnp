{-# LANGUAGE DuplicateRecordFields #-}
module Data.CapNProto.Core.Schema where

import Prelude hiding (id)

import Data.CapNProto.Untyped.ADT
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

-- Still need to implement these, but put them here so the other stuff at least
-- builds.
data Type
data Value
data Annotation
data Parameter
data Node'Struct'
data Node'Interface'
data Node'Const'
data Node'Enum'
data Node'Annotation'
