-- An intermediate form with a flat namespace.
--
-- One issue that needs ironing out to get from capnproto to Haskell
-- is that capnproto schema files can contain deeply-nested, mutually
-- recursive namespaces, while a Haskell module is a single flat
-- namespace. Once we reach this intermediate form, we have bridged
-- that particular gap.
--
-- The names in this flat namespace do have some internal structure
-- to them; see 'IR.Name' for details.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module IR.Flat
    ( File(..)
    , Node(..)
    , Node'(..)
    , Field(..)
    , Variant(..)
    , Union(..)
    ) where

-- Note to self: evolve this to generally making the tree the "right shape",
-- e.g. also do product/sum separation here. Just the flattening is little
-- enough that we end up with a lot of redundant data structures.

import Data.Word

import qualified IR.Common as Common
import qualified IR.Name   as Name

data File = File
    { nodes       :: [Node]
    , fileId      :: !Word64
    , fileName    :: FilePath
    , fileImports :: [Word64]
    }
    deriving(Show, Eq)

data Node = Node
    { name   :: Name.CapnpQ
    , nodeId :: !Word64
    , union_ :: Node'
    }
    deriving(Show, Eq)

data Node'
    = Enum [Name.UnQ]
    | Struct
        { fields        :: [Field]
        -- ^ The struct's fields, excluding an anonymous union, if any.
        , isGroup       :: !Bool
        , dataWordCount :: !Word16
        , pointerCount  :: !Word16
        , union         :: Maybe Union
        -- ^ The struct's anonymous union, if any.
        }
    | Interface
    | Constant
        { value :: Common.Value Node
        }
    deriving(Show, Eq)


data Union = Union
    { tagOffset :: !Word32
    , variants  :: [Variant]
    }
    deriving(Show, Eq)

data Field = Field
    { fieldName    :: Name.CapnpQ
    , fieldLocType :: Common.FieldLocType Node
    }
    deriving(Show, Eq)

data Variant = Variant
    { tagValue :: !Word16
    , field    :: Field
    -- ^ The field's name is really the name of the variant.
    }
    deriving(Show, Eq)
