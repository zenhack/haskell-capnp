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
{-# LANGUAGE NamedFieldPuns #-}
module IR.Flat
    ( File(..)
    , Node(..)
    , Node'(..)
    , Field(..)
    ) where

-- Note to self: evolve this to generally making the tree the "right shape",
-- e.g. also do product/sum separation here. Just the flattening is little
-- enough that we end up with a lot of redundant data structures.

import Data.Word

import qualified IR.Common as Common
import qualified IR.Name   as Name

data File r = File
    { nodes    :: [Node r]
    , fileId   :: !Word64
    , fileName :: FilePath
    }
    deriving(Show, Read, Eq)

data Node r = Node
    { name   :: Name.LocalQ
    , nodeId :: !Word64
    , union_ :: Node' r
    }
    deriving(Show, Read, Eq)

data Node' r
    = Enum [Name.UnQ]
    | Struct
        { fields :: [Field r]
        }
    deriving(Show, Read, Eq)

data Field r = Field
    { fieldName    :: Name.UnQ
    , fieldLocType :: Common.FieldLocType r
    }
    deriving(Show, Read, Eq)
