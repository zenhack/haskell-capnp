-- First stage IR. This models the data structures in schema.capnp more closely
-- than the other intermediate forms. Differences:
--
-- * Lots of information which we won't use is discarded.
-- * Nodes no longer reference eachother by ID; instead we include direct
--   references to the objects.
-- * The details of some structures are tweaked to make them more ergonomic
--   to use and/or more idiomatic Haskell.
{-# LANGUAGE DuplicateRecordFields #-}
module IR.Stage1
    ( File(..)
    , Method(..)
    , Node(..)
    , Node'(..)
    , Struct(..)
    , Field(..)
    ) where

import Data.Word

import qualified IR.Common as Common
import qualified IR.Name   as Name

data File = File
    { fileNodes   :: [(Name.UnQ, Node)]
    , fileName    :: FilePath
    , fileId      :: !Word64
    , fileImports :: [Word64]
    }
    deriving(Show, Eq)

data Node = Node
    { nodeNested :: [(Name.UnQ, Node)]
    , nodeUnion  :: Node'
    , nodeParent :: Maybe Node
    , nodeId     :: !Word64
    }
    deriving(Show, Eq)

data Node'
    = NodeEnum [Name.UnQ]
    | NodeStruct Struct
    | NodeInterface
        { methods :: [Method]
        }
    | NodeConstant (Common.Value Node)
    | NodeOther
    deriving(Show, Eq)

data Method = Method
    { name       :: Name.UnQ
    , paramType  :: Node
    , resultType :: Node
    }
    deriving(Show, Eq)

data Struct = Struct
    { dataWordCount :: !Word16
    , pointerCount  :: !Word16
    , isGroup       :: !Bool
    , tagOffset     :: !Word32
    , fields        :: [Field]
    }
    deriving(Show, Eq)

data Field = Field
    { name    :: Name.UnQ
    , tag     :: Maybe Word16
    , locType :: Common.FieldLocType Node
    }
    deriving(Show, Eq)
