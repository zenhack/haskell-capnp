-- First stage IR. This models the data structures in schema.capnp more closely
-- than the other intermediate forms. Differences:
--
-- * Lots of information which we won't use is discarded.
-- * Nodes no longer reference eachother by ID; instead we include direct
--   references to the objects.
module IR.Stage1
    ( File(..)
    , Node(..)
    , Node'(..)
    ) where

import Data.Word

import qualified IR.Name as Name

data File = File
    { fileNodes :: [(Name.UnQ, Node)]
    , fileName  :: FilePath
    , fileId    :: !Word64
    }
    deriving(Show, Read, Eq)

data Node = Node
    { nodeNested :: [(Name.UnQ, Node)]
    , nodeUnion  :: Node'
    }
    deriving(Show, Read, Eq)

data Node'
    = Enum [Name.UnQ]
    | Other
    deriving(Show, Read, Eq)
