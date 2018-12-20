module IR.Flat
    ( File(..)
    , Node(..)
    ) where

import Data.Word

import qualified IR.Name as Name

data File = File
    { nodes    :: [(Name.LocalQ, Node)]
    , fileId   :: !Word64
    , fileName :: FilePath
    }
    deriving(Show, Read, Eq, Ord)

data Node
    = Enum [Name.UnQ]
    deriving(Show, Read, Eq, Ord)
