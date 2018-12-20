module IR.Flat where

import qualified IR.Name as Name

data File = File
    { nodes  :: [(Name.LocalQ, Node)]
    }
    deriving(Show, Read, Eq, Ord)

data Node
    = Enum [Name.UnQ]
    deriving(Show, Read, Eq, Ord)
