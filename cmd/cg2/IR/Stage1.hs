module IR.Stage1 where

import qualified IR.Name as Name

data Node = Node
    { nodeNested :: [(Name.UnQ, Node)]
    , union_     :: Node'
    }
    deriving(Show, Read, Eq, Ord)

data Node'
    = Enum [Name.UnQ]
    deriving(Show, Read, Eq, Ord)
