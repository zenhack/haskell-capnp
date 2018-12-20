module IR.Stage1 where

import qualified IR.Name as Name

data File = File
    { fileNodes :: [(Name.UnQ, Node)]
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
