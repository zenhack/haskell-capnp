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
