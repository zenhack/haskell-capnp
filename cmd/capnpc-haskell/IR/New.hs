module IR.New
    ( File(..)
    , Decl(..)
    ) where

import qualified Capnp.Repr as R
import           Data.Word
import qualified IR.Name    as Name

data File
    = File
        { fileId :: !Word64
        , decls  :: [Decl]
        }

data Decl
    = TypeDecl
        { name   :: Name.LocalQ
        , nodeId :: !Word64
        , params :: [Name.UnQ]
        , repr   :: R.Repr
        }
