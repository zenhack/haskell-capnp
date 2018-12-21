-- IR for a high-level representation of the low-level API modules.
--
-- This talks about things like getters, setters, wrapper types for structs,
-- etc. It's still not at the level of detail of actual Haskell, but encodes
-- the constructs to be generated, as opposed to the declarative description
-- of the schema.
module IR.Raw (File(..), Decl(..)) where

import Data.Word

import qualified IR.Common as Common
import qualified IR.Name   as Name

data File = File
    { fileId   :: !Word64
    , fileName :: FilePath
    , decls    :: [Decl]
    }
    deriving(Show, Read, Eq)

data Decl
    = StructWrapper
        { ctorName :: Name.LocalQ
        }
    | Enum
        { typeCtor  :: Name.LocalQ
        , dataCtors :: [Name.LocalQ]
        }
    | Getter
        { fieldName     :: Name.LocalQ
        , containerType :: Name.LocalQ
        , fieldLocType  :: Common.FieldLocType Name.GlobalQ
        }
    deriving(Show, Read, Eq)
