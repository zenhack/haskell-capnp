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
    -- | Define a newtype wrapper around a struct. This also defines
    -- some instances of type classes that exist for all such wrappers.
    = StructWrapper
        { ctorName :: Name.LocalQ
        }
    -- | Define instances of several type classes which should only
    -- exist for "real" structs, i.e. not groups.
    | StructInstances
        { ctorName :: Name.LocalQ
        }
    | InterfaceWrapper
        { ctorName :: Name.LocalQ
        }
    | Enum
        { typeCtor  :: Name.LocalQ
        , dataCtors :: [Name.LocalQ]
        }
    | Getter
        { fieldName     :: Name.LocalQ
        , containerType :: Name.LocalQ
        , fieldLocType  :: Common.FieldLocType Name.CapnpQ
        }
    deriving(Show, Read, Eq)
