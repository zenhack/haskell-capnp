-- IR for a high-level representation of the low-level API modules.
--
-- This talks about things like getters, setters, wrapper types for structs,
-- etc. It's still not at the level of detail of actual Haskell, but encodes
-- the constructs to be generated, as opposed to the declarative description
-- of the schema.
{-# LANGUAGE DuplicateRecordFields #-}
module IR.Raw (File(..), Decl(..), Variant(..), TagSetter(..), NewFnType(..)) where

import Data.Word

import qualified IR.Common as Common
import qualified IR.Name   as Name

data File = File
    { fileId      :: !Word64
    , fileName    :: FilePath
    , fileImports :: [Word64]
    , decls       :: [Decl]
    }
    deriving(Show, Read, Eq)

data Decl
    -- | Define a newtype wrapper around a struct. This also defines
    -- some instances of type classes that exist for all such wrappers.
    = StructWrapper
        { typeCtor :: Name.LocalQ
        }
    -- | Define instances of several type classes which should only
    -- exist for "real" structs, i.e. not groups.
    | StructInstances
        { typeCtor      :: Name.LocalQ
        -- ^ The type constructor for the type to generate instances for.

        -- Needed for some instances:
        , dataWordCount :: !Word16
        , pointerCount  :: !Word16
        }
    | InterfaceWrapper
        { typeCtor :: Name.LocalQ
        }
    | UnionVariant
        { parentTypeCtor :: Name.LocalQ
        -- ^ The type constructor of the parent, i.e. the enclosing struct.
        -- we can derive the type constructor for the union proper from this,
        -- and it is useful to have for other things (like unknown' variants).
        , tagOffset      :: !Word32
        , unionDataCtors :: [Variant]
        }
    | Enum
        { typeCtor  :: Name.LocalQ
        , dataCtors :: [Name.LocalQ]
        }
    | Getter -- get_* function
        { fieldName     :: Name.LocalQ
        , containerType :: Name.LocalQ
        , fieldLocType  :: Common.FieldLocType Name.CapnpQ
        }
    | Setter -- set_* function
        { fieldName     :: Name.LocalQ
        , containerType :: Name.LocalQ
        , fieldLocType  :: Common.FieldLocType Name.CapnpQ

        , tag           :: Maybe TagSetter
        -- ^ Info for setting the tag, if this is a union.
        }
    | HasFn -- has_* function
        { fieldName     :: Name.LocalQ
        , containerType :: Name.LocalQ
        , ptrIndex      :: !Word16
        }
    | NewFn -- new_* function
        { fieldName     :: Name.LocalQ
        , containerType :: Name.LocalQ
        , fieldLocType  :: Common.FieldLocType Name.CapnpQ

        , newFnType     :: NewFnType
        }
    deriving(Show, Read, Eq)

data NewFnType
    = NewList
    | NewText
    | NewData
    | NewStruct
    deriving(Show, Read, Eq)

data TagSetter = TagSetter
    { tagOffset :: !Word32
    , tagValue  :: !Word16
    }
    deriving(Show, Read, Eq)

data Variant = Variant
    { name     :: Name.LocalQ
    , tagValue :: !Word16
    , locType  :: Common.FieldLocType Name.CapnpQ
    }
    deriving(Show, Read, Eq)
