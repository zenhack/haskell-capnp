{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-- This module defines datatypes that represent something between the capnp
-- schema and Haskell code. The representation has the following
-- chracteristics:
--
-- * Type definitions can map directly to idiomatic haskell types. Both the capnp
--   schema format itself and the types defined for it in schema.capnp have some
--   quirks that need to be ironed out to perform the mapping:
--   * Unions and structs are not separate types; a union is just a (possibly
--     anonymous field within a struct.
--   * Groups are only kindof their own type.
--   * Schema can have nested, mutually recursive namespaces even within a
--     single file. We want to generate a module with a flat namespace.
--   * Even features that conceptually map simply have somewhat odd
--     representations and irregularities schema.capnp
--   Our intermediate representation just has data declarations, with
--   enough information attached to access each variant and argument.
-- * Names are fully-qualified; see the 'Name' type for more information
--   (TODO).
module HsSchema
    ( Name(..)
    , Namespace(..)
    , Module(..)
    , ModuleRef(..)
    , Import(..)
    , Type(..)
    , Variant(..)
    , VariantParams(..)
    , Field(..)
    , DataDef(..)
    , CerialType(..)
    , FieldLoc(..)
    , DataLoc(..)
    , subName
    ) where

import Data.Word

import Data.Capnp.Core.Schema (Id)
import Data.String            (IsString(fromString))
import Data.Text              (Text)
import GHC.Exts               (IsList(..))

import qualified Data.Text as T

newtype Namespace = Namespace [Text]
    deriving(Show, Read, Eq)

instance IsList Namespace where
    type Item Namespace = Text
    fromList = Namespace
    toList (Namespace parts) = parts

data Module = Module
    { modId      :: Id
    , modFile    :: Text
    , modImports :: [Import]
    , modDefs    :: [DataDef]
    }
    deriving(Show, Read, Eq)

newtype Import = Import ModuleRef
    deriving(Show, Read, Eq)

data ModuleRef
    = FullyQualified Namespace
    | ByCapnpId Id
    deriving(Show, Read, Eq)

data Name = Name
    { nameModule      :: ModuleRef
    , nameLocalNS     :: Namespace
    , nameUnqualified :: Text
    }
    deriving(Show, Read, Eq)

subName :: Name -> Text -> Name
subName name@Name{..} nextPart = name
    { nameLocalNS = fromList $ toList nameLocalNS ++ [nameUnqualified]
    , nameUnqualified = nextPart
    }

instance IsString Name where
    fromString str = Name
        { nameModule = HsSchema.FullyQualified []
        , nameLocalNS = []
        , nameUnqualified = T.pack str
        }

data Type
    = Type Name [Type]
    | Unit
    deriving(Show, Read, Eq)

data Variant = Variant
    { variantName   :: Name
    , variantParams :: VariantParams
    , variantTag    :: Maybe Word16
    }
    deriving(Show, Read, Eq)

data VariantParams
    = Unnamed Type
    | Record [Field]
    | NoParams
    deriving(Show, Read, Eq)

data Field = Field
    { fieldName :: Text
    , fieldType :: Type
    , fieldLoc  :: FieldLoc
    }
    deriving(Show, Read, Eq)

data DataDef = DataDef
    { dataName       :: Name
    , dataVariants   :: [Variant]
    -- | The location of the tag for the union, if any.
    , dataTagLoc     :: Maybe DataLoc
    , dataCerialType :: CerialType
    }
    deriving(Show, Read, Eq)

-- | What kind of untyped wire format a type is stored as.
data CerialType
    -- | Stored as a struct
    = CTyStruct
    -- | Stored in the data section (i.e. an integer-like type). The argument
    -- is the size of the data type, in bits.
    | CTyWord !Int
    deriving(Show, Read, Eq)

-- | The location of a field within a struct
data FieldLoc
    -- | The field is in the struct's data section.
    = DataField DataLoc
    -- | The field is in the struct's pointer section (the argument is the
    -- index).
    | PtrField !Word16
    -- | The field is a group or union; it's "location" is the whole struct.
    | HereField
    -- | The field is zero-size (and has no argument)
    | VoidField
    deriving(Show, Read, Eq)

-- | The location of a field within a struct's data section.
data DataLoc = DataLoc
    { dataIdx :: !Int
    -- ^ The index of the 64-bit word containing the field.
    , dataOff :: !Int
    }
    deriving(Show, Read, Eq)
