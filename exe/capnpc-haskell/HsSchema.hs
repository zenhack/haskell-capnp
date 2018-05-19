{-# LANGUAGE NamedFieldPuns #-}
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
module HsSchema where

import Data.Word

import Data.Text (Text)

newtype Name = Name [Text]
    deriving(Show, Read, Eq)

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
