{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
-- This module defines datatypes that represent something between the capnp
-- schema and Haskell code. The representation has the following
-- chracteristics:
--
-- * Type definitions can map directly to idiomatic haskell types. Both the capnp
--   schema format itself and the types defined for it in schema.capnp have some
--   quirks that need to be ironed out to perform the mapping:
--   * Unions and structs are not separate types; a union is just a (possibly
--     anonymous) field within a struct.
--   * Groups are only kindof their own type.
--   * Schema can have nested, mutually recursive namespaces even within a
--     single file. We want to generate a module with a flat namespace.
--   * Even features that conceptually map simply have somewhat odd
--     representations and irregularities schema.capnp
--   Our intermediate representation just has data declarations, with
--   enough information attached to access each variant and argument.
-- * Names are fully-qualified; see the 'Name' type for more information
--   (TODO).
module IR
    ( Name(..)
    , Namespace(..)
    , Method(..)
    , Module(..)
    , ModuleRef(..)
    , Import(..)
    , Type(..)
    , CompositeType(..)
    , WordType(..)
    , PtrType(..)
    , PrimWord(..)
    , PrimPtr(..)
    , AnyPtr(..)
    , Variant(..)
    , VariantParams(..)
    , Field(..)
    , Decl(..)
    , DataDef(..)
    , StructDef(..)
    , InterfaceDef(..)
    , Const(..)
    , FieldLocType(..)
    , DataLoc(..)
    , StructInfo(..)
    , subName
    , prefixName
    , valueName
    ) where

import Data.Word

import Data.Char   (toLower)
import Data.String (IsString(fromString))
import Data.Text   (Text)

import GHC.Exts (IsList(..))

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

import Util

import qualified Capnp.Untyped.Pure as Untyped

newtype Namespace = Namespace [Text]
    deriving(Show, Eq, Ord, IsList)

data Module = Module
    { modId      :: Id
    , modName    :: Namespace
    , modFile    :: Text
    , modImports :: [Import]
    -- XXX TODO: 'Name' includes a 'ModuleRef', which means it is possible
    -- for two different 'Name's to refer to the same actual variable; using
    -- them as map keys is questionable. In practice, when building a module
    -- all of the ModuleRefs are the same, though, so this won't *break*
    -- anything -- but ideally we'd remove that information from the key.
    , modDecls   :: M.Map Name Decl
    }
    deriving(Show, Eq)

data Decl
    = DeclDef DataDef
    | DeclConst Const
    deriving(Show, Eq)

newtype Import = Import ModuleRef
    deriving(Show, Eq)

data ModuleRef
    = FullyQualified Namespace
    | ByCapnpId Id
    deriving(Show, Eq, Ord)

data Name = Name
    { nameModule      :: ModuleRef
    , nameLocalNS     :: Namespace
    , nameUnqualified :: Text
    }
    deriving(Show, Eq, Ord)

subName :: Name -> Text -> Name
subName name@Name{..} nextPart = name
    { nameLocalNS = fromList $ toList nameLocalNS ++ [nameUnqualified]
    , nameUnqualified = nextPart
    }

prefixName :: Text -> Name -> Name
prefixName prefix name@Name{nameLocalNS=(toList -> (x:xs))} =
    name { nameLocalNS = fromList $ (prefix <> x):xs }
prefixName prefix name = name { nameLocalNS = fromList [prefix] }

-- | 'valueName' converts a name to one which starts with a lowercase
-- letter, so that it is valid to use as a name for a value (as opposed
-- to a type).
valueName :: Name -> Name
valueName name@Name{nameLocalNS=(toList -> (T.unpack -> (c:cs)):xs)} =
    name { nameLocalNS = fromList $ T.pack (toLower c : cs) : xs }
valueName name = name

instance IsString Name where
    fromString str = Name
        { nameModule = FullyQualified []
        , nameLocalNS = []
        , nameUnqualified = T.pack str
        }

data Type
    = CompositeType CompositeType
    | VoidType
    | WordType WordType
    | PtrType PtrType
    deriving(Show, Eq)

data CompositeType
    = StructType Name [Type]
    deriving(Show, Eq)

data WordType
    = EnumType Name
    | PrimWord PrimWord
    deriving(Show, Eq)

data PtrType
    = ListOf Type
    | PrimPtr PrimPtr
    | PtrComposite CompositeType
    | PtrInterface Name
    deriving(Show, Eq)

data PrimWord
    = PrimInt { isSigned :: !Bool, size :: !Int }
    | PrimFloat32
    | PrimFloat64
    | PrimBool
    deriving(Show, Eq)

data PrimPtr
    = PrimText
    | PrimData
    | PrimAnyPtr AnyPtr
    deriving(Show, Eq)

data AnyPtr
    = Struct
    | List
    | Cap
    | Ptr
    deriving(Show, Eq)

data Variant = Variant
    { variantName   :: Name
    , variantParams :: VariantParams
    , variantTag    :: !Word16
    }
    deriving(Show, Eq)

data VariantParams
    = Unnamed Type FieldLocType
    | Record [Field]
    deriving(Show, Eq)

data Field = Field
    { fieldName    :: Text
    , fieldLocType :: FieldLocType
    }
    deriving(Show, Eq)

data Const
    = WordConst
        { wordValue :: Word64
        , wordType  :: WordType
        }
    | VoidConst
    | PtrConst
        { ptrValue :: Maybe Untyped.PtrType
        , ptrType  :: PtrType
        }
    deriving(Show, Eq)

data DataDef
    = DefUnion
        { dataVariants     :: [Variant]
        -- | The location of the tag for the union.
        , dataTagLoc       :: DataLoc
        -- | The name of the containing struct.
        , parentStructName :: Name
        , parentStruct     :: StructDef
        }
    | DefStruct StructDef
    | DefEnum [Name]
    | DefInterface InterfaceDef
    deriving(Show, Eq)

data StructDef = StructDef
    { fields :: [Field]
    , info   :: StructInfo
    }
    deriving(Show, Eq)

data StructInfo
    = IsGroup
    -- ^ The struct is really a group.
    | IsStandalone
        { dataSz :: !Word16
        , ptrSz  :: !Word16
        }
    -- ^ The struct is a full-fledged struct that can be directly
    -- allocated, stored in a list, etc. Info contains the sizes
    -- of its sections.
    deriving(Show, Eq)

data InterfaceDef = InterfaceDef
    { interfaceId :: !Word64
    , methods     :: [Method]
    }
    deriving(Show, Eq)

data Method = Method
    { methodName :: Name
    , ordinal    :: !Word16
    , paramType  :: CompositeType
    , resultType :: CompositeType
    }
    deriving(Show, Eq)

-- | The type and location of a field.
data FieldLocType
    -- | The field is in the struct's data section.
    = DataField DataLoc WordType
    -- | The field is in the struct's pointer section (the argument is the
    -- index).
    | PtrField !Word16 PtrType
    -- | The field is a group or union; it's "location" is the whole struct.
    | HereField CompositeType
    -- | The field is of type void (and thus is zero-size).
    | VoidField
    deriving(Show, Eq)

-- | The location of a field within a struct's data section.
data DataLoc = DataLoc
    { dataIdx :: !Int
    -- ^ The index of the 64-bit word containing the field.
    , dataOff :: !Int
    , dataDef :: !Word64
    -- ^ The value is stored xor-ed with this value. This is used
    -- to allow for encoding default values. Note that this is xor-ed
    -- with the bits representing the value, not the whole word.
    }
    deriving(Show, Eq)
