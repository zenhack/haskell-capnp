{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module HsAst where

import Data.List   (intersperse)
import Data.Monoid (Monoid, mconcat, (<>))
import Data.String (IsString(..))
import Data.Text   (Text)
import Data.Word

import qualified Data.Text.Lazy.Builder as TB

-- | Generalization of 'Data.List.intercalate', analogous to concat/mconcat
mintercalate :: Monoid w => w -> [w] -> w
mintercalate sep = mconcat . intersperse sep

class HsFmt a where
    -- | Format the value as haskell source code.
    hsFmt :: a -> TB.Builder

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

data HsExpr
    = ExCase HsExpr [(HsPat, HsExpr)]
    | ExApp HsExpr HsExpr
    | ExVar Text
    | ExInt !Int
    deriving(Show, Read, Eq)

data HsPat
    = PatInt Int
    | PatVar Text
    deriving(Show, Read, Eq)

data CerialType
    = CTyStruct
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

instance IsString Name where
    fromString str = Name [fromString str]

instance HsFmt Text where
    hsFmt = TB.fromText

instance HsFmt Name where
    hsFmt (Name parts) = mintercalate "'" (map hsFmt parts)

instance HsFmt Type where
    hsFmt (Type name params) =
        hsFmt name <> mconcat [" (" <> hsFmt ty <> ")" | ty <- params]
    hsFmt Unit = "()"

instance HsFmt Variant where
    hsFmt Variant{variantName,variantParams} = hsFmt variantName <>
        case variantParams of
            NoParams -> ""
            Unnamed ty -> " (" <> hsFmt ty <> ")"
            Record [] -> ""
            Record fields -> mconcat
                [ "\n        { "
                , mintercalate "\n        , " $ map hsFmt fields
                ,  "\n        }"
                ]

instance HsFmt Field where
    hsFmt Field{fieldName,fieldType} = hsFmt fieldName <> " :: " <> hsFmt fieldType

instance HsFmt DataDef where
    hsFmt DataDef{dataName,dataVariants} = mconcat
        [ "data ", hsFmt dataName, "\n    = "
        , mintercalate "\n    | " (map hsFmt dataVariants)
        , "\n    deriving(Show, Read, Eq)\n\n"
        ]
