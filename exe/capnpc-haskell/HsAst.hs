{-# LANGUAGE OverloadedStrings #-}
module HsAst where

import Data.List   (intersperse)
import Data.Monoid (Monoid, mconcat, (<>))
import Data.String (IsString(..))
import Data.Text   (Text)

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

data Variant
    = Record
        { recordName   :: Name
        , recordFields :: [Field]
        }
    | NormalVariant
        { variantName :: Name
        , variantType :: Maybe Type
        }
    deriving(Show, Read, Eq)

data Field = Field
    { fieldName :: Text
    , fieldType :: Type
    }
    deriving(Show, Read, Eq)

data DataDef = DataDef
    { dataName     :: Name
    , dataVariants :: [Variant]
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
    hsFmt (NormalVariant name Nothing) = hsFmt name
    hsFmt (NormalVariant name (Just ty)) = hsFmt name <> " (" <> hsFmt ty <> ")"
    hsFmt (Record name []) = hsFmt name
    hsFmt (Record name fields) = mconcat
        [ hsFmt name
        , "\n        { "
        , mintercalate "\n        , " $ map hsFmt fields
        ,  "\n        }"
        ]

instance HsFmt Field where
    hsFmt (Field name ty) = hsFmt name <> " :: " <> hsFmt ty

instance HsFmt DataDef where
    hsFmt (DataDef name variants) = mconcat
        [ "data ", hsFmt name, "\n    = "
        , mintercalate "\n    | " (map hsFmt variants)
        , "\n    deriving(Show, Read, Eq)\n\n"
        ]
