module HsAst where

import Data.List   (intercalate)
import Data.String (IsString(..))

class HsFmt a where
    -- | Format the value as haskell source code.
    hsFmt :: a -> String

newtype Name = Name [String]
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
    { fieldName :: String
    , fieldType :: Type
    }
    deriving(Show, Read, Eq)

data DataDef = DataDef
    { dataName     :: Name
    , dataVariants :: [Variant]
    }
    deriving(Show, Read, Eq)

instance IsString Name where
    fromString str = Name [str]

instance HsFmt Name where
    hsFmt (Name parts) = intercalate "'" parts

instance HsFmt Type where
    hsFmt (Type name params) =
        hsFmt name ++ concat [" (" ++ hsFmt ty ++ ")" | ty <- params]
    hsFmt Unit = "()"

instance HsFmt Variant where
    hsFmt (NormalVariant name Nothing) = hsFmt name
    hsFmt (NormalVariant name (Just ty)) = hsFmt name ++ " (" ++ hsFmt ty ++ ")"
    hsFmt (Record name []) = hsFmt name
    hsFmt (Record name fields) = concat
        [ hsFmt name, "\n    { ", intercalate "\n    , " $ map hsFmt fields,  "\n    }" ]

instance HsFmt Field where
    hsFmt (Field name ty) = name ++ " :: " ++ hsFmt ty

instance HsFmt DataDef where
    hsFmt (DataDef name variants) = concat
        [ "data ", hsFmt name, " = "
        , intercalate " | " (map hsFmt variants)
        , " deriving(Show, Read, Eq)"
        ]
