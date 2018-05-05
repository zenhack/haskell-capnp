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

data Variant = Variant
    { variantName   :: Name
    , variantFields :: [Field]
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
    hsFmt (Variant name []) = hsFmt name
    hsFmt (Variant name fields) = concat
        [ hsFmt name, "{ ", intercalate ", " $ map hsFmt fields,  "}" ]

instance HsFmt Field where
    hsFmt (Field name ty) = name ++ " :: " ++ hsFmt ty

instance HsFmt DataDef where
    hsFmt (DataDef name variants) = concat
        [ "data ", hsFmt name, " = "
        , intercalate " | " (map hsFmt variants)
        , " deriving(Show, Read, Eq)"
        ]
