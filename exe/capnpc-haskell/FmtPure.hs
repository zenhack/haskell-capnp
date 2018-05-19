{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
-- Generate idiomatic haskell data types from the types in HsSchema.
module FmtPure
    ( HsFmt(..)
    -- TODO: move mintercalate somewhere else (or find it in some library).
    , mintercalate
    ) where

import HsSchema

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
