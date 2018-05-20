{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- Generate idiomatic haskell data types from the types in HsSchema.
module FmtPure
    ( HsFmt(..)
    -- TODO: move mintercalate somewhere else (or find it in some library).
    , mintercalate
    , moduleFromId
    ) where

import HsSchema

import Data.Capnp.Core.Schema (Id)
import Data.List              (intersperse)
import Data.Monoid            (Monoid, mconcat, (<>))
import GHC.Exts               (IsList(..))
import Text.Printf            (printf)

import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as TB

-- | Generalization of 'Data.List.intercalate', analogous to concat/mconcat
mintercalate :: Monoid w => w -> [w] -> w
mintercalate sep = mconcat . intersperse sep

class HsFmt a where
    -- | Format the value as haskell source code. The Id parameter indicates
    -- which module the value is being referenced from (really only of interest
    -- to 'Name'; everything else just passes it through).
    hsFmt :: Id -> a -> TB.Builder

instance HsFmt Name where
    hsFmt thisMod Name{..} = modPrefix <> localName
      where
        localName = mintercalate "'" $
            map TB.fromText $ fromList $ toList nameLocalNS ++ [nameUnqualified]
        modPrefix
            | null (toList nameModule) || moduleFromId thisMod == nameModule = ""
            | otherwise = mintercalate "." (map TB.fromText $ toList nameModule) <> "."

moduleFromId :: Id -> HsSchema.Namespace
moduleFromId id = HsSchema.Namespace
    ["Data", "Capnp", "ById", T.pack (printf "X%x" id), "Pure"]

instance HsFmt Type where
    hsFmt thisMod (Type name params) =
        hsFmt thisMod name
        <> mconcat [" (" <> hsFmt thisMod ty <> ")" | ty <- params]
    hsFmt _ Unit = "()"

instance HsFmt Variant where
    hsFmt thisMod Variant{variantName,variantParams} =
        hsFmt thisMod variantName
        <> case variantParams of
            NoParams -> ""
            Unnamed ty -> " (" <> hsFmt thisMod ty <> ")"
            Record [] -> ""
            Record fields -> mconcat
                [ "\n        { "
                , mintercalate "\n        , " $ map (hsFmt thisMod) fields
                ,  "\n        }"
                ]

instance HsFmt Field where
    hsFmt thisMod Field{fieldName,fieldType} =
        TB.fromText fieldName <> " :: " <> hsFmt thisMod fieldType

instance HsFmt DataDef where
    hsFmt thisMod DataDef{dataName,dataVariants} = mconcat
        [ "data ", hsFmt thisMod dataName, "\n    = "
        , mintercalate "\n    | " $ map (hsFmt thisMod) dataVariants
        , "\n    deriving(Show, Read, Eq)\n\n"
        ]
