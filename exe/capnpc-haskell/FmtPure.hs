{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- Generate idiomatic haskell data types from the types in HsSchema.
module FmtPure
    ( fmtModule
    ) where

import HsSchema

import Data.Capnp.Core.Schema (Id)
import Data.Monoid            (mconcat, (<>))
import GHC.Exts               (IsList(..))
import Text.Printf            (printf)
import Util                   (mintercalate)

import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as TB

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
            | null nsParts || modRefToNS (ByCapnpId thisMod) == ns = ""
            | otherwise = hsFmt thisMod nameModule <> "."
        ns@(Namespace nsParts) = modRefToNS nameModule

modRefToNS :: ModuleRef -> Namespace
modRefToNS (FullyQualified ns) = ns
modRefToNS (ByCapnpId id) = HsSchema.Namespace
    ["Data", "Capnp", "ById", T.pack (printf "X%x" id), "Pure"]

fmtModule :: Module -> TB.Builder
fmtModule Module{..} = mintercalate "\n"
    [ "{-# LANGUAGE DuplicateRecordFields #-}"
    , "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    , "module "
        <> hsFmt modId (ByCapnpId modId)
        <> " where"
    , ""
    , "-- generated from " <> TB.fromText modFile
    , ""
    , "import Data.Int"
    , "import Data.Word"
    , ""
    , "import Data.Capnp.Untyped.Pure (List)"
    , "import Data.Capnp.BuiltinTypes.Pure (Data, Text)"
    , ""
    , "import qualified Data.Capnp.Untyped.Pure"
    , "import qualified Codec.Capnp"
    , ""
    , mintercalate "\n" $ map (hsFmt modId) modImports
    , ""
    , mconcat $ map (hsFmt modId) modDefs
    ]

instance HsFmt Import where
    hsFmt _ (Import ref) =
        "import qualified " <>
            mintercalate "."
            (map TB.fromText $ toList $ modRefToNS ref)

instance HsFmt ModuleRef where
    hsFmt _ ref = mintercalate "." (map TB.fromText $ toList $ modRefToNS ref)

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
