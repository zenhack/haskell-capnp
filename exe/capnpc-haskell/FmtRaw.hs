-- Generate low-level accessors from type types in HsSchema.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FmtRaw
    ( fmtModule
    ) where

import HsSchema

import Data.Capnp.Core.Schema (Id)
import Data.Monoid            ((<>))
import Text.Printf            (printf)
import Util                   (mintercalate)

import qualified Data.Text.Lazy.Builder as TB

fmtModule :: Module -> TB.Builder
fmtModule Module{..} = mintercalate "\n"
    [ "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    , "module " <> fmtModRef (ByCapnpId modId) <> " where"
    , ""
    , "-- generated from " <> TB.fromText modFile
    , ""
    , "import qualified Data.Capnp.Untyped"
    , ""
    , mintercalate "\n" $ map fmtImport modImports
    , ""
    , mintercalate "\n" $ map (fmtDataDef modId) modDefs
    ]

fmtModRef :: ModuleRef -> TB.Builder
fmtModRef (ByCapnpId id) = TB.fromString $ printf "Data.Capnp.ById.X%x" id
fmtModRef (FullyQualified (Namespace ns)) = mintercalate "." (map TB.fromText ns)

fmtImport :: Import -> TB.Builder
fmtImport (Import ref) = "import qualified " <> fmtModRef ref

fmtDataDef :: Id -> DataDef -> TB.Builder
fmtDataDef thisMod DataDef{dataVariants=[variant], dataCerialType=CTyStruct, ..} =
    let name = fmtName thisMod dataName
    in mconcat
        [ "newtype ", name, " b = ", name, " (Data.Capnp.Untyped.Struct b)"
        ]
fmtDataDef _ _ = ""

fmtName :: Id -> Name -> TB.Builder
-- TODO: actually do something with the Id parameter. This is sufficient for
-- what we support so far, but not to reference things from other modules.
fmtName _ Name{nameLocalNS=Namespace parts, nameUnqualified=localName} =
    mintercalate "'" $ map TB.fromText $ parts ++ [localName]
