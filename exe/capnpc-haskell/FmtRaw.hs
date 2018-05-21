-- Generate low-level accessors from type types in HsSchema.
{-# LANGUAGE NamedFieldPuns    #-}
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
    , "import Data.Int"
    , "import Data.Word"
    , ""
    , "import qualified Data.Capnp.BuiltinTypes"
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
fmtDataDef thisMod DataDef{dataCerialType=CTyStruct,..} = mconcat
    [ "data ", fmtName thisMod dataName, " b"
    , "\n    = "
    , mintercalate "\n    | " (map fmtDataVariant dataVariants)
    ]
  where
    fmtDataVariant Variant{..} = fmtName thisMod variantName <>
        case variantParams of
            Record _   -> " (Data.Capnp.Untyped.Struct b)"
            NoParams   -> ""
            Unnamed ty -> " " <> fmtType ty

    fmtType :: Type -> TB.Builder
    fmtType (List eltType) =
        "(Data.Capnp.Untyped.ListOf b " <> fmtType eltType <> ")"
    fmtType (Type name []) = fmtName thisMod name
    fmtType (Type name params) = mconcat
        [ "("
        , fmtName thisMod name
        , " "
        , mintercalate " " (map fmtType params)
        , ")"
        ]
    fmtType (PrimType prim) = fmtPrimType prim

fmtDataDef _ _ = ""


fmtPrimType :: PrimType -> TB.Builder
-- TODO: most of this (except Text & Data) should probably be shared with FmtPure.
fmtPrimType PrimInt{isSigned=True,size}  = "Int" <> TB.fromString (show size)
fmtPrimType PrimInt{isSigned=False,size} = "Word" <> TB.fromString (show size)
fmtPrimType PrimFloat32                  = "Float32"
fmtPrimType PrimFloat64                  = "Float64"
fmtPrimType PrimBool                     = "Bool"
fmtPrimType PrimVoid                     = "()"
fmtPrimType PrimText                     = "(Data.Capnp.BuiltinTypes.Text b)"
fmtPrimType PrimData                     = "(Data.Capnp.BuiltinTypes.Data b)"

fmtName :: Id -> Name -> TB.Builder
fmtName thisMod Name{nameModule, nameLocalNS=Namespace parts, nameUnqualified=localName} =
    modPrefix <> mintercalate "'" (map TB.fromText $ parts <> [localName])
  where
    modPrefix = case nameModule of
        ByCapnpId id                  | id == thisMod -> ""
        FullyQualified (Namespace []) -> ""
        _                             -> fmtModRef nameModule <> "."
