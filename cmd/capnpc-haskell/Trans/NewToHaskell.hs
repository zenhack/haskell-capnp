{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.NewToHaskell
    ( fileToModules
    ) where

import qualified Capnp.Repr            as R
import           Data.String           (IsString(fromString))
import           Data.Word
import qualified IR.Common             as C
import qualified IR.Haskell            as Hs
import qualified IR.Name               as Name
import qualified IR.New                as New
import           Trans.ToHaskellCommon

imports :: [Hs.Import]
imports =
    [ Hs.ImportAs { importAs = "R", parts = ["Capnp", "Repr"] }
    , Hs.ImportAs { importAs = "F", parts = ["Capnp", "Fields"] }
    , Hs.ImportAs { importAs = "Basics", parts = ["Capnp", "New", "Basics"] }
    , Hs.ImportAs { importAs = "OL", parts = ["GHC", "OverloadedLabels"] }
    , Hs.ImportAs { importAs = "GH", parts = ["Capnp", "GenHelpers", "New"] }
    ]

fileToModules :: New.File -> [Hs.Module]
fileToModules file@New.File{fileName} =
    [ Hs.Module
        { modName = ["Capnp", "Gen"] ++ makeModName fileName ++ ["New"]
        , modLangPragmas =
            [ "TypeFamilies"
            , "DataKinds"
            , "FlexibleInstances"
            , "MultiParamTypeClasses"
            , "UndecidableInstances"
            ]
        , modExports = Nothing
        , modImports = imports
        , modDecls = fileToDecls file
        }
    ]

fileToDecls :: New.File -> [Hs.Decl]
fileToDecls New.File{fileId, decls} =
    concatMap (declToDecls fileId) decls


declToDecls :: Word64 -> New.Decl -> [Hs.Decl]
declToDecls thisMod decl =
    case decl of
        New.TypeDecl {name, params, repr} ->
            let dataName = Name.localToUnQ name
                typeArgs = toTVars params
            in
            [ Hs.DcData Hs.Data
                { dataName
                , typeArgs
                , dataVariants = []
                , derives = []
                , dataNewtype = False
                }
            , Hs.DcTypeInstance
                (Hs.TApp
                    (tgName ["R"] "ReprFor")
                    [ case typeArgs of
                        [] -> tuName dataName
                        _  -> Hs.TApp (tuName dataName) typeArgs
                    ]
                )
                (toType repr)
            ]
        New.FieldDecl{containerType, typeParams, fieldName, fieldLocType} ->
            let tVars = toTVars typeParams
                ctx = map paramConstraints tVars
                labelType = Hs.TString (Name.renderUnQ fieldName)
                parentType = Hs.TApp (Hs.TLName containerType) tVars
                childType = fieldLocTypeToType thisMod fieldLocType
            in
            [ Hs.DcInstance
                { ctx
                , typ = Hs.TApp (tgName ["OL"] "IsLabel")
                    [ labelType
                    , Hs.TApp (tgName ["F"] "Field") [parentType, childType]
                    ]
                , defs =
                    [ Hs.IdValue Hs.DfValue
                        { name = "fromLabel"
                        , value = fieldLocTypeToFromLabel fieldLocType
                        , params = []
                        }
                    ]
                }
            , Hs.DcInstance
                { ctx
                , typ = Hs.TApp (tgName ["F"] "HasField") [labelType, parentType, childType]
                , defs = []
                }
            ]

-- | Constraints required for a capnproto type parameter. The returned
-- expression has kind 'Constraint'.
paramConstraints :: Hs.Type -> Hs.Type
paramConstraints t =
    -- FIXME: make pr variable unique. Otherwise, this will
    -- require all type parameters to have the *same* representation.
    Hs.TApp (tgName ["GH"] "TypeParam") [t, Hs.TVar "pr"]

tCapnp :: Word64 -> Name.CapnpQ -> Hs.Type
tCapnp thisMod Name.CapnpQ{local, fileId}
    | thisMod == fileId = Hs.TLName local
    | otherwise = tgName (map Name.renderUnQ $ idToModule fileId ++ ["New"]) local

fieldLocTypeToType :: Word64 -> C.FieldLocType New.Brand Name.CapnpQ -> Hs.Type
fieldLocTypeToType thisMod = \case
    C.VoidField     -> Hs.TUnit
    C.DataField _ t -> wordTypeToType thisMod t
    C.PtrField _ t  -> ptrTypeToType thisMod t
    C.HereField t   -> compositeTypeToType thisMod t

wordTypeToType thisMod = \case
    C.EnumType t -> tCapnp thisMod t
    C.PrimWord t -> primWordToType t

primWordToType = \case
    C.PrimInt t   -> intTypeToType t
    C.PrimFloat32 -> tStd_ "Float"
    C.PrimFloat64 -> tStd_ "Double"
    C.PrimBool    -> tStd_ "Bool"

intTypeToType (C.IntType sign size) =
    let prefix = case sign of
            C.Signed   -> "Int"
            C.Unsigned -> "Word"
    in
    tStd_ $ fromString $ prefix ++ show (C.sizeBits size)

wordTypeBits = \case
    C.EnumType _                              -> 16
    C.PrimWord (C.PrimInt (C.IntType _ size)) -> C.sizeBits size
    C.PrimWord C.PrimFloat32                  -> 32
    C.PrimWord C.PrimFloat64                  -> 64
    C.PrimWord C.PrimBool                     -> 1

ptrTypeToType thisMod = \case
    C.ListOf t       -> Hs.TApp (tgName ["R"] "List") [typeToType thisMod t]
    C.PrimPtr t      -> primPtrToType t
    C.PtrComposite t -> compositeTypeToType thisMod t
    C.PtrInterface t -> interfaceTypeToType thisMod t
    C.PtrParam t     -> typeParamToType t

typeToType thisMod = \case
    C.CompositeType t -> compositeTypeToType thisMod t
    C.VoidType        -> Hs.TUnit
    C.WordType t      -> wordTypeToType thisMod t
    C.PtrType t       -> ptrTypeToType thisMod t

primPtrToType = \case
    C.PrimText     -> tgName ["Basics"] "Text"
    C.PrimData     -> tgName ["Basics"] "Data"
    C.PrimAnyPtr t -> anyPtrToType t

anyPtrToType :: C.AnyPtr -> Hs.Type
anyPtrToType t = tgName ["Basics"] $ case t of
    C.Struct -> "AnyStruct"
    C.List   -> "AnyList"
    C.Cap    -> "Capability"
    C.Ptr    -> "AnyPointer"

compositeTypeToType thisMod (C.StructType    name brand) = namedType thisMod name brand
interfaceTypeToType thisMod (C.InterfaceType name brand) = namedType thisMod name brand

typeParamToType = Hs.TVar . Name.typeVarName . C.paramName

namedType :: Word64 -> Name.CapnpQ -> C.ListBrand Name.CapnpQ -> Hs.Type
namedType thisMod name (C.ListBrand [])   = tCapnp thisMod name
namedType thisMod name (C.ListBrand args) =
    Hs.TApp
        (tCapnp thisMod name)
        [ typeToType thisMod (C.PtrType t) | t <- args ]

fieldLocTypeToFromLabel = \case
    C.DataField loc wt ->
        let shift = C.dataOff loc
            index = C.dataIdx loc
            nbits = wordTypeBits wt
            defaultValue = C.dataDef loc
        in
        Hs.EApp
            (egName ["GH"] "dataField")
            [ Hs.EInt $ fromIntegral shift
            , Hs.EInt $ fromIntegral index
            , Hs.EInt $ fromIntegral nbits
            , Hs.EInt $ fromIntegral defaultValue
            ]
    C.PtrField idx _ ->
        Hs.EApp (egName ["GH"] "ptrField") [Hs.EInt $ fromIntegral idx]
    C.VoidField ->
        egName ["GH"] "voidField"
    C.HereField _ ->
        -- TODO
        eStd_ "undefined"


class ToType a where
    toType :: a -> Hs.Type

instance ToType R.Repr where
    toType (R.Ptr p)  = rApp "Ptr" [toType p]
    toType (R.Data d) = rApp "Data" [toType d]

instance ToType a => ToType (Maybe a) where
    toType Nothing  = tStd_ "Nothing"
    toType (Just a) = Hs.TApp (tStd_ "Just") [toType a]

instance ToType R.PtrRepr where
    toType R.Cap      = tReprName "Cap"
    toType (R.List r) = rApp "List" [toType r]
    toType R.Struct   = tReprName "Struct"

instance ToType R.ListRepr where
    toType (R.ListNormal nl) = rApp "ListNormal" [toType nl]
    toType R.ListComposite   = tReprName "ListComposite"

instance ToType R.NormalListRepr where
    toType (R.ListData r) = rApp "ListData" [toType r]
    toType R.ListPtr      = tReprName "ListPtr"

instance ToType R.DataSz where
    toType = tReprName . fromString . show


rApp :: Name.LocalQ -> [Hs.Type] -> Hs.Type
rApp n = Hs.TApp (tReprName n)

tReprName :: Name.LocalQ -> Hs.Type
tReprName = tgName ["R"]
