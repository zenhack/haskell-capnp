{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.NewToHaskell
    ( fileToModules
    ) where

import qualified Capnp.Repr            as R
import           Control.Monad         (guard)
import           Data.String           (IsString(fromString))
import           Data.Word
import qualified IR.Common             as C
import qualified IR.Haskell            as Hs
import qualified IR.Name               as Name
import qualified IR.New                as New
import           Trans.ToHaskellCommon

-- | Modules imported by all generated modules.
imports :: [Hs.Import]
imports =
    [ Hs.ImportAs { importAs = "R", parts = ["Capnp", "Repr"] }
    , Hs.ImportAs { importAs = "RP", parts = ["Capnp", "Repr", "Parsed"] }
    , Hs.ImportAs { importAs = "Basics", parts = ["Capnp", "New", "Basics"] }
    , Hs.ImportAs { importAs = "OL", parts = ["GHC", "OverloadedLabels"] }
    , Hs.ImportAs { importAs = "GH", parts = ["Capnp", "GenHelpers", "New"] }
    , Hs.ImportAs { importAs = "C", parts = ["Capnp", "New", "Classes"] }
    , Hs.ImportAs { importAs = "Generics", parts = ["GHC", "Generics"] }
    ]

-- | Modules imported by generated modules that use rpc. We separate these out to
-- avoid a circular import when generating code for rpc.capnp -- which does not
-- contain interfaces, so does not need to import the rpc system -- but which
-- must be imported *by* the rpc system.
rpcImports :: [Hs.Import]
rpcImports =
    [ Hs.ImportAs { importAs = "GH", parts = ["Capnp", "GenHelpers", "New", "Rpc"] }
    ]

fileToModules :: New.File -> [Hs.Module]
fileToModules file =
    [ fileToMainModule file
    , fileToModuleAlias file
    ]

fileToMainModule :: New.File -> Hs.Module
fileToMainModule file@New.File{fileName, usesRpc} =
    fixImports $ Hs.Module
        { modName = ["Capnp", "Gen"] ++ makeModName fileName ++ ["New"]
        , modLangPragmas =
            [ "TypeFamilies"
            , "DataKinds"
            , "DeriveGeneric"
            , "DuplicateRecordFields"
            , "EmptyDataDeriving"
            , "FlexibleInstances"
            , "MultiParamTypeClasses"
            , "UndecidableInstances"
            , "UndecidableSuperClasses"
            , "OverloadedLabels"
            , "OverloadedStrings"
            , "StandaloneDeriving"
            , "RecordWildCards"
            , "TypeApplications"
            , "ScopedTypeVariables"
            ]
        , modExports = Nothing
        , modImports = imports ++ (guard usesRpc >> rpcImports)
        , modDecls = fileToDecls file
        }

fileToModuleAlias :: New.File -> Hs.Module
fileToModuleAlias New.File{fileId,fileName} =
    let reExport = ["Capnp", "Gen"] ++ makeModName fileName ++ ["New"]
    in Hs.Module
        { modName = idToModule fileId ++ ["New"]
        , modLangPragmas = []
        , modExports = Just [Hs.ExportMod reExport]
        , modImports = [ Hs.ImportAll { parts = reExport } ]
        , modDecls = []
        }

fileToDecls :: New.File -> [Hs.Decl]
fileToDecls New.File{fileId, decls} =
    concatMap (declToDecls fileId) decls


declToDecls :: Word64 -> New.Decl -> [Hs.Decl]
declToDecls thisMod decl =
    case decl of
        New.TypeDecl {name, nodeId, params, repr, extraTypeInfo} ->
            let dataName = Name.localToUnQ name
                typeArgs = toTVars params
                typ = case typeArgs of
                    [] -> tuName dataName
                    _  -> Hs.TApp (tuName dataName) typeArgs
            in
            [ Hs.DcData Hs.Data
                { dataName
                , typeArgs
                , dataVariants =
                    case extraTypeInfo of
                        Just (New.EnumTypeInfo variants) ->
                            [ Hs.DataVariant
                                { dvCtorName = Name.localToUnQ $ Name.mkSub name variantName
                                , dvArgs = Hs.APos []
                                }
                            | variantName <- variants
                            ]
                            ++
                            [ declareUnknownVariant name ]
                        _ -> []
                , derives =
                    case extraTypeInfo of
                        Just New.EnumTypeInfo {} -> [ "Std_.Eq", "Std_.Show", "Generics.Generic" ]
                        _                        -> []
                , dataNewtype = False
                , dataInstance = False
                }
            , Hs.DcTypeInstance
                (Hs.TApp (tgName ["R"] "ReprFor") [typ])
                (toType repr)
            , Hs.DcInstance
                { ctx = []
                , typ = Hs.TApp (tgName ["C"] "HasTypeId") [typ]
                , defs =
                    [ Hs.IdValue Hs.DfValue
                        { name = "typeId"
                        , params = []
                        , value = Hs.EInt (fromIntegral nodeId)
                        }
                    ]
                }
            ] ++
            let ctx = paramsContext typeArgs in
            case extraTypeInfo of
                Just New.StructTypeInfo{nWords, nPtrs} ->
                    [ Hs.DcInstance
                        { ctx
                        , typ = Hs.TApp (tgName ["C"] "TypedStruct") [typ]
                        , defs =
                            [ Hs.IdValue Hs.DfValue
                                { name = "numStructWords"
                                , params = []
                                , value = Hs.EInt $ fromIntegral nWords
                                }
                            , Hs.IdValue Hs.DfValue
                                { name = "numStructPtrs"
                                , params = []
                                , value = Hs.EInt $ fromIntegral nPtrs
                                }
                            ]
                        }
                    , Hs.DcInstance
                        { ctx
                        , typ = Hs.TApp (tgName ["C"] "Allocate") [typ]
                        , defs =
                            [ Hs.IdType $
                                Hs.TypeAlias "AllocHint" [typ] Hs.TUnit
                            , Hs.IdValue Hs.DfValue
                                { name = "new"
                                , params = [Hs.PVar "_"]
                                , value = egName ["C"] "newTypedStruct"
                                }
                            ]
                        }
                    , Hs.DcInstance
                        { ctx
                        , typ = Hs.TApp
                            (tgName ["C"] "EstimateAlloc")
                            [ typ, Hs.TApp (tgName ["C"] "Parsed") [typ] ]
                        , defs = []
                        }
                    , Hs.DcInstance
                        { ctx
                        , typ = Hs.TApp (tgName ["C"] "AllocateList") [typ]
                        , defs =
                            [ Hs.IdType $ Hs.TypeAlias "ListAllocHint" [typ] (tStd_ "Int")
                            , Hs.IdValue Hs.DfValue
                                { name = "newList"
                                , params = []
                                , value = egName ["C"] "newTypedStructList"
                                }
                            ]
                        }
                    , Hs.DcInstance
                        { ctx
                        , typ = Hs.TApp
                            (tgName ["C"] "EstimateListAlloc")
                            [typ, Hs.TApp (tgName ["C"] "Parsed") [typ]]
                        , defs = []
                        }
                    ]
                Just (New.EnumTypeInfo variants) ->
                    [ Hs.DcInstance
                        { ctx
                        , typ = Hs.TApp (tStd_ "Enum") [typ]
                        , defs =
                            [ Hs.IdValue Hs.DfValue
                                { name = "toEnum"
                                , params = [Hs.PVar "n_"]
                                , value =
                                    Hs.ECase (euName "n_") $
                                        [ ( Hs.PInt i
                                          , Hs.ELName (Name.mkSub name variantName)
                                          )
                                        | (i, variantName) <- zip [0..] variants
                                        ] ++
                                        [ ( Hs.PVar "tag_"
                                          , Hs.EApp
                                                (Hs.ELName (unknownVariant name))
                                                [Hs.EApp (eStd_ "fromIntegral") [euName "tag_"]]
                                          )
                                        ]

                                }
                            , Hs.IdValue Hs.DfValue
                                { name = "fromEnum"
                                , params = [Hs.PVar "value_"]
                                , value =
                                    Hs.ECase (euName "value_") $
                                        [ ( Hs.PLCtor (Name.mkSub name variantName) []
                                          , Hs.EInt i
                                          )
                                        | (i, variantName) <- zip [0..] variants
                                        ] ++
                                        [ ( Hs.PLCtor (unknownVariant name) [Hs.PVar "tag_"]
                                          , Hs.EApp (eStd_ "fromIntegral") [euName "tag_"]
                                          )
                                        ]
                                }
                            ]
                        }
                    , Hs.DcInstance
                        { ctx
                        , typ = Hs.TApp (tgName ["C"] "IsWord") [typ]
                        , defs =
                            [ Hs.IdValue Hs.DfValue
                                { name = "fromWord"
                                , params = [Hs.PVar "w_"]
                                , value = Hs.EApp (eStd_ "toEnum") [Hs.EApp (eStd_ "fromIntegral") [Hs.EVar "w_"]]
                                }
                            , Hs.IdValue Hs.DfValue
                                { name = "toWord"
                                , params = [Hs.PVar "v_"]
                                , value = Hs.EApp (eStd_ "fromIntegral") [Hs.EApp (eStd_ "fromEnum") [Hs.EVar "v_"]]
                                }
                            ]
                        }
                    , Hs.DcInstance
                        { ctx
                        , typ = Hs.TApp (tgName ["C"] "Parse") [typ, typ]
                        , defs =
                            [ Hs.IdValue Hs.DfValue
                                { name = "parse"
                                , params = []
                                , value = egName ["GH"] "parseEnum"
                                }
                            , Hs.IdValue Hs.DfValue
                                { name = "encode"
                                , params = []
                                , value = egName ["GH"] "encodeEnum"
                                }
                            ]
                        }
                    , Hs.DcInstance
                        { ctx
                        , typ = Hs.TApp (tgName ["C"] "AllocateList") [typ]
                        , defs =
                            [ Hs.IdType $ Hs.TypeAlias "ListAllocHint" [typ] (tStd_ "Int")
                            ]
                        }
                    , Hs.DcInstance
                        { ctx
                        , typ = Hs.TApp (tgName ["C"] "EstimateListAlloc") [typ, typ]
                        , defs = []
                        }
                    ]
                Just New.InterfaceTypeInfo {methods, supers} ->
                    defineInterfaceParse name params
                    ++ defineInterfaceServer thisMod name params methods supers
                Nothing -> []
        New.FieldDecl{containerType, typeParams, fieldName, fieldLocType} ->
            let tVars = toTVars typeParams
                ctx = paramsContext tVars
                labelType = Hs.TString (Name.renderUnQ fieldName)
                parentType = Hs.TApp (Hs.TLName containerType) tVars
                childType = fieldLocTypeToType thisMod fieldLocType
                fieldKind = Hs.TGName $ fieldLocTypeToFieldKind fieldLocType
            in
            [ Hs.DcInstance
                { ctx
                , typ = Hs.TApp
                    (tgName ["GH"] "HasField")
                    [labelType, fieldKind, parentType, childType]
                , defs =
                    [ Hs.IdValue Hs.DfValue
                        { name = "fieldByLabel"
                        , value = fieldLocTypeToField fieldLocType
                        , params = []
                        }
                    ]
                }
            ]
        New.UnionDecl{name, typeParams, tagLoc, variants} ->
            let tVars = toTVars typeParams
                typ = Hs.TApp (Hs.TLName name) tVars
            in
            Hs.DcInstance
                { ctx = paramsContext tVars
                , typ = Hs.TApp (tgName ["GH"] "HasUnion") [typ]
                , defs =
                    [ Hs.IdValue Hs.DfValue
                        { name = "unionField"
                        , params = []
                        , value = fieldLocTypeToField $ C.DataField
                            tagLoc
                            (C.PrimWord (C.PrimInt (C.IntType C.Unsigned C.Sz16)))
                        }
                    , defineRawData thisMod name tVars variants
                    , defineInternalWhich name variants
                    , Hs.IdData Hs.Data
                        { dataName = "Which"
                        , typeArgs = [typ]
                        , dataVariants = []
                        , derives = []
                        , dataNewtype = False
                        , dataInstance = False
                        }
                    ]
                }
            : concatMap (variantToDecls thisMod name typeParams) variants
        New.SuperDecl { subName, typeParams, superType } ->
            let tVars = toTVars typeParams in
            [ Hs.DcInstance
                { ctx = paramsContext tVars
                , typ = Hs.TApp (tgName ["C"] "Super")
                    [ typeToType thisMod $ C.PtrType $ C.PtrInterface superType
                    , Hs.TApp (Hs.TLName subName) tVars
                    ]
                , defs = []
                }
            ]
        New.MethodDecl
                { interfaceName
                , interfaceId
                , methodId
                , methodInfo = New.MethodInfo
                    { typeParams
                    , methodName
                    , paramType
                    , resultType
                    }
                } ->
            let tVars = toTVars typeParams in
            [ Hs.DcInstance
                { ctx = paramsContext tVars
                , typ = Hs.TApp
                    (tgName ["GH"] "HasMethod")
                    [ Hs.TString (Name.renderUnQ methodName)
                    , Hs.TApp (Hs.TLName interfaceName) tVars
                    , compositeTypeToType thisMod paramType
                    , compositeTypeToType thisMod resultType
                    ]
                , defs =
                    [ Hs.IdValue Hs.DfValue
                        { name = "methodByLabel"
                        , params = []
                        , value = Hs.EApp
                            (egName ["GH"] "Method")
                            [ Hs.EInt $ fromIntegral interfaceId
                            , Hs.EInt $ fromIntegral methodId
                            ]
                        }
                    ]
                }
            ]
        New.ParsedInstanceDecl{ typeName, typeParams, parsedInstances } ->
            defineParsedInstances thisMod typeName typeParams parsedInstances
        New.ConstDecl { name, value } ->
            defineConstant thisMod name value

defineConstant thisMod localName value =
    let name = Name.localToUnQ localName in
    case value of
        C.VoidValue ->
            [ Hs.DcValue
                { typ = Hs.TUnit
                , def = Hs.DfValue
                    { name
                    , params = []
                    , value = Hs.ETup []
                    }
                }
            ]
        C.WordValue t v ->
            [ Hs.DcValue
                { typ = typeToType thisMod (C.WordType t)
                , def = Hs.DfValue
                    { name
                    , params = []
                    , value = Hs.EApp (egName ["C"] "fromWord") [Hs.EInt (fromIntegral v)]
                    }
                }
            ]
        C.PtrValue t v ->
            [ Hs.DcValue
                { typ = Hs.TApp (tgName ["R"] "Raw") [ typeToType thisMod (C.PtrType t), tgName ["GH"] "Const"]
                , def = Hs.DfValue
                    { name
                    , params = []
                    , value = Hs.EApp
                        (egName ["GH"] "getPtrConst")
                        [Hs.ETypeAnno (Hs.EBytes (makePtrBytes v)) (tgName ["GH"] "ByteString")]
                    }
                }
            ]

defineRawData thisMod name tVars variants =
    Hs.IdData Hs.Data
        { dataName = "RawWhich"
        , typeArgs =
            [ Hs.TApp (Hs.TVar $ Name.renderUnQ $ Name.localToUnQ name) tVars
            , Hs.TVar "mut_"
            ]

        , dataNewtype = False
        , dataInstance = False
        , dataVariants =
            [ Hs.DataVariant
                { dvCtorName =
                    "RW_" <> Name.localToUnQ (Name.mkSub name variantName)
                , dvArgs = Hs.APos
                    [ Hs.TApp
                        (tReprName "Raw")
                        [ fieldLocTypeToType thisMod fieldLocType
                        , Hs.TVar "mut_"
                        ]
                    ]
                }
            | New.UnionVariant{variantName, fieldLocType} <- variants
            ]
            ++
            [ Hs.DataVariant
                { dvCtorName = "RW_" <> Name.localToUnQ (unknownVariant name)
                , dvArgs = Hs.APos [tStd_ "Word16"]
                }
            ]

        -- TODO: derive Show, Read, Eq, Generic, to be feature complete with the code generated by RawToHaskell.
        -- This will require a stand-alone deriving declaration
        , derives = []
        }

unknownVariant :: Name.LocalQ -> Name.LocalQ
unknownVariant name = Name.mkSub name "unknown'"

rawCtorName :: Name.LocalQ -> Name.UnQ
rawCtorName local = "RW_" <> Name.localToUnQ local


defineInternalWhich structName variants =
    Hs.IdValue Hs.DfValue
        { name = "internalWhich"
        , params = [Hs.PVar "tag_", Hs.PVar "struct_"]
        , value =
            Hs.ECase (Hs.ELName "tag_") $
                [ ( Hs.PInt $ fromIntegral tagValue
                  , Hs.EFApp
                        (euName $ rawCtorName (Name.mkSub structName variantName))
                        [ Hs.EApp
                            (egName ["GH"] "readVariant")
                            [ Hs.ELabel variantName
                            , euName "struct_"
                            ]
                        ]
                  )
                | New.UnionVariant{tagValue, variantName} <- variants
                ]
                ++
                [ ( Hs.PVar "_"
                  , Hs.EApp (eStd_ "pure")
                        [ Hs.EApp
                            (euName $ rawCtorName (unknownVariant structName))
                            [euName "tag_"]
                        ]
                  )
                ]
        }

variantToDecls thisMod containerType typeParams New.UnionVariant{tagValue, variantName, fieldLocType} =
    let tVars = toTVars typeParams
        ctx = paramsContext tVars
        labelType = Hs.TString (Name.renderUnQ variantName)
        parentType = Hs.TApp (Hs.TLName containerType) tVars
        childType = fieldLocTypeToType thisMod fieldLocType
        fieldKind = Hs.TGName $ fieldLocTypeToFieldKind fieldLocType
    in
    [ Hs.DcInstance
        { ctx
        , typ = Hs.TApp
            (tgName ["GH"] "HasVariant")
            [labelType, fieldKind, parentType, childType]
        , defs =
            [ Hs.IdValue Hs.DfValue
                { name = "variantByLabel"
                , params = []
                , value = Hs.EApp
                    (egName ["GH"] "Variant")
                    [ fieldLocTypeToField fieldLocType
                    , Hs.EInt (fromIntegral tagValue)
                    ]
                }
            ]
        }
    ]

paramsContext :: [Hs.Type] -> [Hs.Type]
paramsContext = map paramConstraints

-- | Constraints required for a capnproto type parameter. The returned
-- expression has kind 'Constraint'.
paramConstraints :: Hs.Type -> Hs.Type
paramConstraints t =
    Hs.TApp (tgName ["GH"] "TypeParam") [t]

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

fieldLocTypeToFieldKind :: C.FieldLocType b n -> Name.GlobalQ
fieldLocTypeToFieldKind = \case
    C.HereField _ -> gName ["GH"] "Group"
    _             -> gName ["GH"] "Slot"

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
anyPtrToType t = case t of
    C.Struct -> basics "AnyStruct"
    C.List   -> basics "AnyList"
    C.Cap    -> basics "Capability"
    C.Ptr    -> Hs.TApp (tStd_ "Maybe") [basics "AnyPointer"]
  where
    basics = tgName ["Basics"]

compositeTypeToType thisMod (C.StructType    name brand) = namedType thisMod name brand
interfaceTypeToType thisMod (C.InterfaceType name brand) = namedType thisMod name brand

typeParamToType = Hs.TVar . Name.typeVarName . C.paramName

namedType :: Word64 -> Name.CapnpQ -> C.ListBrand Name.CapnpQ -> Hs.Type
namedType thisMod name (C.ListBrand [])   = tCapnp thisMod name
namedType thisMod name (C.ListBrand args) =
    Hs.TApp
        (tCapnp thisMod name)
        [ typeToType thisMod (C.PtrType t) | t <- args ]

fieldLocTypeToField = \case
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
        egName ["GH"] "groupField"


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

declareUnknownVariant :: Name.LocalQ -> Hs.DataVariant
declareUnknownVariant name = Hs.DataVariant
    { dvCtorName = Name.localToUnQ $ Name.mkSub name "unknown'"
    , dvArgs = Hs.APos [tStd_ "Word16"]
    }


defineParsedInstances :: Word64 -> Name.LocalQ -> [Name.UnQ] -> New.ParsedInstances -> [Hs.Decl]
defineParsedInstances thisMod typeName typeParams instanceInfo =
    concatMap (\f -> f typeName typeParams instanceInfo)
        [ defineParsed thisMod
        , defineParse
        , defineMarshal
        ]

defineParsed :: Word64 -> Name.LocalQ -> [Name.UnQ] -> New.ParsedInstances -> [Hs.Decl]
defineParsed thisMod typeName typeParams instanceInfo =
    let tVars = map (Hs.TVar . Name.typeVarName) typeParams
        typ = Hs.TApp (Hs.TLName typeName) tVars
        parsedTy = case instanceInfo of
                    New.ParsedStruct{} -> typ
                    New.ParsedUnion{}  -> Hs.TApp (tgName ["GH"] "Which") [typ]
    in
    Hs.DcData Hs.Data
        { dataName = "C.Parsed"
        , typeArgs = [parsedTy]
        , derives = [ "Generics.Generic" ]
        , dataNewtype = False
        , dataInstance = True
        , dataVariants =
            case instanceInfo of
                New.ParsedStruct { fields, hasUnion, dataCtorName } ->
                    [ Hs.DataVariant
                        { dvCtorName = Name.localToUnQ dataCtorName
                        , dvArgs = Hs.ARec $
                            [ ( name
                              , Hs.TApp
                                  (tgName ["RP"] "Parsed")
                                  [fieldLocTypeToType thisMod typ]
                              )
                            | (name, typ) <- fields
                            ] ++
                            [ ( "union'"
                              , Hs.TApp (tgName ["C"] "Parsed")
                                  [Hs.TApp (tgName ["GH"] "Which") [typ]]
                              )
                            | hasUnion
                            ]
                        }
                    ]
                New.ParsedUnion { variants } ->
                    [ Hs.DataVariant
                        { dvCtorName = Name.localToUnQ $ Name.mkSub typeName name
                        , dvArgs = case ftype of
                            C.VoidField -> Hs.APos []
                            _ -> Hs.APos
                                [ Hs.TApp
                                    (tgName ["RP"] "Parsed")
                                    [fieldLocTypeToType thisMod ftype]
                                ]
                        }
                    | (name, ftype) <- variants
                    ]
                    ++
                    [ declareUnknownVariant typeName ]

        }
    :
    [ Hs.DcDeriveInstance
        [ Hs.TApp (tStd_ cls) [Hs.TApp (tgName ["RP"] "Parsed") [v]]
        | v <- tVars
        ]
        (Hs.TApp (tStd_ cls) [Hs.TApp (tgName ["C"] "Parsed") [parsedTy]])
    | cls <- ["Show", "Eq"]
    ]

defineParse :: Name.LocalQ -> [Name.UnQ] -> New.ParsedInstances -> [Hs.Decl]
defineParse typeName typeParams New.ParsedStruct { fields, hasUnion, dataCtorName } =
    let tVars = toTVars typeParams
        typ = Hs.TApp (Hs.TLName typeName) tVars
    in
    [ Hs.DcInstance
        { ctx = paramsContext tVars
        , typ = Hs.TApp (tgName ["C"] "Parse") [typ, Hs.TApp (tgName ["C"] "Parsed") [typ]]
        , defs =
            [ Hs.IdValue Hs.DfValue
                { name = "parse"
                , params = [Hs.PVar "raw_"]
                , value = Hs.EFApp (Hs.ELName dataCtorName) $
                    [ Hs.EApp
                        (egName ["GH"] "parseField")
                        [Hs.ELabel field, euName "raw_"]
                    | field <- map fst fields
                    ]
                    ++
                    if hasUnion then
                        [ Hs.EApp
                            (egName ["C"] "parse")
                            [Hs.EApp (egName ["GH"] "structUnion") [euName "raw_"]]
                        ]
                    else
                        []
                }
            ]
        }
    ]
defineParse typeName typeParams New.ParsedUnion{ variants } =
    let tVars = toTVars typeParams
        typ = Hs.TApp (tgName ["GH"] "Which") [Hs.TApp (Hs.TLName typeName) tVars]
    in
    [ Hs.DcInstance
        { ctx = paramsContext tVars
        , typ = Hs.TApp (tgName ["C"] "Parse") [typ, Hs.TApp (tgName ["C"] "Parsed") [typ]]
        , defs =
            [ Hs.IdValue Hs.DfValue
                { name = "parse"
                , params = [Hs.PVar "raw_"]
                , value = Hs.EDo
                    [ Hs.DoBind "rawWhich_" $ Hs.EApp (egName ["GH"] "unionWhich") [euName "raw_"]
                    ]
                    (Hs.ECase (euName "rawWhich_") $
                        [ let ctorName = Name.mkSub typeName variantName in
                          case fieldLocType of
                            C.VoidField ->
                                ( puName (rawCtorName ctorName) [Hs.PVar "_"]
                                , Hs.EApp (eStd_ "pure") [Hs.ELName ctorName]
                                )
                            _ ->
                                ( puName (rawCtorName ctorName) [Hs.PVar "rawArg_"]
                                , Hs.EFApp
                                    (Hs.ELName ctorName)
                                    [Hs.EApp (egName ["C"] "parse") [euName "rawArg_"]]
                                )
                        | (variantName, fieldLocType) <- variants
                        ]
                        ++
                        [ let ctorName = unknownVariant typeName in
                          ( puName (rawCtorName ctorName) [Hs.PVar "tag_"]
                          , Hs.EApp
                                (eStd_ "pure")
                                [Hs.EApp (Hs.ELName ctorName) [euName "tag_"]]
                          )
                        ]
                    )
                }
            ]
        }
    ]

defineInterfaceParse typeName typeParams =
    let tVars = toTVars typeParams
        typ = Hs.TApp (Hs.TLName typeName) tVars
    in
    [ Hs.DcInstance
        { ctx = paramsContext tVars
        , typ = Hs.TApp (tgName ["C"] "Parse") [typ, Hs.TApp (tgName ["GH"] "Client") [typ]]
        , defs =
            [ Hs.IdValue Hs.DfValue
                { name = "parse"
                , params = []
                , value = egName ["GH"] "parseCap"
                }
            , Hs.IdValue Hs.DfValue
                { name = "encode"
                , params = []
                , value = egName ["GH"] "encodeCap"
                }
            ]
        }
    ]

defineMarshal :: Name.LocalQ -> [Name.UnQ] -> New.ParsedInstances -> [Hs.Decl]
defineMarshal typeName typeParams New.ParsedStruct { fields, hasUnion, dataCtorName } =
    let tVars = toTVars typeParams
        typ = Hs.TApp (Hs.TLName typeName) tVars
    in
    [ Hs.DcInstance
        { ctx = paramsContext tVars
        , typ = Hs.TApp
                    (tgName ["C"] "Marshal")
                    [typ, Hs.TApp (tgName ["C"] "Parsed") [typ]]
        , defs =
            [ if null fields && not hasUnion then
                -- We need to special case this, since otherwise GHC will complain about
                -- the record wildcard pattern on a ctor with no arguments.
                Hs.IdValue Hs.DfValue
                    { name = "marshalInto"
                    , params = [ Hs.PVar "_raw", Hs.PLCtor dataCtorName [] ]
                    , value = Hs.EApp (eStd_ "pure") [Hs.ETup []]
                    }
              else
                Hs.IdValue Hs.DfValue
                    { name = "marshalInto"
                    , params =
                        [ Hs.PVar "raw_", Hs.PLRecordWildCard dataCtorName]
                    , value = Hs.EDo
                        (map (Hs.DoE . uncurry emitMarshalField) fields)
                        (if hasUnion then
                            Hs.EApp (egName ["C"] "marshalInto")
                              [ Hs.EApp (egName ["GH"] "structUnion") [euName "raw_"]
                              , euName "union'"
                              ]
                        else
                            Hs.EApp (eStd_ "pure") [Hs.ETup []]
                        )
                    }
            ]
        }
    ]
defineMarshal typeName typeParams New.ParsedUnion { variants } =
    let tVars = toTVars typeParams
        typ = Hs.TApp (tgName ["GH"] "Which") [Hs.TApp (Hs.TLName typeName) tVars]
    in
    [ Hs.DcInstance
        { ctx = paramsContext tVars
        , typ = Hs.TApp
            (tgName ["C"] "Marshal")
            [typ, Hs.TApp (tgName ["C"] "Parsed") [typ]]
        , defs =
            [ Hs.IdValue Hs.DfValue
                { name = "marshalInto"
                , params = [ Hs.PVar "raw_", Hs.PVar "parsed_" ]
                , value = Hs.ECase (Hs.EVar "parsed_") $
                    [ ( Hs.PLCtor (Name.mkSub typeName name) $
                            case fieldLocType of
                                C.VoidField -> []
                                _           -> [Hs.PVar "arg_"]
                      , emitMarshalVariant name fieldLocType
                      )
                    | (name, fieldLocType) <- variants
                    ] ++
                    [ ( Hs.PLCtor (unknownVariant typeName) [Hs.PVar "tag_"]
                      , Hs.EApp
                            (egName ["GH"] "encodeField")
                            [ egName ["GH"] "unionField"
                            , Hs.EVar "tag_"
                            , unionStruct (euName "raw_")
                            ]
                      )
                    ]
                }
            ]
        }
    ]

defineInterfaceServer thisMod typeName typeParams methods supers =
    let tVars = toTVars typeParams
        typ = Hs.TApp (Hs.TLName typeName) tVars
        clsName = Name.mkSub typeName "server_"
    in
    [ Hs.DcInstance
        { ctx = paramsContext tVars
        , typ = Hs.TApp (tgName ["GH"] "Export") [typ]
        , defs =
            [ Hs.IdType $ Hs.TypeAlias "Server" [typ] $ Hs.TApp (Hs.TLName clsName) tVars
            , Hs.IdValue Hs.DfValue
                { name = "methodHandlerTree"
                , params = [Hs.PVar "_", Hs.PVar "s_"]
                , value =
                    Hs.EApp (egName ["GH"] "MethodHandlerTree")
                        [ Hs.ETypeApp (egName ["C"] "typeId") [typ]
                        , Hs.EList
                            [ Hs.EApp
                                (egName ["GH"] "toUntypedMethodHandler")
                                [ Hs.EApp
                                    (Hs.ETypeApp
                                        (Hs.EVar (Name.renderUnQ (Name.valueName (Name.mkSub typeName methodName))))
                                        tVars
                                    )
                                    [Hs.EVar "s_"]
                                ]
                            | New.MethodInfo{methodName} <- methods
                            ]
                        , Hs.EList
                            [ Hs.EApp (egName ["GH"] "methodHandlerTree")
                                [ Hs.ETypeApp
                                    (egName ["GH"] "Proxy")
                                    [typeToType thisMod $ C.PtrType $ C.PtrInterface super]
                                , Hs.EVar "s_"
                                ]
                            | super <- supers
                            ]
                        ]
                }
            ]
        }
    , Hs.DcClass
        { ctx =
            [ Hs.TApp (tgName ["GH"] "Server")
                [ typeToType thisMod $ C.PtrType $ C.PtrInterface super
                , Hs.TVar "s_"
                ]
            | super <- supers
            ]
        , name = clsName
        , params = map (Name.UnQ . Name.typeVarName) typeParams ++ ["s_"]
        , funDeps = []
        , decls =
            Hs.CdMinimal
                [ mkMethodName typeName methodName
                | New.MethodInfo{methodName} <- methods
                ]
            : concatMap (defineIfaceClassMethod thisMod typeName) methods
        }
    ]
defineIfaceClassMethod thisMod typeName New.MethodInfo{methodName, paramType, resultType} =
    let mkType t = typeToType thisMod (C.CompositeType t)
        name = mkMethodName typeName methodName
    in
    [ Hs.CdValueDecl
        name
        (Hs.TFn
            [ Hs.TVar "s_"
            , Hs.TApp
                (tgName ["GH"] "MethodHandler")
                [ mkType paramType
                , mkType resultType
                ]
            ]
        )
    , Hs.CdValueDef Hs.DfValue
        { name
        , params = [Hs.PVar "_"]
        , value = egName ["GH"] "methodUnimplemented"
        }
    ]

mkMethodName typeName methodName =
    Name.valueName (Name.mkSub typeName methodName)

emitMarshalField :: Name.UnQ -> C.FieldLocType New.Brand Name.CapnpQ -> Hs.Exp
emitMarshalField name (C.HereField _) =
    Hs.EDo
        [ Hs.DoBind "group_" $ Hs.EApp
            (egName ["GH"] "readField")
            [Hs.ELabel name, Hs.EVar "raw_"]
        ]
        (Hs.EApp (egName ["C"] "marshalInto") [Hs.EVar "group_", euName name])
emitMarshalField name _ =
    Hs.EApp (egName ["GH"] "encodeField")
        [ Hs.ELabel name
        , euName name
        , euName "raw_"
        ]

emitMarshalVariant :: Name.UnQ -> C.FieldLocType New.Brand Name.CapnpQ -> Hs.Exp
emitMarshalVariant name (C.HereField _) = Hs.EDo
    [ Hs.DoBind "rawGroup_" $
        Hs.EApp (egName ["GH"] "initVariant")
            [ Hs.ELabel name
            , unionStruct (euName "raw_")
            ]
    ]
    (Hs.EApp (egName ["C"] "marshalInto") [euName "rawGroup_", euName "arg_"])
emitMarshalVariant name C.VoidField =
    Hs.EApp (egName ["GH"] "encodeVariant")
        [ Hs.ELabel name
        , Hs.ETup []
        , unionStruct (euName "raw_")
        ]
emitMarshalVariant name _ =
    Hs.EApp (egName ["GH"] "encodeVariant")
        [ Hs.ELabel name
        , euName "arg_"
        , unionStruct (euName "raw_")
        ]

unionStruct :: Hs.Exp -> Hs.Exp
unionStruct e = Hs.EApp (egName ["GH"] "unionStruct") [e]
