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
    , Hs.ImportAs { importAs = "RP", parts = ["Capnp", "Repr", "Parsed"] }
    , Hs.ImportAs { importAs = "Basics", parts = ["Capnp", "New", "Basics"] }
    , Hs.ImportAs { importAs = "OL", parts = ["GHC", "OverloadedLabels"] }
    , Hs.ImportAs { importAs = "GH", parts = ["Capnp", "GenHelpers", "New"] }
    , Hs.ImportAs { importAs = "C", parts = ["Capnp", "New", "Classes"] }
    , Hs.ImportAs { importAs = "Generics", parts = ["GHC", "Generics"] }
    ]

fileToModules :: New.File -> [Hs.Module]
fileToModules file =
    [ fileToMainModule file
    , fileToModuleAlias file
    ]

fileToMainModule :: New.File -> Hs.Module
fileToMainModule file@New.File{fileName} =
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
            , "OverloadedLabels"
            , "StandaloneDeriving"
            , "RecordWildCards"
            ]
        , modExports = Nothing
        , modImports = imports
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
        New.TypeDecl {name, params, repr, extraTypeInfo} ->
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
                        Just New.EnumTypeInfo {} -> [ "Std_.Eq", "Std_.Show" ]
                        _                        -> []
                , dataNewtype = False
                , dataInstance = False
                }
            , Hs.DcTypeInstance
                (Hs.TApp (tgName ["R"] "ReprFor") [typ])
                (toType repr)
            ] ++
            case extraTypeInfo of
                Just New.StructTypeInfo{nWords, nPtrs} ->
                    let ctx = paramsContext typeArgs in
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
                    ]
                _ -> []
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
        New.MethodDecl
                { interfaceName
                , typeParams
                , interfaceId
                , methodName
                , methodId
                , paramType
                , resultType
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


defineRawData thisMod name tVars variants =
    Hs.IdData Hs.Data
        { dataName = "RawWhich"
        , typeArgs =
            [ Hs.TVar "mut_"
            , Hs.TApp (Hs.TVar $ Name.renderUnQ $ Name.localToUnQ name) tVars
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
                        [ Hs.TVar "mut_"
                        , fieldLocTypeToType thisMod fieldLocType
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
paramsContext tVars =
    zipWith paramConstraints tVars (map (("pr_" ++) . show) [1..])

-- | Constraints required for a capnproto type parameter. The returned
-- expression has kind 'Constraint'.
--
-- The second argument is a unique type variable name within this scope.
paramConstraints :: Hs.Type -> String -> Hs.Type
paramConstraints t s =
    Hs.TApp (tgName ["GH"] "TypeParam") [t, Hs.TVar $ fromString s]

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
        [ (defineParsed thisMod)
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
            [ if fields == [] && not hasUnion then
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
defineMarshal _ _ _ = [] -- TODO

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
