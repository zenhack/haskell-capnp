{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.PureToHaskell (fileToModules) where

import Data.Word

import Control.Monad (guard)
import Text.Printf   (printf)

import qualified Data.Text as T

import IR.Haskell
import Trans.ToHaskellCommon

import qualified IR.Common as C
import qualified IR.Name   as Name
import qualified IR.Pure   as P


-- | Modules imported by all generated modules.
commonImports :: [Import]
commonImports =
    [ ImportAs { importAs = "V", parts = reExp ["Data", "Vector"] }
    , ImportAs { importAs = "T", parts = reExp ["Data", "Text"] }
    , ImportAs { importAs = "BS", parts = reExp ["Data", "ByteString"] }
    , ImportAs { importAs = "Default", parts = reExp ["Data", "Default"] }
    , ImportAs { importAs = "Generics", parts = ["GHC", "Generics"] }
    , ImportAs { importAs = "MonadIO", parts = ["Control", "Monad", "IO", "Class"] }
    , ImportAs { importAs = "UntypedPure", parts = ["Capnp", "Untyped", "Pure"] }
    , ImportAs { importAs = "Untyped", parts = ["Capnp", "Untyped"] }
    , ImportAs { importAs = "Message", parts = ["Capnp", "Message"] }
    , ImportAs { importAs = "Classes", parts = ["Capnp", "Classes"] }
    , ImportAs { importAs = "BasicsPure", parts = ["Capnp", "Basics", "Pure" ] }
    , ImportAs { importAs = "GenHelpersPure", parts = ["Capnp", "GenHelpers", "Pure"] }
    ]


-- | Modules imported by generated modules that use rpc.
rpcImports :: [Import]
rpcImports =
    [ ImportAs { importAs = "Rpc", parts = ["Capnp", "Rpc", "Untyped"] }
    , ImportAs { importAs = "Server", parts = ["Capnp", "Rpc", "Server"] }
    , ImportAs { importAs = "RpcHelpers", parts = ["Capnp", "GenHelpers", "Rpc"] }
    , ImportAs { importAs = "STM", parts = reExp ["Control", "Concurrent", "STM"] }
    , ImportAs { importAs = "STM", parts = reExp [ "Control", "Monad", "STM", "Class"] }
    , ImportAs { importAs = "Supervisors", parts = reExp ["Supervisors"] }
    ]

reExp :: [Name.UnQ] -> [Name.UnQ]
reExp parts = ["Capnp", "GenHelpers", "ReExports"] ++ parts

-- | Whether the serialized and unserialized forms of this type
-- are the same. If not, there is a marshalling step, if so, the
-- accessors work with the decerialized form directly.
--
-- For example, this is True for Enums, basic integers, etc. but
-- False for structs, interfaces, etc.
cerialEq :: C.Type b r -> Bool
cerialEq C.VoidType          = True
cerialEq (C.WordType _)      = True
cerialEq (C.CompositeType _) = False
cerialEq (C.PtrType _)       = False

fileToModules :: P.File -> [Module]
fileToModules file =
    [ fileToMainModule file
    , fileToModuleAlias file
    ]

fileToModuleAlias :: P.File -> Module
fileToModuleAlias P.File{fileName, fileId} =
    let reExport = ["Capnp", "Gen"] ++ makeModName fileName ++ ["Pure"]
    in Module
        { modName = idToModule fileId ++ ["Pure"]
        , modLangPragmas = []
        , modExports = Just [ExportMod reExport]
        , modImports = [ ImportAll { parts = reExport } ]
        , modDecls = []
        }

fileToMainModule :: P.File -> Module
fileToMainModule P.File{fileName, fileId, decls, reExportEnums, usesRpc} =
    fixImports $ Module
        { modName = ["Capnp", "Gen"] ++ makeModName fileName ++ ["Pure"]
        , modLangPragmas =
            [ "DeriveGeneric"
            , "DuplicateRecordFields"
            , "FlexibleContexts"
            , "FlexibleInstances"
            , "FunctionalDependencies"
            , "RecordWildCards"
            , "MultiParamTypeClasses"
            , "TypeFamilies"
            , "UndecidableInstances"
            ]
        , modExports = Just $
            [ExportGCtors (gName (rawModule fileId) name) | name <- reExportEnums]
            ++ concatMap (declToExport fileId) decls
        , modImports =
            commonImports ++ (guard usesRpc >> rpcImports)
        , modDecls =
            concatMap (declToDecls fileId) decls
            ++ concatMap (enumInstances fileId) reExportEnums
        }


-- | Convert a declaration into the list of related exports we need.
-- The first argument is the id for this module.
declToExport :: Word64 -> P.Decl -> [Export]
declToExport fileId = \case
    P.DataDecl P.Data{typeName} ->
        [ ExportLCtors typeName ]
    P.ConstDecl P.Constant { name, value=C.WordValue _ _ } ->
        [ ExportGName $ gName (rawModule fileId) name ]
    P.ConstDecl P.Constant { name, value=C.VoidValue } ->
        [ ExportGName $ gName (rawModule fileId) name ]
    P.ConstDecl P.Constant { name, value=C.PtrValue _ _ } ->
        [ ExportLName name ]
    P.IFaceDecl P.IFace{ name=Name.CapnpQ{local} } ->
        [ ExportLCtors local
        , ExportLCtors (Name.mkSub local "server_")
        , ExportLName (Name.unQToLocal $ Name.UnQ $ "export_" <> Name.renderLocalQ local)
        ]

-- | enumInstances' generates some type class instances an enum data type.
enumInstances :: Word64 -> Name.LocalQ -> [Decl]
enumInstances thisMod name =
    let rawName = gName (rawModule thisMod) name in
    instance_ [] ["Classes"] "Decerialize" [TGName rawName]
        [ iType "Cerial" [tuName "msg", TGName rawName] (TGName rawName)
        , iValue "decerialize" [] (eStd_ "pure")
        ]
    : instance_ [] ["Classes"] "Cerialize" [tuName "s", TGName rawName]
        [ iValue "cerialize" [PVar "_"] (eStd_ "pure")
        ]
    : [ instance_ [] ["Classes"] "Cerialize" [tuName "s", t]
        [ iValue "cerialize" [] (egName ["Classes"] "cerializeBasicVec")
        ]
      | t <- take 6 $ drop 1 $ iterate
            (\t -> TApp (tgName ["V"] "Vector") [t])
            (TGName rawName)
      ]

-- | Convert an 'IR.Pure.Decl' into a series of Haskell declarations.
declToDecls :: Word64 -> P.Decl -> [Decl]
declToDecls thisMod (P.DataDecl data_)     = dataToDecls thisMod data_
declToDecls thisMod (P.ConstDecl constant) = constToDecls thisMod constant
declToDecls thisMod (P.IFaceDecl iface)    = ifaceToDecls thisMod iface

dataToDecls :: Word64 -> P.Data -> [Decl]
dataToDecls thisMod data_@P.Data{firstClass} = concat $
    [ dataToDataDecl thisMod data_
    , dataToSimpleInstances thisMod data_
    , dataToDecerialize thisMod data_
    , dataToMarshal thisMod data_
    ]
    ++
    [ firstClassInstances thisMod data_ | firstClass ]

-- | Get the name of a data constructor for a product type. Arguments are the
-- name of the type constructor and whether or not the type is "first class".
productVariantName :: Name.LocalQ -> Bool -> Name.LocalQ
productVariantName typeName firstClass
    | firstClass = typeName
    | otherwise = Name.mkSub typeName ""
    -- ^ This is a group. If it's part of a union, then the union
    -- will have a data constructor that is the same as typeName,
    -- so we appent a ' to avoid a colision.

dataToDataDecl :: Word64 -> P.Data -> [Decl]
dataToDataDecl thisMod P.Data
        { typeName
        , typeParams
        , cerialName
        , firstClass
        , def
        } =
    let unknownCtor = Name.mkSub cerialName "unknown'" in
    [ DcData Data
        { dataName = Name.localToUnQ typeName
        , typeArgs = map (TVar . Name.typeVarName) typeParams
        , derives =
            ["Std_.Show", "Std_.Eq", "Generics.Generic"]
        , dataNewtype = False
        , dataVariants =
            case def of
                P.Product fields ->
                    [ DataVariant
                        { dvCtorName = Name.localToUnQ $ productVariantName typeName firstClass
                        , dvArgs = ARec (map (fieldToField thisMod) fields)
                        }
                    ]
                P.Sum variants ->
                    [ DataVariant
                        { dvCtorName = Name.localToUnQ name
                        , dvArgs = case arg of
                            Nothing -> APos []
                            Just ty -> APos [typeToType thisMod ty]
                        }
                    | P.Variant{name, arg} <- variants
                    ]
                    ++
                    [ DataVariant
                        { dvCtorName = Name.localToUnQ unknownCtor
                        , dvArgs = APos [TPrim $ C.PrimInt $ C.IntType C.Unsigned C.Sz16]
                        }
                    ]
        }
    ]

typeParamsDecerializeCtx :: [Name.UnQ] -> [Type]
typeParamsDecerializeCtx typeParams =
    let
        vars = typeParamVars typeParams
        constMsg = tgName ["Message"] "ConstMsg"
        cerial msg v = TApp (tgName ["Classes"] "Cerial") [msg, v]
    in
    concat
        [ [ TApp (tgName ["Classes"] "Decerialize") [v]
          , TApp
            (tgName ["Classes"] "FromPtr")
            [ constMsg
            , cerial constMsg v
            ]
          ]
        | v <- vars
        ]

typeParamsCerializeCtx :: [Name.UnQ] -> [Type]
typeParamsCerializeCtx typeParams =
    typeParamsDecerializeCtx typeParams ++ concat
        [ [ TApp (tgName ["Classes"] "Cerialize") [tuName "s", v]
          , TApp (tgName ["Classes"] "ToPtr")
               [ tuName "s"
               , TApp (tgName ["Classes"] "Cerial")
                   [ TApp (tgName ["Message"] "MutMsg") [tuName "s"]
                   , v
                   ]
               ]
           ]
        | v <- typeParamVars typeParams
        ]

dataToSimpleInstances :: Word64 -> P.Data -> [Decl]
dataToSimpleInstances _thisMod P.Data{ typeName, typeParams } =
    let typ = typeWithParams typeName typeParams
        ctx = typeParamsDecerializeCtx typeParams
    in
    [ instance_ ctx ["Default"] "Default" [typ]
        [ iValue "def" [] (egName ["GenHelpersPure"] "defaultStruct")
        ]
    , instance_ ctx ["Classes"] "FromStruct" [tgName ["Message"] "ConstMsg", typ]
        [ iValue "fromStruct" [PVar "struct"] $ EBind
            (EApp (egName ["Classes"] "fromStruct") [euName "struct"])
            (egName ["Classes"] "decerialize")
        ]
    ]

dataToDecerialize :: Word64 -> P.Data -> [Decl]
dataToDecerialize thisMod P.Data
        { typeName
        , typeParams
        , cerialName
        , firstClass
        , def
        } =
    let typ = typeWithParams typeName typeParams
        ctx = typeParamsDecerializeCtx typeParams
    in
    [ instance_ ctx ["Classes"] "Decerialize" [typ]
        [ iType "Cerial" [tuName "msg", typ] $
            TApp (tgName (rawModule thisMod) cerialName) $
                [ TApp (tgName ["Classes"] "Cerial") [tuName "msg", v]
                | v <- typeParamVars typeParams
                ]
                ++ [tuName "msg"]
        , iValue "decerialize" [PVar "raw"] $
            case def of
                P.Sum variants ->
                    variantsToDecerialize thisMod cerialName variants
                P.Product fields ->
                    fieldsToDecerialize thisMod typeName firstClass fields
        ]
    ]

variantsToDecerialize :: Word64 -> Name.LocalQ -> [P.Variant] -> Exp
variantsToDecerialize thisMod cerialName variants =
    let unknownCtor = Name.mkSub cerialName "unknown'"
        fieldGetter parentName name = egName
            (rawModule thisMod)
            (Name.unQToLocal $ Name.getterName $ Name.mkSub parentName name)
    in
    EDo
        [DoBind "raw" $ EApp (fieldGetter cerialName "") [euName "raw"]
        ]
        (ECase (ELName "raw") $
            [ case arg of
                Nothing ->
                    ( pgName (rawModule thisMod) name []
                    , EApp (eStd_ "pure") [ELName name]
                    )
                Just type_ | cerialEq type_ ->
                    ( pgName (rawModule thisMod) name [PVar "raw"]
                    , EApp (eStd_ "pure") [EApp (ELName name) [euName "raw"]]
                    )
                Just _ ->
                    ( pgName (rawModule thisMod) name [PVar "raw"]
                    , EFApp
                        (ELName name)
                        [EApp (egName ["Classes"] "decerialize") [euName "raw"]]
                    )
            | P.Variant{name, arg} <- variants
            ]
            ++
            [ ( pgName (rawModule thisMod) unknownCtor [PVar "tag"]
              , EApp
                  (eStd_ "pure")
                  [EApp (ELName unknownCtor) [euName "tag"]]
              )
            ]
        )

fieldsToDecerialize :: Word64 -> Name.LocalQ -> Bool -> [P.Field] -> Exp
fieldsToDecerialize thisMod typeName firstClass fields =
    let fieldGetter parentName name = egName
            (rawModule thisMod)
            (Name.unQToLocal $ Name.getterName $ Name.mkSub parentName name)
        variantName = productVariantName typeName firstClass
    in
    case fields of
        [] ->
            EApp (eStd_ "pure") [ELName variantName]
        _ ->
            EFApp
                (ELName variantName)
                [
                    let getter = EApp (fieldGetter typeName name) [euName "raw"] in
                    if name == "union'" then
                        -- unions decerialize from the same type as their parents. Don't
                        -- do anything but pass it off.
                        EApp (egName ["Classes"] "decerialize") [euName "raw"]
                    else if cerialEq type_ then
                        getter
                    else
                        EBind getter (egName ["Classes"] "decerialize")
                | P.Field{name, type_} <- fields
                ]


dataToMarshal :: Word64 -> P.Data -> [Decl]
dataToMarshal thisMod P.Data
        { typeName
        , typeParams
        , cerialName
        , firstClass
        , def
        } =
    let typ = typeWithParams typeName typeParams in
    [ instance_ (typeParamsCerializeCtx typeParams) ["Classes"] "Marshal" [TVar "s", typ]
        [ iValue "marshalInto" [PVar "raw_", PVar "value_"] $
            case def of
                P.Sum variants ->
                    variantsToMarshal thisMod cerialName variants
                P.Product fields ->
                    fieldsToMarshal thisMod typeName firstClass fields
        ]
    ]

variantsToMarshal :: Word64 -> Name.LocalQ -> [P.Variant] -> Exp
variantsToMarshal thisMod cerialName variants =
    let unknownCtor = Name.mkSub cerialName "unknown'" in
    ECase (euName "value_") $
        [ let setter = Name.unQToLocal $ Name.setterName variantName
              setExp = EApp (egName (rawModule thisMod) setter) [euName "raw_"]
          in case arg of
            Nothing ->
                ( PLCtor variantName []
                , setExp
                )
            Just type_ ->
                ( PLCtor variantName [PVar "arg_"]
                , marshalField MarshalField
                    { thisMod
                    , into = "raw_"
                    , localQField = variantName
                    , from = "arg_"
                    , type_
                    , inUnion = True
                    }
                )
        | P.Variant{name=variantName, arg} <- variants
        ]
        ++
        let setter = Name.unQToLocal $ Name.setterName unknownCtor in
        [ ( PLCtor unknownCtor [PVar "tag"]
          , EApp (egName (rawModule thisMod) setter) [euName "raw_", euName "tag"]
          )
        ]

fieldsToMarshal :: Word64 -> Name.LocalQ -> Bool -> [P.Field] -> Exp
fieldsToMarshal thisMod typeName firstClass fields =
    let variantName = productVariantName typeName firstClass in
    ECase (euName "value_")
        [ ( case fields of
              -- If there are no fields, use of RecordWildCards will
              -- cause an error:
              [] -> PLCtor variantName []
              _  -> PLRecordWildCard variantName
          , EDo
              [ DoE $ marshalField MarshalField
                  { thisMod
                  , into = "raw_"
                  , localQField = Name.mkSub typeName fieldName
                  , from = fieldName
                  , type_
                  , inUnion = False
                  }
              | P.Field{name=fieldName, type_} <- fields
              ]
              ePureUnit
          )
        ]

firstClassInstances :: Word64 -> P.Data -> [Decl]
firstClassInstances _thisMod P.Data{ typeName, typeParams } =
    let typ = typeWithParams typeName typeParams
        ctx = typeParamsCerializeCtx typeParams
    in
    [ instance_ ctx ["Classes"] "Cerialize" [tuName "s", typ] []
    , instance_ ctx ["Classes"] "Cerialize" [tuName "s", TApp (tgName ["V"] "Vector") [typ]]
        [ iValue "cerialize" [] (egName ["GenHelpersPure"] "cerializeCompositeVec")
        ]
    ] ++
    -- Generate instances of Cerialize s (Vector (Vector ... t)) up to some reasonable
    -- nesting level. I(zenhack) can't figure out how to get a general case
    -- Cerialize s (Vector a) => Cerialize s (Vector (Vector a)) to type check, so this
    -- will have to do for now.
    [ instance_ ctx ["Classes"] "Cerialize" [tuName "s", t]
        [ iValue "cerialize" [] (egName ["GenHelpersPure"] "cerializeBasicVec")
        ]
    | t <- take 6 $ drop 2 $ iterate
            (\t -> TApp (tgName ["V"] "Vector") [t])
            typ
    ]

ifaceToDecls :: Word64 -> P.Interface -> [Decl]
ifaceToDecls thisMod iface =
    [ ifaceClientDecl thisMod iface
    , ifaceClassDecl thisMod iface
    , ifaceExportFn thisMod iface
    ]
    ++
    ifaceInstances thisMod iface

-- | Declare the newtype wrapper for clients of this interface.
ifaceClientDecl :: Word64 -> P.Interface -> Decl
ifaceClientDecl _thisMod P.IFace{ name=Name.CapnpQ{local=name}, typeParams } =
    DcData Data
        { dataName = Name.localToUnQ name
        , dataNewtype = True
        , dataVariants =
            [ DataVariant
                { dvCtorName = Name.localToUnQ name
                , dvArgs = APos [tgName ["Message"] "Client"]
                }
            ]
        , typeArgs = map (TVar . Name.typeVarName . C.paramName) typeParams
        , derives =
            [ "Std_.Show"
            , "Std_.Eq"
            , "Generics.Generic"
            ]
        }

-- | define the *'server_ class for the interface.
ifaceClassDecl :: Word64 -> P.Interface -> Decl
ifaceClassDecl thisMod P.IFace { name=Name.CapnpQ{local=name}, methods, supers, typeParams } =
    let typeParamNames = map (Name.typeVarName . C.paramName) typeParams in
    DcClass
        { ctx =
            let superConstraints =
                    -- Add class constraints for superclasses:
                    [ let superClass = pureTName thisMod fileId (Name.mkSub local "server_")
                      in TApp superClass $
                            [tuName "m", tuName "cap"]
                            ++ map (typeToType thisMod . C.PtrType) types
                    | (P.IFace{name=Name.CapnpQ{local, fileId}}, C.ListBrand types) <- supers
                    ]
            in
            TApp (tgName ["MonadIO"] "MonadIO") [tuName "m"]
            : case superConstraints of
                -- If this interface doesn't have any superclasses, then we need to
                -- specify a constraint for the 'Server' class. Otherwise, it's
                -- implied by the supers.
                [] -> [ TApp (tgName ["Server"] "Server") [tuName "m", tuName "cap"] ]
                _ -> superConstraints
        , name = Name.mkSub name "server_"
        , params = map Name.UnQ $ ["m", "cap"] ++ typeParamNames
        , funDeps = [ ("cap", v) | v <- typeParamNames ]
        , decls =
            let mkName = mkMethodName name in
            CdMinimal [ mkName name | P.Method{name} <- methods ]
            : concat
                [ [ CdValueDecl
                        (mkName methodName)
                        (TFn
                            [ tuName "cap"
                            , TApp
                                (tgName ["Server"] "MethodHandler")
                                [ tuName "m"
                                , typeToType thisMod $ C.CompositeType paramType
                                , typeToType thisMod $ C.CompositeType resultType
                                ]
                            ]
                        )
                    , CdValueDef DfValue
                        { name = mkName methodName
                        , params = [PVar "_"]
                        , value = egName ["Server"] "methodUnimplemented"
                        }
                    ]
                | P.Method
                    { name=methodName
                    , paramType
                    , resultType
                    }
                <- methods
                ]
        }


-- | Define the export_Foo function for the interface.
ifaceExportFn :: Word64 -> P.Interface -> Decl
ifaceExportFn thisMod iface@P.IFace { name=Name.CapnpQ{ local }, typeParams, ancestors } =
    let typeParamVars = map (TVar . Name.typeVarName . C.paramName) typeParams in
    DcValue
        { typ = TCtx
            [ TApp (tgName ["STM"] "MonadSTM") [tuName "m"]
            , TApp
                (TLName (Name.mkSub local "server_"))
                $ [tStd_ "IO", tuName "cap"] ++ typeParamVars
            ]
            (TFn
                [ tgName ["Supervisors"] "Supervisor"
                , tuName "cap"
                , TApp (tuName "m") [TApp (TLName local) typeParamVars]
                ]
            )
        , def = DfValue
            { name = Name.UnQ $ "export_" <> Name.renderLocalQ local
            , params = [PVar "sup_", PVar "server_"]
            , value = EApp (egName ["STM"] "liftSTM") $ pure $ EFApp (ELName local)
                [ EApp (egName ["Rpc"] "export")
                    [ euName "sup_"
                    , ERecord (egName ["Server"] "ServerOps")
                        [ ( "handleCast"
                          , EApp (egName ["Server"] "unwrap") [ELName "server_"]
                          )
                        , ( "handleStop"
                          , EApp (egName ["Server"] "shutdown") [ELName "server_"]
                          )
                        , ( "handleCall"
                          , ELambda [PVar "interfaceId_", PVar "methodId_"] $
                                ECase (euName "interfaceId_") $
                                    [ ( PInt (fromIntegral interfaceId)
                                      , ECase (euName "methodId_") $
                                            [ ( PInt i
                                              , EApp
                                                    (egName ["Server"] "toUntypedHandler")
                                                    [ let methodName = mkMethodName ifaceName mname in
                                                      EApp
                                                        (if thisMod == ifaceMod
                                                            then euName methodName
                                                            else egName (pureModule ifaceMod) (Name.unQToLocal methodName)
                                                        )
                                                        [euName "server_"]
                                                    ]
                                              )
                                            | (i, P.Method{name=mname}) <- zip [0..] methods
                                            ]
                                            ++
                                            [ ( PVar "_"
                                              , egName ["Server"] "methodUnimplemented"
                                              )
                                            ]
                                      )
                                    | P.IFace
                                        { name = Name.CapnpQ{local=ifaceName, fileId=ifaceMod}
                                        , interfaceId
                                        , methods
                                        }
                                    <- iface : map fst ancestors
                                    ]
                                    ++
                                    [ ( PVar "_"
                                      , egName ["Server"] "methodUnimplemented"
                                      )
                                    ]
                          )
                        ]
                    ]
                ]
            }
        }

-- | Declare instances for clients for this interface.
ifaceInstances :: Word64 -> P.Interface -> [Decl]
ifaceInstances thisMod iface@P.IFace{ name=Name.CapnpQ{local=name}, typeParams } =
    let typ = typeWithParams name (map C.paramName typeParams) in
    [ instance_ [] ["Rpc"] "IsClient" [typ]
        [ iValue "fromClient" [] (ELName name)
        , iValue "toClient" [PLCtor name [PVar "client"]] (euName "client")
        ]
    , instance_ [] ["Classes"] "FromPtr" [tuName "msg", typ]
        [ iValue "fromPtr" [] (egName ["RpcHelpers"] "isClientFromPtr")
        ]
    , instance_ [] ["Classes"] "ToPtr" [tuName "s", typ]
        [ iValue "toPtr" [] (egName ["RpcHelpers"] "isClientToPtr")
        ]
    , instance_ [] ["Classes"] "Decerialize" [typ]
        [ iType "Cerial" [tuName "msg", typ] $
            TApp (tgName (rawModule thisMod) name) $
                [ TApp (tgName ["Classes"] "Cerial")
                    [ tuName "msg"
                    , TVar (Name.typeVarName (C.paramName t))
                    ]
                | t <- typeParams
                ]
                ++ [tuName "msg"]
        , iValue "decerialize"
            [ pgName (rawModule thisMod) (Name.mkSub name "newtype_") [PVar "maybeCap"]
            ]
            (ECase (euName "maybeCap")
                [ (PGCtor (std_ "Nothing") []
                  , EApp
                        (eStd_ "pure")
                        [ EApp (ELName name) [egName ["Message"] "nullClient"]
                        ]
                  )
                , (PGCtor (std_ "Just") [PVar "cap"]
                  , EFApp
                        (ELName name)
                        [ EApp (egName ["Untyped"] "getClient") [euName "cap"]]
                  )
                ]
            )
        ]
    , instance_ [] ["Classes"] "Cerialize" [tuName "s", typ]
        [ iValue "cerialize" [PVar "msg", PLCtor name [PVar "client"]] $
            EFApp
                (egName (rawModule thisMod) (Name.mkSub name "newtype_"))
                [ EFApp
                    (eStd_ "Just")
                    [EApp (egName ["Untyped"] "appendCap") [euName "msg", euName "client"]]
                ]
        ]
    ]
    ++
    ifaceServerInstances thisMod iface

-- | Generate a reference to a type defined in a pure module given:
--
-- * The id of the module we're in (thisMod)
-- * The id of the module in which the type is defined (targetMod)
-- * The local name of the type.
pureTName :: Word64 -> Word64 -> Name.LocalQ -> Type
pureTName thisMod targetMod local
    | thisMod == targetMod = TLName local
    | otherwise = tgName (pureModule targetMod) local

-- | Instance declarations for this interface's client for its *'server_ class
-- and those of its ancestors.
ifaceServerInstances :: Word64 -> P.Interface -> [Decl]
ifaceServerInstances thisMod iface@P.IFace{ name=Name.CapnpQ{local=name}, typeParams, ancestors } =
    DcInstance
        { ctx = []
        , typ = TApp (tgName ["Server"] "Server") [tStd_ "IO", typ]
        , defs = []
        }
    : map go ((iface, C.ListBrand (map C.PtrParam typeParams)):ancestors)
  where
    typ = typeWithParams name (map C.paramName typeParams)
    go (P.IFace { name=Name.CapnpQ{local, fileId}, interfaceId, methods }, C.ListBrand types) =
        let className = Name.mkSub local "server_"
            classType = pureTName thisMod fileId className
            classArgs = map (typeToType thisMod . C.PtrType) types
        in
        -- Can't use 'instance_' here, because we don't know ahead of time what
        -- module the class is in.
        DcInstance
            { ctx = []
            , typ = TApp classType $ [tStd_ "IO", typ] ++ classArgs
            , defs =
                [ let methodName = mkMethodName local mname in
                  iValue methodName [PLCtor name [PVar "client"]] $
                    EApp
                        (egName ["Rpc"] "clientMethodHandler")
                        [ EInt $ fromIntegral interfaceId
                        , EInt i
                        , euName "client"
                        ]
                | (i, P.Method{name=mname}) <- zip [0..] methods
                ]
            }

-- | Generate declarations for a constant.
constToDecls :: Word64 -> P.Constant -> [Decl]
constToDecls thisMod P.Constant { name, value } = case value of
    C.PtrValue ty _ ->
        -- Generated code parses the corresponding constant from the raw
        -- module.
        [ DcValue
            { typ = typeToType thisMod (C.PtrType ty)
            , def = DfValue
                { name = Name.localToUnQ name
                , params = []
                , value = EApp
                    (egName ["GenHelpersPure"] "toPurePtrConst")
                    [egName (rawModule thisMod) name]
                }
            }
        ]
    -- For these two we just re-export the constant from the raw module, so no
    -- need to do anything here:
    C.WordValue _ _ -> []
    C.VoidValue -> []

mkMethodName :: Name.LocalQ -> Name.UnQ -> Name.UnQ
mkMethodName typeName methodName = Name.valueName $ Name.mkSub typeName methodName

-- | Arguments to 'marshalField'
data MarshalField = MarshalField
    { thisMod     :: !Word64
    -- ^ The id for the module we're generating
    , into        :: Name.UnQ
    -- ^ An variable holding the destination for the marshalled data.
    , localQField :: Name.LocalQ
    -- ^ The name of the field to marshal, qualified within the current module.
    , from        :: Name.UnQ
    -- ^ A variable holding the value to marshal
    , type_       :: C.Type (C.ListBrand Name.CapnpQ) Name.CapnpQ
    , inUnion     :: !Bool
    -- ^ whether the parent of this field is a union
    }

marshalField :: MarshalField -> Exp
marshalField MarshalField{thisMod, into, localQField, from, type_, inUnion} =
    let setter = egName (rawModule thisMod) $ Name.unQToLocal (Name.setterName localQField)
        getter = egName (rawModule thisMod) $ Name.unQToLocal (Name.getterName localQField)
    in case type_ of
        C.PtrType _ ->
            EBind
                (EApp
                    (egName ["Classes"] "cerialize")
                    [ EApp (egName ["Untyped"] "message") [euName "raw_"]
                    , euName from
                    ]
                )
                (EApp setter [euName into])
        C.VoidType ->
            ePureUnit
        C.WordType _ ->
            EApp setter [euName into, euName from]
        C.CompositeType _ -> EDo
            (if Name.getUnQ localQField == "union'" then
                -- Anonymous union; has the same Cerial as us. Just delegate.
                []
            else if inUnion then
                [ DoBind into $ EApp setter [euName into] ]
            else
                [ DoBind into $ EApp getter [euName into] ]
            )
            (EApp (egName ["Classes"] "marshalInto") [euName into, euName from])

fieldToField :: Word64 -> P.Field -> (Name.UnQ, Type)
fieldToField thisMod P.Field{name, type_} = (name, typeToType thisMod type_)

typeToType :: Word64 -> C.Type (C.ListBrand Name.CapnpQ) Name.CapnpQ -> Type
typeToType _thisMod C.VoidType =
    TUnit
typeToType _thisMod (C.WordType (C.PrimWord ty)) =
    TPrim ty
typeToType thisMod (C.WordType (C.EnumType Name.CapnpQ{local, fileId})) =
    -- Enums are just re-exported from the raw module, so we have a choice as to
    -- how to refer to them. Using their 'Pure' module makes sure we pull in any
    -- type class instances in those modules, but we have to make an exception
    -- if the enum is defined in the file we're generating code for, since otherwise
    -- we'd introduce a cyclic dependency. In that case we use the raw name; the
    -- instnaces are defined in this same module so we don't need to worry about those.
    if thisMod == fileId then
        tgName (rawModule fileId) local
    else
        tgName (pureModule fileId) local
typeToType thisMod (C.CompositeType (C.StructType n b)) =
    nameToType thisMod n b
typeToType thisMod (C.PtrType (C.PtrComposite (C.StructType n b))) =
    nameToType thisMod n b
typeToType thisMod (C.PtrType (C.ListOf ty)) =
    TApp (tgName ["V"] "Vector") [typeToType thisMod ty]
typeToType _thisMod (C.PtrType (C.PrimPtr C.PrimText)) =
    tgName ["T"] "Text"
typeToType _thisMod (C.PtrType (C.PrimPtr C.PrimData)) =
    tgName ["BS"] "ByteString"
typeToType _thisMod (C.PtrType (C.PrimPtr (C.PrimAnyPtr _))) =
    -- TODO: distinguish different pointer types.
    TApp (tStd_ "Maybe") [tgName ["UntypedPure"] "Ptr"]
typeToType thisMod (C.PtrType (C.PtrInterface (C.InterfaceType n b))) =
    nameToType thisMod n b
typeToType _thisMod (C.PtrType (C.PtrParam C.TypeParamRef{paramName})) =
    TVar (Name.typeVarName paramName)

typeParamVars :: [Name.UnQ] -> [Type]
typeParamVars = map (TVar . Name.typeVarName)

typeWithParams :: Name.LocalQ -> [Name.UnQ] -> Type
typeWithParams f xs =
    TApp (TLName f) $ typeParamVars xs

nameToType :: Word64 -> Name.CapnpQ -> C.ListBrand Name.CapnpQ -> Type
nameToType thisMod Name.CapnpQ{local, fileId} (C.ListBrand args) =
    let head
            | thisMod == fileId = TLName local
            | otherwise = tgName (pureModule fileId) local
    in
    case args of
        [] -> head
        _  -> TApp head (map (typeToType thisMod . C.PtrType) args)

rawModule :: Word64 -> [T.Text]
rawModule modId =
    ["Capnp", "Gen", "ById", T.pack $ printf "X%x" modId]

pureModule :: Word64 -> [T.Text]
pureModule modId =
    rawModule modId ++ ["Pure"]
