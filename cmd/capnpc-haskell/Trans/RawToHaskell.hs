{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.RawToHaskell (fileToModules) where

import Data.Word

import Data.Maybe  (fromJust)
import Data.String (fromString)
import GHC.Exts    (fromList)

import qualified Data.Text as T

import IR.Haskell
import Trans.ToHaskellCommon

import qualified Capnp
import qualified Capnp.Untyped.Pure as Untyped

import qualified IR.Common as C
import qualified IR.Name   as Name
import qualified IR.Raw    as Raw

tMutMsg :: Type
tMutMsg = TApp (tgName ["Message"] "MutMsg" ) [TVar "s"]

tConstMsg :: Type
tConstMsg = tgName ["Message"] "ConstMsg"

fileToModules :: Raw.File -> [Module]
fileToModules file =
    [ fileToMainModule file
    , fileToModuleAlias file
    ]

fileToModuleAlias :: Raw.File -> Module
fileToModuleAlias Raw.File{fileName, fileId} =
    let reExport = ["Capnp", "Gen"] ++ makeModName fileName
    in Module
        { modName = idToModule fileId
        , modLangPragmas = []
        , modExports = Just [ExportMod reExport]
        , modImports = [ ImportAll { parts = reExport } ]
        , modDecls = []
        }

fileToMainModule :: Raw.File -> Module
fileToMainModule Raw.File{fileName, fileId, decls} =
    fixImports $ Module
        { modName = ["Capnp", "Gen"] ++ makeModName fileName
        , modLangPragmas =
            [ "FlexibleContexts"
            , "FlexibleInstances"
            , "MultiParamTypeClasses"
            , "TypeFamilies"
            , "DeriveGeneric"
            , "OverloadedStrings"
            ]
        , modExports = Nothing
        , modImports =
            [ imp ["Capnp", "Message"] "Message"
            , imp ["Capnp", "Untyped"] "Untyped"
            , imp ["Capnp", "Basics"] "Basics"
            , imp ["Capnp", "GenHelpers"] "GenHelpers"
            , imp ["Capnp", "Classes"] "Classes"
            , imp ["GHC", "Generics"] "Generics"

            -- So we can treat 'Word1' the same as 'Word16' etc.
            , imp ["Capnp", "Bits"] "Std_"

            , imp ["Data", "Maybe"] "Std_"
            , imp ["Capnp", "GenHelpers", "ReExports", "Data", "ByteString"] "BS"
            ]
        , modDecls = concatMap (declToDecls fileId) decls
        }
  where
    imp parts importAs = ImportAs {parts, importAs}

declToDecls :: Word64 -> Raw.Decl -> [Decl]
declToDecls thisMod Raw.UnionVariant{parentTypeCtor, typeParams, tagOffset, unionDataCtors} =
    let unknownCtor = Name.mkSub parentTypeCtor "unknown'"
        typeCtor = Name.mkSub parentTypeCtor ""
        containerTypeType = containerTypeToType typeCtor typeParams
    in
    [ DcData Data
        { dataName = Name.localToUnQ typeCtor
        , dataNewtype = False
        , typeArgs = [TVar "msg"]
        , dataVariants =
            [ DataVariant
                { dvCtorName = Name.localToUnQ dataCtor
                , dvArgs = APos $
                    case locType of
                        C.VoidField ->
                            []
                        _ ->
                            [ typeToType thisMod (C.fieldType locType) (TVar "msg") ]
                }
            | Raw.Variant{name=dataCtor, locType} <- unionDataCtors
            ]
            ++
            [ DataVariant
                { dvCtorName = Name.localToUnQ unknownCtor
                , dvArgs = APos [ TPrim $ C.PrimInt $ C.IntType C.Unsigned C.Sz16 ]
                }
            ]
        , derives = []
        }
    , instance_ [] ["Classes"] "FromStruct" [TVar "msg", containerTypeType (TVar "msg")]
        [ iValue "fromStruct" [PVar "struct"] $ EDo
            [ DoBind "tag"
                (EApp
                    (egName ["GenHelpers"] "getTag")
                    [ ELName "struct"
                    , EInt (fromIntegral tagOffset)
                    ]
                )
            ]
            (ECase (ELName "tag") $
                [ ( PInt $ fromIntegral tagValue
                  , case locType of
                        C.VoidField ->
                            EApp (eStd_ "pure") [ELName name]
                        C.HereField _ ->
                            EFApp
                                (ELName name)
                                [ EApp
                                    (egName ["Classes"] "fromStruct")
                                    [ELName "struct"]
                                ]
                        C.DataField loc _ ->
                            EFApp
                                (ELName name)
                                [eGetWordField (ELName "struct") loc]
                        C.PtrField idx _ ->
                            EFApp
                                (ELName name)
                                [ EDo
                                    [DoBind "ptr" $ EApp
                                        (egName ["Untyped"] "getPtr")
                                        [ EInt $ fromIntegral idx
                                        , ELName "struct"
                                        ]
                                    ]
                                    (EApp
                                        (egName ["Classes"] "fromPtr")
                                        [ EApp
                                            (egName ["Untyped"] "message")
                                            [ELName "struct"]
                                        , ELName "ptr"
                                        ]
                                    )
                                ]
                  )
                | Raw.Variant{name, tagValue, locType} <- unionDataCtors
                ] ++
                [ ( PVar "_"
                  , EApp
                        (eStd_ "pure")
                        [ EApp
                            (ELName unknownCtor)
                            [ EApp
                                (eStd_ "fromIntegral")
                                [ELName "tag"]
                            ]
                        ]
                  )
                ]
            )
        ]
    ]
declToDecls _thisMod Raw.Enum{typeCtor, dataCtors} =
    let listCtor = Name.mkSub typeCtor "List_"
        unknownCtor = Name.mkSub typeCtor "unknown'"
    in
    [ DcData Data
        { dataName = Name.localToUnQ typeCtor
        , dataNewtype = False
        , typeArgs = []
        , dataVariants =
            map enumerantToVariant dataCtors
            ++
            [ DataVariant
                { dvCtorName = Name.localToUnQ unknownCtor
                , dvArgs = APos
                    [ TPrim $ C.PrimInt $ C.IntType C.Unsigned C.Sz16 ]
                }
            ]
        , derives =
            [ "Std_.Show"
            , "Std_.Read"
            , "Std_.Eq"
            , "Generics.Generic"
            ]
        }
    , mkIsWordInstance typeCtor dataCtors unknownCtor
    -- An Enum instance that just wraps the IsWord instance:
    , instance_ [] ["Std_"] "Enum" [TLName typeCtor]
        [ iValue "fromEnum" [PVar "x"] $
            EApp
                (eStd_ "fromIntegral")
                [ EApp
                    (egName ["Classes"] "toWord")
                    [ELName "x"]
                ]
        , iValue "toEnum" [PVar "x"] $
            EApp
                (egName ["Classes"] "fromWord")
                [ EApp
                    (eStd_ "fromIntegral")
                    [ELName "x"]
                ]
        ]
    -- lists:
    , instance_ [] ["Basics"] "ListElem" [TVar "msg", TLName typeCtor]
        [ IdData Data
            { dataName = "List"
            , typeArgs =
                [ TVar "msg"
                , TLName typeCtor
                ]
            , dataVariants =
                [ DataVariant
                    { dvCtorName = Name.localToUnQ listCtor
                    , dvArgs = APos
                        [ TApp
                            (tgName ["Untyped"] "ListOf")
                            [ TVar "msg"
                            , tStd_ "Word16"
                            ]
                        ]
                    }
                ]
            , derives = []
            , dataNewtype = True
            }
        , iValue "index" [PVar "i", PLCtor listCtor [PVar "l"]] $ EFApp
            (egName ["Classes"] "fromWord")
            [ EFApp
                (eStd_ "fromIntegral")
                [ EApp
                    (egName ["Untyped"] "index")
                    [ ELName "i"
                    , ELName "l"
                    ]
                ]
            ]
        , iValue "listFromPtr" [PVar "msg", PVar "ptr"] $ EFApp
            (ELName listCtor)
            [ EApp
                (egName ["Classes"] "fromPtr")
                [ ELName "msg"
                , ELName "ptr"
                ]
            ]
        , iValue "toUntypedList" [PLCtor listCtor [PVar "l"]] $ EApp
            (egName ["Untyped"] "List16")
            [ELName "l"]
        , iValue "length" [PLCtor listCtor [PVar "l"]] $ EApp
            (egName ["Untyped"] "length")
            [ELName "l"]
        ]
    , instance_ [] ["Classes"] "MutListElem" [TVar "s", TLName typeCtor]
        [ iValue
            "setIndex"
            [PVar "elt", PVar "i", PLCtor listCtor [PVar "l"]] $
            EApp
                (egName ["Untyped"] "setIndex")
                [ EApp
                    (eStd_ "fromIntegral")
                    [ EApp (egName ["Classes"] "toWord") [ELName "elt"]
                    ]
                , ELName "i"
                , ELName "l"
                ]
        , iValue "newList" [PVar "msg", PVar "size"] $ EFApp
            (ELName listCtor)
            [ EApp
                (egName ["Untyped"] "allocList16")
                [ ELName "msg"
                , ELName "size"
                ]
            ]
        ]
    ]
  where
    enumerantToVariant variantName =
        DataVariant
            { dvCtorName =
                Name.localToUnQ variantName
            , dvArgs = APos []
            }
declToDecls _thisMod Raw.InterfaceWrapper{typeCtor, typeParams} =
    let dataCtor = Name.mkSub typeCtor "newtype_" in
    [ newtypeWrapper typeCtor (map Name.typeVarName typeParams ++ ["msg"]) $ TApp
        (tStd_ "Maybe")
        [ TApp
            (tgName ["Untyped"] "Cap")
            [TVar "msg"]
        ]
    , wrapperFromPtr typeCtor typeParams dataCtor
    , instance_ [] ["Classes"] "ToPtr"
        [ TVar "s"
        , containerTypeToType typeCtor typeParams (TApp (tgName ["Message"] "MutMsg") [TVar "s"])
        ]
        [ iValue "toPtr" [PVar "msg", PLCtor dataCtor [PGCtor (std_ "Nothing") []]]
            (EApp (eStd_ "pure") [eStd_ "Nothing"])
        , iValue "toPtr" [PVar "msg", PLCtor dataCtor [PGCtor (std_ "Just") [PVar "cap"]]]
            (EApp
                (eStd_ "pure")
                [ EApp
                    (eStd_ "Just")
                    [ EApp
                        (egName ["Untyped"] "PtrCap")
                        [ELName "cap"]
                    ]
                ]
            )
        ]
    ]
declToDecls _thisMod Raw.StructWrapper{typeCtor, typeParams} =
    let dataCtor = Name.mkSub typeCtor "newtype_"
        containerTypeType = containerTypeToType typeCtor typeParams
    in
    [ newtypeWrapper typeCtor (map Name.typeVarName typeParams ++ ["msg"]) $ TApp
        (tgName ["Untyped"] "Struct")
        [TVar "msg"]

    -- There are several type classes that are defined for all structs:
    , instance_ [] ["Classes"] "FromStruct" [TVar "msg", containerTypeType (TVar "msg")]
        [ iValue "fromStruct" [PVar "struct"] $ EApp
            (eStd_ "pure")
            [EApp (ELName dataCtor) [ELName "struct"]]
        ]
    , instance_ [] ["Classes"] "ToStruct" [TVar "msg", containerTypeType (TVar "msg")]
        [ iValue "toStruct" [PLCtor dataCtor [PVar "struct"]]
            (ELName "struct")
        ]
    , instance_ [] ["Untyped"] "HasMessage" [containerTypeType (TVar "msg")]
        [ IdType $ TypeAlias
            "InMessage"
            [ containerTypeType (TVar "msg") ]
            (TVar "msg")
        , iValue "message" [PLCtor dataCtor [PVar "struct"]]
            (EApp (egName ["Untyped"] "message") [ELName "struct"])
        ]
    , instance_ [] ["Untyped"] "MessageDefault" [containerTypeType (TVar "msg")]
        [ iValue "messageDefault" [PVar "msg"] $ EApp
            (ELName dataCtor)
            [ EApp
                (egName ["Untyped"] "messageDefault")
                [ELName "msg"]
            ]
        ]
    ]
declToDecls _thisMod Raw.StructInstances{typeCtor, typeParams, dataWordCount, pointerCount} =
    let listCtor = Name.mkSub typeCtor "List_"
        dataCtor = Name.mkSub typeCtor "newtype_"
        containerTypeType = containerTypeToType typeCtor typeParams
    in
    [ wrapperFromPtr typeCtor typeParams dataCtor
    , instance_ [] ["Classes"] "ToPtr"
        [ TVar "s"
        , containerTypeType $ TApp (tgName ["Message"] "MutMsg") [TVar "s"]
        ]
        [ iValue
            "toPtr"
            [PVar "msg", PLCtor dataCtor [PVar "struct"]]
            (EApp
                (egName ["Classes"] "toPtr")
                [ ELName "msg"
                , ELName "struct"
                ]
            )
        ]
    , instance_ [] ["Classes"] "Allocate"
        [ TVar "s"
        , containerTypeType $ TApp (tgName ["Message"] "MutMsg") [TVar "s"]
        ]
        [ iValue "new" [PVar "msg"] $ EFApp
            (ELName dataCtor)
            [ EApp
                (egName ["Untyped"] "allocStruct")
                [ ELName "msg"
                , EInt $ fromIntegral dataWordCount
                , EInt $ fromIntegral pointerCount
                ]
            ]
        ]
    , instance_ [] ["Basics"] "ListElem"
        [ TVar "msg"
        , containerTypeType $ TVar "msg"
        ]
        [ IdData Data
            { dataName = "List"
            , typeArgs =
                [ TVar "msg"
                , containerTypeType (TVar "msg")
                ]
            , dataVariants =
                [ DataVariant
                    { dvCtorName = Name.localToUnQ listCtor
                    , dvArgs = APos
                        [ TApp
                            (tgName ["Untyped"] "ListOf")
                            [ TVar "msg"
                            , TApp
                                (tgName ["Untyped"] "Struct")
                                [TVar "msg"]
                            ]
                        ]
                    }
                ]
            , derives = []
            , dataNewtype = True
            }
        , iValue "listFromPtr" [PVar "msg", PVar "ptr"] $ EFApp
            (ELName listCtor)
            [ EApp
                (egName ["Classes"] "fromPtr")
                [ELName "msg", ELName "ptr"]
            ]
        , iValue "toUntypedList" [PLCtor listCtor [PVar "l"]] $ EApp
            (egName ["Untyped"] "ListStruct")
            [ELName "l"]
        , iValue "length" [PLCtor listCtor [PVar "l"]] $ EApp
            (egName ["Untyped"] "length")
            [ELName "l"]
        , iValue "index" [PVar "i", PLCtor listCtor [PVar "l"]] $ EDo
            [ DoBind "elt" $ EApp
                (egName ["Untyped"] "index")
                [ ELName "i"
                , ELName "l"
                ]
            ]
            ( EApp
                (egName ["Classes"] "fromStruct")
                [ELName "elt"]
            )
        ]
    , instance_ [] ["Basics"] "MutListElem"
        [ TVar "s"
        , containerTypeType $ TApp (tgName ["Message"] "MutMsg") [TVar "s"]
        ]
        [ iValue "setIndex"
            [ PLCtor dataCtor [PVar "elt"]
            , PVar "i"
            , PLCtor listCtor [PVar "l"]
            ]
            (EApp
                (egName ["Untyped"] "setIndex")
                [ ELName "elt"
                , ELName "i"
                , ELName "l"
                ]
            )
        , iValue "newList" [PVar "msg", PVar "len"] $ EFApp
            (ELName listCtor)
            [ EApp
                (egName ["Untyped"] "allocCompositeList")
                [ ELName "msg"
                , EInt $ fromIntegral dataWordCount
                , EInt $ fromIntegral pointerCount
                , ELName "len"
                ]
            ]
        ]
    ]
declToDecls thisMod Raw.Getter{fieldName, fieldLocType, containerType, typeParams} =
    let containerDataCtor = Name.mkSub containerType "newtype_"
        fieldType = typeToType thisMod (C.fieldType fieldLocType) (TVar "msg")
        isPtrType = case C.fieldType fieldLocType of
            C.PtrType _       -> True
            C.CompositeType _ -> True
            _                 -> False
    in
    [ DcValue
        { typ = TCtx
            (readCtx "m" "msg"
                : if isPtrType
                    then
                        [ case fieldLocType of
                            C.HereField _ ->
                                TApp (tgName ["Classes"] "FromStruct") [TVar "msg", fieldType]
                            _ ->
                                TApp (tgName ["Classes"] "FromPtr") [TVar "msg", fieldType]
                        ]
                    else []
            )
            (TFn
                [ containerTypeToType containerType typeParams (TVar "msg")
                , TApp (TVar "m") [fieldType]
                ]
            )
        , def = DfValue
            { name = Name.getterName fieldName
            , params = [PLCtor containerDataCtor [PVar "struct"]]
            , value = case fieldLocType of
                C.DataField dataLoc  _ ->
                    eGetWordField (ELName "struct") dataLoc
                C.PtrField idx _ -> EDo
                    [ DoBind "ptr" $ EApp
                        (egName ["Untyped"] "getPtr")
                        [ EInt (fromIntegral idx)
                        , ELName "struct"
                        ]
                    ]
                    (EApp
                        (egName ["Classes"] "fromPtr")
                        [ EApp
                            (egName ["Untyped"] "message")
                            [ELName "struct"]
                        , ELName "ptr"
                        ]
                    )
                C.HereField _ ->
                    EApp
                        (egName ["Classes"] "fromStruct")
                        [ELName "struct"]
                C.VoidField ->
                    EApp
                        (eStd_ "pure")
                        [ETup []]
            }
        }
    ]
declToDecls thisMod Raw.Setter{fieldName, fieldLocType, containerType, typeParams, tag} =
    -- XXX: the way this is organized is a little gross; conceptually we have
    -- two kinds of setters:
    --
    -- * Those that actually take an argument, and return unit.
    -- * Those that are groups, in which case they don't take an argument,
    --   and return the value.
    --
    -- The latter are only actually useful if the group is a union member,
    -- but we generate them regardless.
    --
    -- The below is a little ugly in that there's a bit to much conditional
    -- logic strewn about because of the above.
    let containerDataCtor = Name.mkSub containerType "newtype_"
        fieldType = typeToType
            thisMod
            (C.fieldType fieldLocType)
            tMutMsg
        isPtrType = case C.fieldType fieldLocType of
            C.PtrType _       -> True
            C.CompositeType _ -> True
            _                 -> False
        containerTypeType = containerTypeToType containerType typeParams tMutMsg
    in
    [ DcValue
        { typ = TCtx
            (rwCtx "m" "s"
                : if isPtrType
                    then
                        [ case fieldLocType of
                            C.HereField _ ->
                                TApp (tgName ["Classes"] "FromStruct") [tMutMsg, fieldType]
                            _ ->
                                TApp (tgName ["Classes"] "ToPtr") [TVar "s", fieldType]
                        ]
                    else
                        []
            )
            (case fieldLocType of
                C.HereField _ -> TFn
                    [ containerTypeType
                    , TApp (TVar "m") [fieldType]
                    ]
                C.VoidField -> TFn
                    -- We don't need to pass in the redundant () value.
                    [ containerTypeType
                    , TApp (TVar "m") [fieldType]
                    ]
                _ -> TFn
                    [ containerTypeType
                    , fieldType
                    , TApp (TVar "m") [TUnit]
                    ]
            )
        , def = DfValue
            { name = Name.setterName fieldName
            , params =
                case fieldLocType of
                    C.HereField _ ->
                        [ PLCtor containerDataCtor [PVar "struct"] ]
                    C.VoidField ->
                        [ PLCtor containerDataCtor [PVar "struct"] ]
                    _ ->
                        [ PLCtor containerDataCtor [PVar "struct"]
                        , PVar "value"
                        ]
            , value =
                case tag of
                    Just tagSetter ->
                        EDo
                            [DoE $ eSetTag tagSetter]
                            (eSetValue fieldLocType)
                    Nothing ->
                        eSetValue fieldLocType
            }
        }
    ]
declToDecls _thisMod Raw.HasFn{fieldName, containerType, typeParams, ptrIndex} =
    let containerDataCtor = Name.mkSub containerType "newtype_"
        containerTypeType = containerTypeToType containerType typeParams (TVar "msg")
    in
    [ DcValue
        { typ = TCtx
            [readCtx "m" "msg"]
            (TFn
                [ containerTypeType
                , TApp (TVar "m") [tStd_ "Bool"]
                ]
            )
        , def = DfValue
            { name = Name.hasFnName fieldName
            , params = [PLCtor containerDataCtor [PVar "struct"]]
            , value = EFApp (eStd_ "isJust")
                [ EApp
                    (egName ["Untyped"] "getPtr")
                    [ EInt $ fromIntegral ptrIndex
                    , ELName "struct"
                    ]
                ]
            }
        }
    ]
declToDecls thisMod Raw.NewFn{fieldName, containerType, typeParams, fieldLocType, newFnType} =
    -- TODO(cleanup): I(zenhack) am a little unhappy with having several case
    -- expressions to distinguish struct vs non-struct; we should refactor.
    let fieldType = typeToType
            thisMod
            (C.fieldType fieldLocType)
            tMutMsg
        containerTypeType = containerTypeToType containerType typeParams tMutMsg
    in
    [ DcValue
        { typ = TCtx
            [rwCtx "m" "s"]
            (TFn $ concat
                [ case newFnType of
                    -- length
                    Raw.NewStruct -> []
                    _             -> [tStd_ "Int"]
                , [containerTypeType]
                , [TApp (TVar "m") [fieldType]]
                ]
            )
        , def = DfValue
            { name = Name.newFnName fieldName
            , params =
                case newFnType of
                    Raw.NewStruct -> [PVar "struct"]
                    _             -> [PVar "len", PVar "struct"]
            , value = EDo
                [ DoBind "result" $
                    let message = EApp (egName ["Untyped"] "message") [ELName "struct"]
                    in case newFnType of
                        Raw.NewStruct -> EApp (egName ["Classes"] "new") [message]
                        Raw.NewList -> EApp (egName ["Classes"] "newList") [message, ELName "len"]
                        Raw.NewText -> EApp (egName ["Basics"] "newText") [message, ELName "len"]
                        Raw.NewData -> EApp (egName ["Basics"] "newData") [message, ELName "len"]
                , DoE $ EApp
                    (ELName $ Name.mkLocal Name.emptyNS (Name.setterName fieldName))
                    [ ELName "struct"
                    , ELName "result"
                    ]
                ]
                (EApp (eStd_ "pure") [ELName "result"])
            }
        }
    ]
declToDecls _thisMod Raw.Constant{ name, value=C.VoidValue } =
    [ DcValue
        { typ = TUnit
        , def = DfValue
            { name = Name.localToUnQ name
            , params = []
            , value = ETup []
            }
        }
    ]
declToDecls thisMod Raw.Constant{ name, value=C.WordValue ty val } =
    [ DcValue
        { typ = typeToType thisMod (C.WordType ty) tConstMsg
        , def = DfValue
            { name = Name.localToUnQ name
            , params = []
            , value = EApp
                (egName ["Classes"] "fromWord")
                [EInt $ fromIntegral val]
            }
        }
    ]
declToDecls thisMod Raw.Constant{ name, value=C.PtrValue ty val } =
    [ DcValue
        { typ = typeToType thisMod (C.PtrType ty) tConstMsg
        , def = DfValue
            { name = Name.localToUnQ name
            , params = []
            , value = EApp
                (egName ["GenHelpers"] "getPtrConst")
                [ETypeAnno (EBytes (makePtrBytes val)) (tgName ["BS"] "ByteString")]
            }
        }
    ]
  where
    makePtrBytes ptr =
        Capnp.msgToLBS $ fromJust $ Capnp.createPure Capnp.defaultLimit $ do
            msg <- Capnp.newMessage Nothing
            rootPtr <- Capnp.cerialize msg $ Untyped.Struct
                (fromList [])
                (fromList [ptr])
            Capnp.setRoot rootPtr
            pure msg

containerTypeToType :: Name.LocalQ -> [Name.UnQ] -> Type -> Type
containerTypeToType name params msgTy =
    TApp (TLName name) $ [ TApp (TVar (Name.typeVarName p)) [msgTy] | p <- params ] ++ [msgTy]


eSetValue :: C.FieldLocType (C.ListBrand Name.CapnpQ) Name.CapnpQ -> Exp
eSetValue = \case
    C.DataField dataLoc ty ->
        eSetWordField
            (ELName "struct")
            (ETypeAnno
                (EApp
                    (eStd_ "fromIntegral")
                    [EApp
                        (egName ["Classes"] "toWord")
                        [ELName "value"]
                    ]
                )
                (tStd_ $ fromString $ "Word" <> show (C.dataFieldSize ty))
            )
            dataLoc
    C.PtrField idx _ -> EDo
        [ DoBind "ptr" $ EApp
            (egName ["Classes"] "toPtr")
            [ EApp
                (egName ["Untyped"] "message")
                [ELName "struct"]
            , ELName "value"
            ]
        ]
        (EApp
            (egName ["Untyped"] "setPtr")
            [ ELName "ptr"
            , EInt (fromIntegral idx)
            , ELName "struct"
            ]
        )
    C.HereField _ ->
        -- We actually just fetch the field in this case; this only happens for
        -- groups (and unions, but the tag is handled separately in that case).
        EApp
            (egName ["Classes"] "fromStruct")
            [ELName "struct"]
    C.VoidField ->
        EApp
            (eStd_ "pure")
            [ETup []]

eSetTag :: Raw.TagSetter -> Exp
eSetTag Raw.TagSetter{tagOffset, tagValue} =
    eSetWordField
        (ELName "struct")
        (ETypeAnno
            (EInt $ fromIntegral tagValue)
            (TPrim $ C.PrimInt $ C.IntType C.Unsigned C.Sz16)
        )
        (Raw.tagOffsetToDataLoc tagOffset)

eSetWordField :: Exp -> Exp -> C.DataLoc -> Exp
eSetWordField struct value C.DataLoc{dataIdx, dataOff, dataDef} =
    EApp
        (egName ["GenHelpers"] "setWordField")
        [ struct
        , value
        , EInt $ fromIntegral dataIdx
        , EInt $ fromIntegral dataOff
        , EInt $ fromIntegral dataDef
        ]

-- | Make an instance of the IsWord type class for an enum.
mkIsWordInstance :: Name.LocalQ -> [Name.LocalQ] -> Name.LocalQ -> Decl
mkIsWordInstance typeCtor dataCtors unknownCtor =
    instance_ [] ["Classes"] "IsWord" [TLName typeCtor] $
        [ iValue "fromWord" [PVar "n"] $
            ECase
                (ETypeAnno
                    (EApp (eStd_ "fromIntegral") [euName "n"])
                    (TPrim $ C.PrimInt $ C.IntType C.Unsigned C.Sz16)
                ) $
                [ (PInt i, ELName ctor) | (i, ctor) <- zip [0..] dataCtors]
                ++
                [ (PVar "tag", EApp (ELName unknownCtor) [euName "tag"]) ]
        ] ++
        [ IdValue $ DfValue
            { name = "toWord"
            , params = [PLCtor ctor []]
            , value = EInt i
            }
        | (i, ctor) <- zip [0..] dataCtors
        ] ++
        [ IdValue $ DfValue
            { name = "toWord"
            , params =
                [ PLCtor unknownCtor [PVar "tag"] ]
            , value =
                EApp
                    (eStd_ "fromIntegral")
                    [ELName "tag"]
            }
        ]

newtypeWrapper :: Name.LocalQ -> [T.Text] -> Type -> Decl
newtypeWrapper typeCtor typeArgs wrappedType =
    DcData Data
        { dataName = Name.localToUnQ typeCtor
        , dataNewtype = True
        , typeArgs = map TVar typeArgs
        , dataVariants =
            [ DataVariant
                { dvCtorName = Name.localToUnQ $ Name.mkSub typeCtor "newtype_"
                , dvArgs = APos [ wrappedType ]
                }
            ]
        , derives = []
        }

wrapperFromPtr :: Name.LocalQ -> [Name.UnQ] -> Name.LocalQ -> Decl
wrapperFromPtr typeCtor typeParams dataCtor =
    instance_ [] ["Classes"] "FromPtr"
        [ TVar "msg", containerTypeToType typeCtor typeParams (TVar "msg") ]
        [ iValue "fromPtr" [PVar "msg", PVar "ptr"] $ EFApp
            (ELName dataCtor)
            [ EApp
                (egName ["Classes"] "fromPtr")
                [ ELName "msg"
                , ELName "ptr"
                ]
            ]
        ]

nameToType :: Word64 -> Name.CapnpQ -> Type
nameToType thisMod Name.CapnpQ{local, fileId} =
    if fileId == thisMod
        then TLName local
        else tgName
            (map Name.renderUnQ $ idToModule fileId)
            local

typeToType :: Word64 -> C.Type (C.ListBrand Name.CapnpQ) Name.CapnpQ -> Type -> Type
typeToType thisMod ty msgTy = case ty of
    C.VoidType -> TUnit
    C.WordType (C.PrimWord ty) -> TPrim ty
    C.WordType (C.EnumType typeId) -> nameToType thisMod typeId
    C.PtrType (C.ListOf elt) ->
        TApp (tgName ["Basics"] "List")
            [ msgTy
            , typeToType thisMod elt msgTy
            ]
    C.PtrType (C.PrimPtr C.PrimText) ->
        appV $ tgName ["Basics"] "Text"
    C.PtrType (C.PrimPtr C.PrimData) ->
        appV $ tgName ["Basics"] "Data"
    C.PtrType (C.PtrComposite (C.StructType typeId typeParams)) ->
        namedType typeId typeParams
    C.PtrType (C.PtrInterface typeId typeParams) ->
        namedType typeId typeParams
    C.PtrType (C.PrimPtr (C.PrimAnyPtr _)) ->
        TApp (tStd_ "Maybe") [appV $ tgName ["Untyped"] "Ptr"]
    C.PtrType (C.PtrParam C.TypeParamRef{paramName}) ->
        appV $ TVar (Name.typeVarName paramName)
    C.CompositeType (C.StructType typeId typeParams) ->
        namedType typeId typeParams
  where
    appV t = TApp t [msgTy]
    namedType :: Name.CapnpQ -> C.ListBrand Name.CapnpQ -> Type
    namedType name (C.ListBrand [])     = appV $ nameToType thisMod name
    namedType name (C.ListBrand args) =
        TApp
            (nameToType thisMod name)
            (map (\t -> typeToType thisMod (C.PtrType t) msgTy) args ++ [msgTy])
