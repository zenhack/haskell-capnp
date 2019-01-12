{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.PureToHaskell (fileToModules) where

import Data.Word

import Text.Printf (printf)

import qualified Data.Text as T

import IR.Haskell
import Trans.ToHaskellCommon

import qualified IR.Common as C
import qualified IR.Name   as Name
import qualified IR.Pure   as P

-- | Whether the serialized and unserialized forms of this type
-- are the same. If not, there is a marshalling step, if so, the
-- accessors work with the decerialized form directly.
--
-- For example, this is True for Enums, basic integers, etc. but
-- False for structs, interfaces, etc.
cerialEq :: C.Type r -> Bool
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
fileToMainModule P.File{fileName, fileId, decls, fileImports, reExportEnums, usesRpc} = Module
    { modName = ["Capnp", "Gen"] ++ makeModName fileName ++ ["Pure"]
    , modLangPragmas =
        [ "DeriveGeneric"
        , "DuplicateRecordFields"
        , "FlexibleInstances"
        , "RecordWildCards"
        , "MultiParamTypeClasses"
        , "TypeFamilies"
        ]
    , modExports = Just $
        [ExportGCtors (gName (rawModule fileId) name) | name <- reExportEnums]
        ++
        [ case decl of
            P.Data{typeName} ->
                ExportLCtors typeName
            P.Constant { name, value=C.WordValue _ _ } ->
                ExportGName $ gName (rawModule fileId) name
            P.Constant { name, value=C.VoidValue } ->
                ExportGName $ gName (rawModule fileId) name
            P.Constant { name, value=C.PtrValue _ _ } ->
                ExportLName name
            P.Interface { name } ->
                ExportLCtors name
        | decl <- decls
        ]
    , modImports = concat $
        [ ImportAs { importAs = "V", parts = ["Data", "Vector"] }
        , ImportAs { importAs = "T", parts = ["Data", "Text"] }
        , ImportAs { importAs = "BS", parts = ["Data", "ByteString"] }
        , ImportAs { importAs = "Default", parts = ["Data", "Default"] }
        , ImportAs { importAs = "Generics", parts = ["GHC", "Generics"] }
        , ImportAs { importAs = "MonadIO", parts = ["Control", "Monad", "IO", "Class"] }
        , ImportAs { importAs = "UntypedPure", parts = ["Capnp", "Untyped", "Pure"] }
        , ImportAs { importAs = "Untyped", parts = ["Capnp", "Untyped"] }
        , ImportAs { importAs = "Message", parts = ["Capnp", "Message"] }
        , ImportAs { importAs = "Classes", parts = ["Capnp", "Classes"] }
        , ImportAs { importAs = "BasicsPure", parts = ["Capnp", "Basics", "Pure" ] }
        , ImportAs { importAs = "GenHelpersPure", parts = ["Capnp", "GenHelpers", "Pure"] }
        , ImportQual { parts = idToModule fileId }
        ]
        : (if usesRpc
            then
                [ ImportAs { importAs = "Rpc", parts = ["Capnp", "Rpc", "Untyped"] }
                , ImportAs { importAs = "RpcHelpers", parts = ["Capnp", "GenHelpers", "Rpc"] }
                ]
            else
                [])
        :
        [ [ ImportQual { parts = impId }
          , ImportQual { parts = impId ++ ["Pure"] }
          ]
        | impId <- map idToModule fileImports
        ]
    , modDecls = concatMap (declToDecls fileId) decls
    }

declToDecls :: Word64 -> P.Decl -> [Decl]
declToDecls thisMod P.Data
        { typeName
        , cerialName
        , variants
        , isUnion
        , firstClass
        } =
    let unknownCtor = Name.mkSub cerialName "unknown'" in
    [ DcData Data
        { dataName = Name.localToUnQ typeName
        , typeArgs = []
        , derives =
            ["Std_.Show", "Std_.Eq", "Generics.Generic"]
        , dataNewtype = False
        , dataVariants =
            [ DataVariant
                { dvCtorName = Name.localToUnQ name
                , dvArgs = case arg of
                    P.None          -> APos []
                    P.Positional ty -> APos [typeToType thisMod ty]
                    P.Record fields -> ARec (map (fieldToField thisMod) fields)
                }
            | P.Variant{name, arg} <- variants
            ]
            ++
            [ DataVariant
                { dvCtorName = Name.localToUnQ unknownCtor
                , dvArgs = APos [TPrim $ C.PrimInt $ C.IntType C.Unsigned C.Sz16]
                }
            | isUnion
            ]
        }
    , instance_ [] ["Default"] "Default" [TLName typeName]
        [ iValue "def" [] (egName ["GenHelpersPure"] "defaultStruct")
        ]
    , instance_ [] ["Classes"] "FromStruct" [tgName ["Message"] "ConstMsg", TLName typeName]
        [ iValue "fromStruct" [PVar "struct"] $ EBind
            (EApp (egName ["Classes"] "fromStruct") [euName "struct"])
            (egName ["Classes"] "decerialize")
        ]
    , instance_ [] ["Classes"] "Decerialize" [TLName typeName]
        [ iType "Cerial" [tuName "msg", TLName typeName] $
            TApp (tgName (rawModule thisMod) cerialName) [tuName "msg"]
        , iValue "decerialize" [PVar "raw"] $
            let fieldGetter parentName name = egName
                    (rawModule thisMod)
                    (Name.unQToLocal $ Name.getterName $ Name.mkSub parentName name)
                decerializeArgs variantName = \case
                    P.None ->
                        EApp (eStd_ "pure") [ELName variantName]
                    P.Positional type_ ->
                        if cerialEq type_ then
                            EApp (eStd_ "pure") [EApp (ELName variantName) [euName "raw"]]
                        else
                            EFApp
                                (ELName variantName)
                                [EApp (egName ["Classes"] "decerialize") [euName "raw"]]
                    P.Record fields ->
                        EFApp
                            (ELName variantName)
                            [
                                let getter = EApp (fieldGetter variantName name) [euName "raw"] in
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
            in
            case variants of
                [P.Variant{name, arg}] ->
                    decerializeArgs name arg
                _ ->
                    EDo
                        [DoBind "raw" $ EApp (fieldGetter cerialName "") [euName "raw"]
                        ]
                        (ECase (ELName "raw") $
                            [ case arg of
                                P.None ->
                                    ( pgName (rawModule thisMod) name []
                                    , decerializeArgs name arg
                                    )
                                P.Positional _ ->
                                    ( pgName (rawModule thisMod) name [PVar "raw"]
                                    , decerializeArgs name arg
                                    )
                                P.Record _ ->
                                    ( pgName (rawModule thisMod) name [PVar "raw"]
                                    , decerializeArgs name arg
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
        ]
    , instance_ [] ["Classes"] "Marshal" [TLName typeName]
        [ iValue "marshalInto" [PVar "raw_", PVar "value_"] $
            ECase (euName "value_") $
                [ let setter = Name.unQToLocal $ Name.setterName variantName
                      setExp = EApp (egName (rawModule thisMod) setter) [euName "raw_"]
                  in case arg of
                    P.None ->
                        ( PLCtor variantName []
                        , if isUnion
                            then EApp setExp [ETup []]
                            else ePureUnit
                        )
                    P.Positional type_ ->
                        ( PLCtor variantName [PVar "arg_"]
                        , marshalField MarshalField
                            { thisMod
                            , into = "raw_"
                            , localQField = variantName
                            , from = "arg_"
                            , type_
                            , inUnion = isUnion
                            }
                        )
                    P.Record fields ->
                        ( PLRecordWildCard variantName
                        , EDo
                            ([ DoBind "raw_" setExp | isUnion ] ++
                            [ DoE $ marshalField MarshalField
                                { thisMod
                                , into = "raw_"
                                , localQField = Name.mkSub variantName fieldName
                                , from = fieldName
                                , type_
                                , inUnion = isUnion
                                }
                            | P.Field{name=fieldName, type_} <- fields
                            ])
                            ePureUnit
                        )
                | P.Variant{name=variantName, arg} <- variants
                ]
                ++
                if isUnion then
                    let setter = Name.unQToLocal $ Name.setterName unknownCtor in
                    [ ( PLCtor unknownCtor [PVar "tag"]
                      , EApp (egName (rawModule thisMod) setter) [euName "raw_", euName "tag"]
                      )
                    ]
                else
                    []
        ]
    ]
    ++
    if firstClass then
        [ instance_ [] ["Classes"] "Cerialize" [TLName typeName] []
        , instance_ [] ["Classes"] "Cerialize" [TApp (tgName ["V"] "Vector") [TLName typeName]]
            [ iValue "cerialize" [] (egName ["GenHelpersPure"] "cerializeCompositeVec")
            ]
        ]
    else
        []
declToDecls thisMod P.Constant { name, value=C.PtrValue ty _ } =
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
-- For these two we just re-export the ones from the raw module, so no need
-- to do anything here:
declToDecls _thisMod P.Constant { value=C.WordValue _ _ } = []
declToDecls _thisMod P.Constant { value=C.VoidValue } = []

declToDecls thisMod P.Interface { name } =
    [ DcData Data
        { dataName = Name.localToUnQ name
        , dataNewtype = True
        , dataVariants =
            [ DataVariant
                { dvCtorName = Name.localToUnQ name
                , dvArgs = APos [tgName ["Message"] "Client"]
                }
            ]
        , typeArgs = []
        , derives =
            [ "Std_.Show"
            , "Std_.Eq"
            , "Generics.Generic"
            ]
        }
    , instance_ [] ["Rpc"] "IsClient" [TLName name]
        [ iValue "fromClient" [] (ELName name)
        , iValue "toClient" [PLCtor name [PVar "client"]] (euName "client")
        ]
    , instance_ [] ["Classes"] "FromPtr" [tuName "msg", TLName name]
        [ iValue "fromPtr" [] (egName ["RpcHelpers"] "isClientFromPtr")
        ]
    , instance_ [] ["Classes"] "ToPtr" [tuName "s", TLName name]
        [ iValue "toPtr" [] (egName ["RpcHelpers"] "isClientToPtr")
        ]
    , instance_ [] ["Classes"] "Decerialize" [TLName name]
        [ iType "Cerial" [tuName "msg", TLName name] $
            TApp (tgName (rawModule thisMod) name) [tuName "msg"]
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
    , instance_ [] ["Classes"] "Cerialize" [TLName name]
        [ iValue "cerialize" [PVar "msg", PLCtor name [PVar "client"]] $
            EFApp
                (egName (rawModule thisMod) (Name.mkSub name "newtype_"))
                [ EFApp
                    (eStd_ "Just")
                    [EApp (egName ["Untyped"] "appendCap") [euName "msg", euName "client"]]
                ]
        ]
    , DcClass
        { ctx = [TApp (tgName ["MonadIO"] "MonadIO") [tuName "m"]]
        , name = Name.mkSub name "server_"
        , params = ["m", "cap"]
        , decls = [] -- TODO
        }
    ]


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
    , type_       :: C.Type Name.CapnpQ
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

typeToType :: Word64 -> C.Type Name.CapnpQ -> Type
typeToType _thisMod C.VoidType =
    TUnit
typeToType _thisMod (C.WordType (C.PrimWord ty)) =
    TPrim ty
typeToType _thisMod (C.WordType (C.EnumType Name.CapnpQ{local, fileId})) =
    -- Enums are just re-exported from the raw module, we should still
    -- refer to them qualified even if they're in the file we're generated
    -- from:
    tgName (rawModule fileId) local
typeToType thisMod (C.CompositeType (C.StructType n)) =
    nameToType thisMod n
typeToType thisMod (C.PtrType (C.PtrComposite (C.StructType n))) =
    nameToType thisMod n
typeToType thisMod (C.PtrType (C.ListOf ty)) =
    TApp (tgName ["V"] "Vector") [typeToType thisMod ty]
typeToType _thisMod (C.PtrType (C.PrimPtr C.PrimText)) =
    tgName ["T"] "Text"
typeToType _thisMod (C.PtrType (C.PrimPtr C.PrimData)) =
    tgName ["BS"] "ByteString"
typeToType _thisMod (C.PtrType (C.PrimPtr (C.PrimAnyPtr _))) =
    -- TODO: distinguish different pointer types.
    TApp (tStd_ "Maybe") [tgName ["UntypedPure"] "Ptr"]
typeToType thisMod (C.PtrType (C.PtrInterface n)) =
    nameToType thisMod n

nameToType :: Word64 -> Name.CapnpQ -> Type
nameToType thisMod Name.CapnpQ{local, fileId}
    | thisMod == fileId = TLName local
    | otherwise = tgName (pureModule fileId) local

rawModule :: Word64 -> [T.Text]
rawModule modId =
    ["Capnp", "Gen", "ById", T.pack $ printf "X%x" modId]

pureModule :: Word64 -> [T.Text]
pureModule modId =
    rawModule modId ++ ["Pure"]
