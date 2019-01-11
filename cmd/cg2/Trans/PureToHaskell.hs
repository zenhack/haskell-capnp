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
        , modImports = [ ImportQual { parts = reExport } ]
        , modDecls = []
        }

fileToMainModule :: P.File -> Module
fileToMainModule P.File{fileName, fileId, decls, fileImports, reExportEnums} = Module
    { modName = ["Capnp", "Gen"] ++ makeModName fileName ++ ["Pure"]
    , modLangPragmas =
        [ "DeriveGeneric"
        , "DuplicateRecordFields"
        , "RecordWildCards"
        , "MultiParamTypeClasses"
        , "TypeFamilies"
        ]
    , modExports = Just $
        [ExportGCtors (gName (rawModule fileId) name) | name <- reExportEnums]
        ++ [ExportLCtors typeName | P.Data{typeName} <- decls]
    , modImports = concat $
        [ ImportAs { importAs = "V", parts = ["Data", "Vector"] }
        , ImportAs { importAs = "T", parts = ["Data", "Text"] }
        , ImportAs { importAs = "BS", parts = ["Data", "ByteString"] }
        , ImportAs { importAs = "Default", parts = ["Data", "Default"] }
        , ImportAs { importAs = "Generics", parts = ["GHC", "Generics"] }
        , ImportAs { importAs = "UntypedPure", parts = ["Capnp", "Untyped", "Pure"] }
        , ImportAs { importAs = "Message", parts = ["Capnp", "Message"] }
        , ImportAs { importAs = "Classes", parts = ["Capnp", "Classes"] }
        , ImportAs { importAs = "GenHelpersPure", parts = ["Capnp", "GenHelpers", "Pure"] }
        , ImportQual { parts = idToModule fileId }
        ]
        :
        [ [ ImportQual { parts = impId }
          , ImportQual { parts = impId ++ ["Pure"] }
          ]
        | impId <- map idToModule fileImports
        ]
    , modDecls = concatMap (declToDecls fileId) decls
    }

declToDecls :: Word64 -> P.Decl -> [Decl]
declToDecls thisMod P.Data{typeName, cerialName, variants, isUnion} =
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
                            let unknownCtor = Name.mkSub cerialName "unknown'" in
                            [ ( pgName (rawModule thisMod) unknownCtor [PVar "tag"]
                              , EApp
                                  (eStd_ "pure")
                                  [EApp (ELName unknownCtor) [euName "tag"]]
                              )
                            ]
                        )
        ]
    , instance_ [] ["Classes"] "Cerialize" [TLName typeName] []
    , instance_ [] ["Classes"] "Marshal" [TLName typeName]
        [ iValue "marshalInto" [PVar "raw_", PVar "value_"] $
            ECase (euName "value_") $
                [ let setter = Name.unQToLocal $ Name.setterName variantName
                      setExp = EApp (egName (rawModule thisMod) setter) [euName "raw_"]
                  in case arg of
                    P.None ->
                        ( PLCtor variantName []
                        , if isUnion
                            then setExp
                            else ePureUnit
                        )
                    P.Positional type_ ->
                        ( PLCtor variantName [PVar "arg_"]
                        , marshalField
                            thisMod
                            (euName "raw_")
                            variantName
                            "arg_"
                            type_
                        )
                    P.Record fields ->
                        ( PLRecordWildCard variantName
                        , EDo
                            ( (if isUnion
                                then [DoBind "raw_" setExp]
                                else [])
                            ++
                            [ DoE $ marshalField
                                thisMod
                                (euName "raw_")
                                (Name.mkSub variantName fieldName)
                                fieldName
                                type_
                            | P.Field{name=fieldName, type_} <- fields
                            ])
                            ePureUnit
                        )
                | P.Variant{name=variantName, arg} <- variants
                ]
                ++
                if isUnion then
                    let unknownCtor = Name.mkSub typeName "unknown'"
                        setter = Name.unQToLocal $ Name.setterName $ Name.mkSub typeName "unknown'"
                    in
                    [ ( PLCtor unknownCtor [PVar "tag"]
                      , EApp (egName (rawModule thisMod) setter) [euName "raw_", euName "tag"]
                      )
                    ]
                else
                    []
        ]
    ]

marshalField :: Word64 -> Exp -> Name.LocalQ -> Name.UnQ -> C.Type Name.CapnpQ -> Exp
marshalField thisMod into fieldName varName type_ =
    let setter = egName (rawModule thisMod) $ Name.unQToLocal (Name.setterName fieldName)
    in case type_ of
        C.PtrType _ ->
            EBind
                (EApp
                    (egName ["Classes"] "cerialize")
                    [ EApp (egName ["Untyped"] "message") [euName "raw_"]
                    , euName varName
                    ]
                )
                (EApp setter [into])
        C.VoidType ->
            ePureUnit
        C.WordType _ ->
            EApp setter [into, euName varName]
        C.CompositeType _ ->
            -- Always a group or union, since a true struct would be a pointer.
            -- delegate to the field type, which has the same Cerial as us.
            EApp (egName ["Classes"] "marshalInto") [into, euName varName]

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
