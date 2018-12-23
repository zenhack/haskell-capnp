{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.RawToHaskell (fileToModule) where

import Data.Word

import Data.Char       (toUpper)
import System.FilePath (splitDirectories)
import Text.Printf     (printf)

import qualified Data.Text as T

import IR.Haskell

import qualified IR.Common as C
import qualified IR.Name   as Name
import qualified IR.Raw    as Raw

std_ :: Name.UnQ -> Name.GlobalQ
std_ name = gName ["Std_"] (Name.mkLocal Name.emptyNS name)

gName :: [T.Text] -> Name.LocalQ -> Name.GlobalQ
gName parts local = Name.GlobalQ
    { globalNS = Name.NS parts
    , local
    }

dfValue :: Name.UnQ -> [Pattern] -> Exp -> ValueDef
dfValue name params value = DfValue {name, params, value}

readCtx :: T.Text -> T.Text -> Type
readCtx m msg = TApp
    (TGName $ gName ["Untyped"] "ReadCtx")
    [ TVar m
    , TVar msg
    ]

fileToModule :: Raw.File -> Module
fileToModule Raw.File{fileName, fileId, decls} =
    Module
        { modName = makeModName fileName
        , modImports =
            [ imp ["Capnp", "Message"] "Message"
            , imp ["Capnp", "Untyped"] "Untyped"
            , imp ["Capnp", "Basics"] "Basics"
            , imp ["Capnp", "GenHelpers"] "GenHelpers"
            , imp ["Capnp", "Classes"] "Classes"
            ]
        , modDecls = concatMap (declToDecls fileId) decls
        }
  where
    imp parts importAs = Import {parts, importAs}

declToDecls :: Word64 -> Raw.Decl -> [Decl]
declToDecls _thisMod Raw.Enum{typeCtor, dataCtors} =
    let unknownCtor = Name.mkSub typeCtor "unknown'" in
    [ DcData
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
        , derives = [ "Std_.Show", "Std_.Eq" ]
        }
    , mkIsWordInstance typeCtor dataCtors unknownCtor
    ]
  where
    enumerantToVariant variantName =
        DataVariant
            { dvCtorName =
                Name.localToUnQ variantName
            , dvArgs = APos []
            }
declToDecls _thisMod Raw.InterfaceWrapper{ctorName} =
    [ newtypeWrapper ctorName $ gName ["Message"] "Client" ]
declToDecls _thisMod Raw.StructWrapper{ctorName} =
    [ newtypeWrapper ctorName $ gName ["Untyped"] "Struct" ]
declToDecls _thisMod Raw.StructListElem{ctorName} =
    [ DcInstance
        { ctx = []
        , typ = TApp
            (TGName (gName ["Basics"] "ListElem"))
            [ TVar "msg"
            , TApp
                (TLName ctorName)
                [TVar "msg"]
            ]
        , defs =
            let listCtor = Name.mkSub ctorName "List_" in
            [ -- TODO: List newtype.
              dfValue "listFromPtr" [PVar "msg", PVar "ptr"] $ EFApp
                (ELName listCtor)
                [ EApp
                    (EGName $ gName ["Classes"] "fromPtr")
                    [ELName "msg", ELName "ptr"]
                ]
            , dfValue "toUntypedList" [PLCtor listCtor [PVar "l"]] $ EApp
                (EGName $ gName ["Untyped"] "ListStruct")
                [ELName "l"]
            , dfValue "length" [PLCtor listCtor [PVar "l"]] $ EApp
                (EGName $ gName ["Untyped"] "length")
                [ELName "l"]
            , dfValue "index" [PVar "i", PLCtor listCtor [PVar "l"]] $ EDo
                [ DoBind "elt" $ EApp
                    (EGName $ gName ["Untyped"] "index")
                    [ ELName "i"
                    , ELName "l"
                    ]
                ]
                ( EApp
                    (EGName $ gName ["Classes"] "fromStruct")
                    [ELName "elt"]
                )
            ]
        }
    ]
declToDecls thisMod Raw.Getter{fieldName, fieldLocType, containerType} =
    [ DcValue
        { typ = TCtx
            [readCtx "m" "msg"]
            (TFn
                [ TApp
                    (TLName containerType)
                    [ TVar "msg" ]
                , TApp
                    (TVar "m")
                    [ typeToType thisMod (C.fieldType fieldLocType) "msg"
                    ]
                ]
            )
        , def = DfValue
            { name = Name.UnQ $
                "get_" <> Name.renderLocalQ fieldName
            , params = [PLCtor containerType [PVar "struct"]]
            , value = case fieldLocType of
                C.DataField C.DataLoc{dataIdx, dataOff, dataDef} _ ->
                    EApp
                        (EGName $ gName ["GenHelpers"] "getWordField")
                        [ ELName "struct"
                        , EInt $ fromIntegral dataIdx
                        , EInt $ fromIntegral dataOff
                        , EInt $ fromIntegral dataDef
                        ]
                C.PtrField idx _ -> EDo
                    [ DoBind "ptr" $ EApp
                        (EGName $ gName ["Untyped"] "getPtr")
                        [ EInt (fromIntegral idx)
                        , ELName "struct"
                        ]
                    ]
                    (EApp
                        (EGName $ gName ["Classes"] "fromPtr")
                        [ EApp
                            (EGName $ gName ["Untyped"] "message")
                            [ELName "struct"]
                        , ELName "ptr"
                        ]
                    )
                C.HereField _ ->
                    EApp
                        (EGName $ gName ["Classes"] "fromStruct")
                        [ELName "struct"]
                C.VoidField ->
                    EApp
                        (EGName $ std_ "pure")
                        [ETup []]
            }
        }
    ]

-- | Make an instance of the IsWord type class for an enum.
mkIsWordInstance :: Name.LocalQ -> [Name.LocalQ] -> Name.LocalQ -> Decl
mkIsWordInstance typeCtor dataCtors unknownCtor = DcInstance
    { ctx = []
    , typ = TApp
        (TGName $ gName ["Classes"] "IsWord")
        [TLName typeCtor]
    , defs =
        [ DfValue
            { name = "fromWord"
            , params = [PInt i]
            , value = ELName ctor
            }
        | (i, ctor) <- zip [0..] dataCtors
        ] ++
        [ DfValue
            { name = "fromWord"
            , params = [PVar "tag"]
            , value = EApp
                (ELName unknownCtor)
                [ EApp
                    (EGName $ std_ "fromIntegral")
                    [ELName "tag"]
                ]
            }
        ] ++
        [ DfValue
            { name = "toWord"
            , params = [PLCtor ctor []]
            , value = EInt i
            }
        | (i, ctor) <- zip [0..] dataCtors
        ] ++
        [ DfValue
            { name = "toWord"
            , params =
                [ PLCtor unknownCtor [PVar "tag"] ]
            , value =
                EApp
                    (EGName $ std_ "fromIntegral")
                    [ELName "tag"]
            }
        ]
    }

newtypeWrapper :: Name.LocalQ -> Name.GlobalQ -> Decl
newtypeWrapper ctorName wrappedType =
    let name = Name.localToUnQ ctorName
    in DcData
        { dataName = name
        , dataNewtype = True
        , typeArgs = [ "msg" ]
        , dataVariants =
            [ DataVariant
                { dvCtorName = name
                , dvArgs = APos
                    [ TApp
                        (TGName wrappedType)
                        [TVar "msg"]
                    ]
                }
            ]
        , derives = []
        }

nameToType :: Word64 -> Name.CapnpQ -> Type
nameToType thisMod Name.CapnpQ{local, fileId} =
    if fileId == thisMod
        then TLName local
        else TGName $ gName
            ["Capnp", "Gen", T.pack $ printf "X%x" fileId]
            local
typeToType :: Word64 -> C.Type Name.CapnpQ -> T.Text -> Type
typeToType thisMod ty var = case ty of
    C.VoidType -> TUnit
    C.WordType (C.PrimWord ty) -> TPrim ty
    C.WordType (C.EnumType typeId) ->
        nameToType thisMod typeId
    C.PtrType (C.ListOf elt) ->
        TApp (basics "List")
            [ TVar var
            , typeToType thisMod elt var
            ]
    C.PtrType (C.PrimPtr C.PrimText) ->
        appV $ basics "Text"
    C.PtrType (C.PrimPtr C.PrimData) ->
        appV $ basics "Data"
    C.PtrType (C.PtrComposite (C.StructType typeId)) ->
        appV $ nameToType thisMod typeId
    C.PtrType (C.PtrInterface typeId) ->
        nameToType thisMod typeId
    C.PtrType (C.PrimPtr (C.PrimAnyPtr _)) ->
        appV $ TGName $ gName ["Untyped"] "Ptr"
    C.CompositeType (C.StructType typeId) ->
        appV $ nameToType thisMod typeId
  where
    appV t = TApp t [TVar var]
    basics = TGName . gName ["Basics"]

-- | Transform the file path into a valid haskell module name.
-- TODO: this is a best-effort transformation; it gives good
-- results on the schema I've found in the wild, but may fail
-- to generate valid/non-overlapping module names in all cases.
makeModName :: FilePath -> [Name.UnQ]
makeModName fileName =
    "Capnp":"Gen":[ Name.UnQ (T.pack (mangleSegment seg)) | seg <- splitDirectories fileName ]
  where
    mangleSegment "c++.capnp" = "Cxx"
    mangleSegment ""          = error "Unexpected empty file name"
    mangleSegment (c:cs)      = go (toUpper c : cs) where
        go ('-':c:cs) = toUpper c : go cs
        go ".capnp"   = ""
        go []         = ""
        go (c:cs)     = c : go cs
