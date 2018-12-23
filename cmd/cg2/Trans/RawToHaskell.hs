{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.RawToHaskell (fileToModule) where

import Data.Word

import Data.Char       (toUpper)
import System.FilePath (splitDirectories)
import Text.Printf     (printf)

import qualified Data.Text as T

import qualified IR.Common  as C
import qualified IR.Haskell as H
import qualified IR.Name    as Name
import qualified IR.Raw     as Raw

std_ :: Name.UnQ -> Name.GlobalQ
std_ name = gName ["Std_"] (Name.mkLocal Name.emptyNS name)

gName :: [T.Text] -> Name.LocalQ -> Name.GlobalQ
gName parts local = Name.GlobalQ
    { globalNS = Name.NS parts
    , local
    }

readCtx :: T.Text -> T.Text -> H.Type
readCtx m msg = H.TApp
    (H.TGName $ gName ["Untyped"] "ReadCtx")
    [ H.TVar m
    , H.TVar msg
    ]

fileToModule :: Raw.File -> H.Module
fileToModule Raw.File{fileName, fileId, decls} =
    H.Module
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
    imp parts importAs = H.Import {parts, importAs}

declToDecls :: Word64 -> Raw.Decl -> [H.Decl]
declToDecls _thisMod Raw.Enum{typeCtor, dataCtors} =
    let unknownCtor = Name.mkSub typeCtor "unknown'" in
    [ H.DcData
        { H.dataName = Name.localToUnQ typeCtor
        , H.dataNewtype = False
        , H.typeArgs = []
        , H.dataVariants =
            map enumerantToVariant dataCtors
            ++
            [ H.DataVariant
                { dvCtorName = Name.localToUnQ unknownCtor
                , dvArgs = H.APos
                    [ H.TPrim $ C.PrimInt $ C.IntType C.Unsigned C.Sz16 ]
                }
            ]
        , H.derives = [ "Std_.Show", "Std_.Eq" ]
        }
    , mkIsWordInstance typeCtor dataCtors unknownCtor
    ]
  where
    enumerantToVariant variantName =
        H.DataVariant
            { dvCtorName =
                Name.localToUnQ variantName
            , dvArgs = H.APos []
            }
declToDecls _thisMod Raw.InterfaceWrapper{ctorName} =
    [ newtypeWrapper ctorName $ gName ["Message"] "Client" ]
declToDecls _thisMod Raw.StructWrapper{ctorName} =
    [ newtypeWrapper ctorName $ gName ["Untyped"] "Struct" ]
declToDecls thisMod Raw.Getter{fieldName, fieldLocType, containerType} =
    [ H.DcValue
        { typ = H.TCtx
            [readCtx "m" "msg"]
            (H.TFn
                [ H.TApp
                    (H.TLName containerType)
                    [ H.TVar "msg" ]
                , H.TApp
                    (H.TVar "m")
                    [ typeToType thisMod (C.fieldType fieldLocType) "msg"
                    ]
                ]
            )
        , def = H.DfValue
            { name = Name.UnQ $
                "get_" <> Name.renderLocalQ fieldName
            , params = [H.PLCtor containerType [H.PVar "struct"]]
            , value = case fieldLocType of
                C.DataField C.DataLoc{dataIdx, dataOff, dataDef} _ ->
                    H.EApp
                        (H.EGName $ gName ["GenHelpers"] "getWordField")
                        [ H.ELName "struct"
                        , H.EInt $ fromIntegral dataIdx
                        , H.EInt $ fromIntegral dataOff
                        , H.EInt $ fromIntegral dataDef
                        ]
                C.PtrField idx _ -> H.EDo
                    [ H.DoBind "ptr" $ H.EApp
                        (H.EGName $ gName ["Untyped"] "getPtr")
                        [ H.EInt (fromIntegral idx)
                        , H.ELName "struct"
                        ]
                    ]
                    (H.EApp
                        (H.EGName $ gName ["Classes"] "fromPtr")
                        [ H.EApp
                            (H.EGName $ gName ["Untyped"] "message")
                            [H.ELName "struct"]
                        , H.ELName "ptr"
                        ]
                    )
                C.HereField _ ->
                    H.EApp
                        (H.EGName $ gName ["Classes"] "fromStruct")
                        [H.ELName "struct"]
                C.VoidField ->
                    H.EApp
                        (H.EGName $ std_ "pure")
                        [H.ETup []]
            }
        }
    ]

-- | Make an instance of the IsWord type class for an enum.
mkIsWordInstance :: Name.LocalQ -> [Name.LocalQ] -> Name.LocalQ -> H.Decl
mkIsWordInstance typeCtor dataCtors unknownCtor = H.DcInstance
    { ctx = []
    , typ = H.TApp
        (H.TGName $ gName ["Classes"] "IsWord")
        [H.TLName typeCtor]
    , defs =
        [ H.DfValue
            { name = "fromWord"
            , params = [H.PInt i]
            , value = H.ELName ctor
            }
        | (i, ctor) <- zip [0..] dataCtors
        ] ++
        [ H.DfValue
            { name = "fromWord"
            , params = [H.PVar "tag"]
            , value = H.EApp
                (H.ELName unknownCtor)
                [ H.EApp
                    (H.EGName $ std_ "fromIntegral")
                    [H.ELName "tag"]
                ]
            }
        ] ++
        [ H.DfValue
            { name = "toWord"
            , params = [H.PLCtor ctor []]
            , value = H.EInt i
            }
        | (i, ctor) <- zip [0..] dataCtors
        ] ++
        [ H.DfValue
            { name = "toWord"
            , params =
                [ H.PLCtor unknownCtor [H.PVar "tag"] ]
            , value =
                H.EApp
                    (H.EGName $ std_ "fromIntegral")
                    [H.ELName "tag"]
            }
        ]
    }

newtypeWrapper :: Name.LocalQ -> Name.GlobalQ -> H.Decl
newtypeWrapper ctorName wrappedType =
    let name = Name.localToUnQ ctorName
    in H.DcData
        { dataName = name
        , dataNewtype = True
        , typeArgs = [ "msg" ]
        , dataVariants =
            [ H.DataVariant
                { dvCtorName = name
                , dvArgs = H.APos
                    [ H.TApp
                        (H.TGName wrappedType)
                        [H.TVar "msg"]
                    ]
                }
            ]
        , derives = []
        }

nameToType :: Word64 -> Name.CapnpQ -> H.Type
nameToType thisMod Name.CapnpQ{local, fileId} =
    if fileId == thisMod
        then H.TLName local
        else H.TGName $ gName
            ["Capnp", "Gen", T.pack $ printf "X%x" fileId]
            local
typeToType :: Word64 -> C.Type Name.CapnpQ -> T.Text -> H.Type
typeToType thisMod ty var = case ty of
    C.VoidType -> H.TUnit
    C.WordType (C.PrimWord ty) -> H.TPrim ty
    C.WordType (C.EnumType typeId) ->
        nameToType thisMod typeId
    C.PtrType (C.ListOf elt) ->
        H.TApp (basics "List")
            [ H.TVar var
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
        appV $ H.TGName $ gName ["Untyped"] "Ptr"
    C.CompositeType (C.StructType typeId) ->
        appV $ nameToType thisMod typeId
  where
    appV t = H.TApp t [H.TVar var]
    basics = H.TGName . gName ["Basics"]

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
