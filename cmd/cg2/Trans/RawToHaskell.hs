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
std_ name = globalName ["Std_"] (Name.mkLocal Name.emptyNS name)

globalName :: [T.Text] -> Name.LocalQ -> Name.GlobalQ
globalName parts local = Name.GlobalQ
    { globalNS = Name.NS parts
    , local
    }

untypedStruct :: Name.GlobalQ
untypedStruct = Name.GlobalQ
    { globalNS = Name.NS ["Untyped"]
    , local = Name.mkLocal Name.emptyNS "Struct"
    }

untypedClient :: Name.GlobalQ
untypedClient = Name.GlobalQ
    { globalNS = Name.NS ["Message"]
    , local = Name.mkLocal Name.emptyNS "Client"
    }

readCtx :: T.Text -> T.Text -> H.Type
readCtx m msg = H.TypeApp
    (H.GlobalNamedType
        Name.GlobalQ
            { globalNS = Name.NS ["Untyped"]
            , local = Name.mkLocal Name.emptyNS "ReadCtx"
            }
    )
    [ H.TypeVar m
    , H.TypeVar msg
    ]

fileToModule :: Raw.File -> H.Module
fileToModule Raw.File{fileName, fileId, decls} =
    H.Module
        { modName = makeModName fileName
        , modDecls = concatMap (declToDecls fileId) decls
        , modImports =
            [ H.Import
                { importAs = "Message"
                , parts = ["Capnp", "Message"]
                }
            , H.Import
                { importAs = "Untyped"
                , parts = ["Capnp", "Untyped"]
                }
            , H.Import
                { importAs = "Basics"
                , parts = ["Capnp", "Basics"]
                }
            , H.Import
                { importAs = "GenHelpers"
                , parts = ["Capnp", "GenHelpers"]
                }
            , H.Import
                { importAs = "Classes"
                , parts = ["Capnp", "Classes"]
                }
            ]
        }

declToDecls :: Word64 -> Raw.Decl -> [H.Decl]
declToDecls _thisMod Raw.Enum{typeCtor, dataCtors} =
    let unknownCtor = Name.mkSub typeCtor "unknown'" in
    [ H.DataDecl
        { H.dataName = Name.localToUnQ typeCtor
        , H.dataVariants =
            map enumerantToVariant dataCtors
            ++
            [ H.DataVariant
                { dvCtorName = Name.localToUnQ unknownCtor
                , dvArgs = H.PosArgs
                    [ H.PrimType $ C.PrimInt $ C.IntType C.Unsigned C.Sz16 ]
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
            , dvArgs = H.PosArgs []
            }
declToDecls _thisMod Raw.InterfaceWrapper{ctorName} =
    [ newtypeWrapper ctorName untypedClient ]
declToDecls _thisMod Raw.StructWrapper{ctorName} =
    [ newtypeWrapper ctorName untypedStruct ]
declToDecls thisMod Raw.Getter{fieldName, fieldLocType, containerType} =
    [ H.ValueDecl
        { typ = H.CtxType
            [readCtx "m" "msg"]
            (H.FnType
                [ H.TypeApp
                    (H.LocalNamedType containerType)
                    [ H.TypeVar "msg" ]
                , H.TypeApp
                    (H.TypeVar "m")
                    [ typeToType thisMod (C.fieldType fieldLocType) "msg"
                    ]
                ]
            )
        , def = H.ValueDef
            { name = Name.UnQ $
                "get_" <> Name.renderLocalQ fieldName
            , params = [H.PLocalCtor containerType [H.PVar "struct"]]
            , value = case fieldLocType of
                C.DataField C.DataLoc{dataIdx, dataOff, dataDef} _ ->
                    H.ExApp
                        (H.ExGlobalName
                            Name.GlobalQ
                                { globalNS = Name.NS [ "GenHelpers" ]
                                , local = "getWordField"
                                }
                        )
                        [ H.ExLocalName "struct"
                        , H.ExInteger $ fromIntegral dataIdx
                        , H.ExInteger $ fromIntegral dataOff
                        , H.ExInteger $ fromIntegral dataDef
                        ]
                C.PtrField idx _ -> H.ExDo
                    [ H.DoBind "ptr" $ H.ExApp
                        (H.ExGlobalName $ globalName ["Untyped"] "getPtr")
                        [ H.ExInteger (fromIntegral idx)
                        , H.ExLocalName "struct"
                        ]
                    ]
                    (H.ExApp
                        (H.ExGlobalName $ globalName ["Classes"] "fromPtr")
                        [ H.ExApp
                            (H.ExGlobalName $ globalName ["Untyped"] "message")
                            [H.ExLocalName "struct"]
                        , H.ExLocalName "ptr"
                        ]
                    )
                C.HereField _ ->
                    H.ExApp
                        (H.ExGlobalName $ globalName ["Classes"] "fromStruct")
                        [H.ExLocalName "struct"]
                C.VoidField ->
                    H.ExApp
                        (H.ExGlobalName $ std_ "pure")
                        [H.ExTuple []]
            }
        }
    ]

-- | Make an instance of the IsWord type class for an enum.
mkIsWordInstance :: Name.LocalQ -> [Name.LocalQ] -> Name.LocalQ -> H.Decl
mkIsWordInstance typeCtor dataCtors unknownCtor = H.InstanceDecl
    { ctx = []
    , typ = H.TypeApp
        (H.GlobalNamedType Name.GlobalQ
            { globalNS = Name.NS ["Classes"]
            , local = "IsWord"
            })
        [H.LocalNamedType typeCtor]
    , defs =
        [ H.ValueDef
            { name = "fromWord"
            , params = [ H.PInteger i ]
            , value = H.ExLocalName ctor
            }
        | (i, ctor) <- zip [0..] dataCtors
        ] ++
        [ H.ValueDef
            { name = "fromWord"
            , params = [ H.PVar "tag" ]
            , value = H.ExApp
                (H.ExLocalName unknownCtor)
                [ H.ExApp
                    (H.ExGlobalName $ std_ "fromIntegral")
                    [H.ExLocalName "tag"]
                ]
            }
        ] ++
        [ H.ValueDef
            { name = "toWord"
            , params = [ H.PLocalCtor ctor [] ]
            , value = H.ExInteger i
            }
        | (i, ctor) <- zip [0..] dataCtors
        ] ++
        [ H.ValueDef
            { name = "toWord"
            , params =
                [ H.PLocalCtor unknownCtor [H.PVar "tag"] ]
            , value =
                H.ExApp
                    (H.ExGlobalName $ std_ "fromIntegral")
                    [H.ExLocalName "tag"]
            }
        ]
    }

newtypeWrapper :: Name.LocalQ -> Name.GlobalQ -> H.Decl
newtypeWrapper ctorName wrappedType =
    let name = Name.localToUnQ ctorName
    in H.NewtypeDecl
        { dataName = name
        , typeArgs = [ "msg" ]
        , dataVariant = H.DataVariant
            { dvCtorName = name
            , dvArgs = H.PosArgs
                [ H.TypeApp
                    (H.GlobalNamedType wrappedType)
                    [H.TypeVar "msg"]
                ]
            }
        , derives = []
        }

nameToType :: Word64 -> Name.CapnpQ -> H.Type
nameToType thisMod Name.CapnpQ{local, fileId} =
    if fileId == thisMod
        then H.LocalNamedType local
        else H.GlobalNamedType Name.GlobalQ
                { globalNS = Name.NS [ "Capnp", "Gen", T.pack $ printf "X%x" fileId ]
                , local
                }
typeToType :: Word64 -> C.Type Name.CapnpQ -> T.Text -> H.Type
typeToType thisMod ty var = case ty of
    C.VoidType -> H.UnitType
    C.WordType (C.PrimWord ty) -> H.PrimType ty
    C.WordType (C.EnumType typeId) ->
        nameToType thisMod typeId
    C.PtrType (C.ListOf elt) ->
        H.TypeApp (basics "List")
            [ H.TypeVar var
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
        appV $ H.GlobalNamedType Name.GlobalQ
            { globalNS = Name.NS [ "Untyped" ]
            , local = Name.mkLocal Name.emptyNS "Ptr"
            }
    C.CompositeType (C.StructType typeId) ->
        appV $ nameToType thisMod typeId
  where
    appV t = H.TypeApp t [H.TypeVar var]
    basics name = H.GlobalNamedType Name.GlobalQ
        { globalNS = Name.NS [ "Basics" ]
        , local = Name.mkLocal Name.emptyNS name
        }

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
