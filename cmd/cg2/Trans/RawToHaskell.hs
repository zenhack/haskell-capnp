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
import qualified IR.Haskell as Haskell
import qualified IR.Name    as Name
import qualified IR.Raw     as Raw

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

readCtx :: T.Text -> T.Text -> Haskell.Type
readCtx m msg = Haskell.TypeApp
    (Haskell.GlobalNamedType
        Name.GlobalQ
            { globalNS = Name.NS ["Untyped"]
            , local = Name.mkLocal Name.emptyNS "ReadCtx"
            }
    )
    [ Haskell.TypeVar m
    , Haskell.TypeVar msg
    ]

fileToModule :: Raw.File -> Haskell.Module
fileToModule Raw.File{fileName, fileId, decls} =
    Haskell.Module
        { modName = makeModName fileName
        , modDecls = concatMap (declToDecls fileId) decls
        , modImports =
            [ Haskell.Import
                { importAs = "Message"
                , parts = ["Capnp", "Message"]
                }
            , Haskell.Import
                { importAs = "Untyped"
                , parts = ["Capnp", "Untyped"]
                }
            , Haskell.Import
                { importAs = "Basics"
                , parts = ["Capnp", "Basics"]
                }
            , Haskell.Import
                { importAs = "GenHelpers"
                , parts = ["Capnp", "GenHelpers"]
                }
            , Haskell.Import
                { importAs = "Classes"
                , parts = ["Capnp", "Classes"]
                }
            ]
        }

declToDecls :: Word64 -> Raw.Decl -> [Haskell.Decl]
declToDecls _thisMod Raw.Enum{typeCtor, dataCtors} =
    let unknownCtor = Name.mkSub typeCtor "unknown'" in
    [ Haskell.DataDecl
        { Haskell.dataName = Name.localToUnQ typeCtor
        , Haskell.dataVariants =
            map enumerantToVariant dataCtors
            ++
            [ Haskell.DataVariant
                { dvCtorName = Name.localToUnQ unknownCtor
                , dvArgs = Haskell.PosArgs
                    [ Haskell.PrimType $ C.PrimInt $ C.IntType C.Unsigned C.Sz16 ]
                }
            ]
        , Haskell.derives = [ "Std_.Show", "Std_.Eq" ]
        }
    , mkIsWordInstance typeCtor dataCtors unknownCtor
    ]
  where
    enumerantToVariant variantName =
        Haskell.DataVariant
            { dvCtorName =
                Name.localToUnQ variantName
            , dvArgs = Haskell.PosArgs []
            }
declToDecls _thisMod Raw.InterfaceWrapper{ctorName} =
    [ newtypeWrapper ctorName untypedClient ]
declToDecls _thisMod Raw.StructWrapper{ctorName} =
    [ newtypeWrapper ctorName untypedStruct ]
declToDecls thisMod Raw.Getter{fieldName, fieldLocType, containerType} =
    [ Haskell.ValueDecl
        { typ = Haskell.CtxType
            [readCtx "m" "msg"]
            (Haskell.FnType
                [ Haskell.TypeApp
                    (Haskell.LocalNamedType containerType)
                    [ Haskell.TypeVar "msg" ]
                , Haskell.TypeApp
                    (Haskell.TypeVar "m")
                    [ typeToType thisMod (C.fieldType fieldLocType) "msg"
                    ]
                ]
            )
        , def = Haskell.ValueDef
            { name = Name.UnQ $
                "get_" <> Name.renderLocalQ fieldName
            , params = [Haskell.PLocalCtor containerType [Haskell.PVar "struct"]]
            , value = case fieldLocType of
                C.DataField C.DataLoc{dataIdx, dataOff, dataDef} _ ->
                    Haskell.ExApp
                        (Haskell.ExGlobalName
                            Name.GlobalQ
                                { globalNS = Name.NS [ "GenHelpers" ]
                                , local = "getWordField"
                                }
                        )
                        [ Haskell.ExLocalName "struct"
                        , Haskell.ExInteger $ fromIntegral dataIdx
                        , Haskell.ExInteger $ fromIntegral dataOff
                        , Haskell.ExInteger $ fromIntegral dataDef
                        ]
                _ ->
                    Haskell.ExLocalName "undefined"
            }
        }
    ]

-- | Make an instance of the IsWord type class for an enum.
mkIsWordInstance :: Name.LocalQ -> [Name.LocalQ] -> Name.LocalQ -> Haskell.Decl
mkIsWordInstance typeCtor dataCtors unknownCtor = Haskell.InstanceDecl
    { ctx = []
    , typ = Haskell.TypeApp
        (Haskell.GlobalNamedType Name.GlobalQ
            { globalNS = Name.NS ["Classes"]
            , local = "IsWord"
            })
        [Haskell.LocalNamedType typeCtor]
    , defs =
        [ Haskell.ValueDef
            { name = "fromWord"
            , params = [ Haskell.PInteger i ]
            , value = Haskell.ExLocalName ctor
            }
        | (i, ctor) <- zip [0..] dataCtors
        ] ++
        [ Haskell.ValueDef
            { name = "fromWord"
            , params = [ Haskell.PVar "tag" ]
            , value = Haskell.ExApp
                (Haskell.ExLocalName unknownCtor)
                [ Haskell.ExApp
                    (Haskell.ExGlobalName Name.GlobalQ
                        { globalNS = Name.NS ["Std_"]
                        , local = "fromIntegral"
                        })
                    [Haskell.ExLocalName "tag"]
                ]
            }
        ] ++
        [ Haskell.ValueDef
            { name = "toWord"
            , params = [ Haskell.PLocalCtor ctor [] ]
            , value = Haskell.ExInteger i
            }
        | (i, ctor) <- zip [0..] dataCtors
        ] ++
        [ Haskell.ValueDef
            { name = "toWord"
            , params =
                [ Haskell.PLocalCtor unknownCtor [Haskell.PVar "tag"] ]
            , value =
                Haskell.ExApp
                    (Haskell.ExGlobalName Name.GlobalQ
                        { globalNS = Name.NS ["Std_"]
                        , local = "fromIntegral"
                        })
                    [Haskell.ExLocalName "tag"]
            }
        ]
    }

newtypeWrapper :: Name.LocalQ -> Name.GlobalQ -> Haskell.Decl
newtypeWrapper ctorName wrappedType =
    let name = Name.localToUnQ ctorName
    in Haskell.NewtypeDecl
        { dataName = name
        , typeArgs = [ "msg" ]
        , dataVariant = Haskell.DataVariant
            { dvCtorName = name
            , dvArgs = Haskell.PosArgs
                [ Haskell.TypeApp
                    (Haskell.GlobalNamedType wrappedType)
                    [Haskell.TypeVar "msg"]
                ]
            }
        , derives = []
        }

nameToType :: Word64 -> Name.CapnpQ -> Haskell.Type
nameToType thisMod Name.CapnpQ{local, fileId} =
    if fileId == thisMod
        then Haskell.LocalNamedType local
        else Haskell.GlobalNamedType Name.GlobalQ
                { globalNS = Name.NS [ "Capnp", "Gen", T.pack $ printf "X%x" fileId ]
                , local
                }
typeToType :: Word64 -> C.Type Name.CapnpQ -> T.Text -> Haskell.Type
typeToType thisMod ty var = case ty of
    C.VoidType -> Haskell.UnitType
    C.WordType (C.PrimWord ty) -> Haskell.PrimType ty
    C.WordType (C.EnumType typeId) ->
        nameToType thisMod typeId
    C.PtrType (C.ListOf elt) ->
        Haskell.TypeApp (basics "List")
            [ Haskell.TypeVar var
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
        appV $ Haskell.GlobalNamedType Name.GlobalQ
            { globalNS = Name.NS [ "Untyped" ]
            , local = Name.mkLocal Name.emptyNS "Ptr"
            }
    C.CompositeType (C.StructType typeId) ->
        appV $ nameToType thisMod typeId
  where
    appV t = Haskell.TypeApp t [Haskell.TypeVar var]
    basics name = Haskell.GlobalNamedType Name.GlobalQ
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
