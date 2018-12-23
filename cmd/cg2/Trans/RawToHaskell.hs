{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.RawToHaskell (cgrToModules) where

import Data.Word

import Data.Char       (toUpper)
import System.FilePath (splitDirectories)
import Text.Printf     (printf)

import qualified Data.Text as T

import qualified IR.Common  as C
import qualified IR.Haskell as Haskell
import qualified IR.Name    as Name
import qualified IR.Raw     as Raw

import qualified Data.Map.Strict as M

untypedStruct :: Name.GlobalQ
untypedStruct = Name.GlobalQ
    { globalNS = Name.NS ["Untyped"]
    , local = Name.mkLocal Name.emptyNS "Struct"
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

cgrToModules :: Raw.CgReq -> [Haskell.Module]
cgrToModules Raw.CgReq{files, typeMap} = map (fileToModule typeMap) files

fileToModule :: M.Map C.TypeId Raw.TypeRef -> Raw.File -> Haskell.Module
fileToModule env Raw.File{fileName, fileId, decls} =
    Haskell.Module
        { modName = makeModName fileName
        , modDecls = map (declToDecl fileId env) decls
        , modImports =
            [ Haskell.Import
                { importAs = "Untyped"
                , parts = ["Capnp", "Untyped"]
                }
            , Haskell.Import
                { importAs = "Basics"
                , parts = ["Capnp", "Basics"]
                }
            ]
        }

declToDecl :: Word64 -> M.Map C.TypeId Raw.TypeRef -> Raw.Decl -> Haskell.Decl
declToDecl _thisMod _env Raw.Enum{typeCtor, dataCtors} =
    Haskell.DataDecl
        { Haskell.dataName = Name.UnQ (Name.renderLocalQ typeCtor)
        , Haskell.dataVariants =
            map enumerantToVariant dataCtors
            ++
            [ Haskell.DataVariant
                { dvCtorName = Name.localToUnQ $ Name.mkSub typeCtor "unknown'"
                , dvArgs = Haskell.PosArgs
                    [ Haskell.PrimType $ C.PrimInt $ C.IntType C.Unsigned C.Sz16 ]
                }
            ]
        , Haskell.derives = [ "Std_.Show", "Std_.Eq" ]
        }
  where
    enumerantToVariant variantName =
        Haskell.DataVariant
            { dvCtorName =
                Name.localToUnQ variantName
            , dvArgs = Haskell.PosArgs []
            }
declToDecl _thisMod _env Raw.StructWrapper{ctorName} =
    let name = Name.localToUnQ ctorName
    in Haskell.NewtypeDecl
        { dataName = name
        , typeArgs = [ "msg" ]
        , dataVariant = Haskell.DataVariant
            { dvCtorName = name
            , dvArgs = Haskell.PosArgs
                [ Haskell.TypeApp
                    (Haskell.GlobalNamedType untypedStruct)
                    [Haskell.TypeVar "msg"]
                ]
            }
        , derives = []
        }
declToDecl thisMod env Raw.Getter{fieldName, fieldLocType, containerType} =
    Haskell.ValueDecl
        { name = Name.UnQ $
            "get_" <> Name.renderLocalQ fieldName
        , typ = Haskell.CtxType
            [readCtx "m" "msg"]
            (Haskell.FnType
                [ Haskell.TypeApp
                    (Haskell.LocalNamedType containerType)
                    [ Haskell.TypeVar "msg" ]
                , Haskell.TypeApp
                    (Haskell.TypeVar "m")
                    [ typeToType thisMod env (C.fieldType fieldLocType) "msg"
                    ]
                ]
            )
        }

typeToType :: Word64 -> M.Map C.TypeId Raw.TypeRef -> C.Type -> T.Text -> Haskell.Type
typeToType thisMod env ty var = case ty of
    C.VoidType -> Haskell.UnitType
    C.WordType (C.PrimWord ty) -> Haskell.PrimType ty
    C.WordType (C.EnumType typeId) ->
        getNamedType thisMod env typeId
    C.PtrType (C.ListOf elt) ->
        Haskell.TypeApp (basics "List")
            [ Haskell.TypeVar var
            , typeToType thisMod env elt var
            ]
    C.PtrType (C.PrimPtr C.PrimText) ->
        appV $ basics "Text"
    C.PtrType (C.PrimPtr C.PrimData) ->
        appV $ basics "Data"
    C.PtrType (C.PtrComposite (C.StructType typeId)) ->
        appV $ getNamedType thisMod env typeId
    C.PtrType (C.PtrInterface typeId) ->
        getNamedType thisMod env typeId
    C.PtrType (C.PrimPtr (C.PrimAnyPtr _)) ->
        appV $ Haskell.GlobalNamedType Name.GlobalQ
            { globalNS = Name.NS [ "Untyped" ]
            , local = Name.mkLocal Name.emptyNS "Ptr"
            }
    C.CompositeType (C.StructType typeId) ->
        appV $ getNamedType thisMod env typeId
  where
    appV t = Haskell.TypeApp t [Haskell.TypeVar var]
    basics name = Haskell.GlobalNamedType Name.GlobalQ
        { globalNS = Name.NS [ "Basics" ]
        , local = Name.mkLocal Name.emptyNS name
        }


getNamedType :: Word64 -> M.Map C.TypeId Raw.TypeRef -> C.TypeId -> Haskell.Type
getNamedType thisMod env typeId =
    let Raw.TypeRef{tyModule, tyName} =
            case M.lookup typeId env of
                Just v  -> v
                Nothing ->
                    -- hypothesis: typeId is for some type we currently skip.
                    error $ "Not found: " ++ show typeId
    in if tyModule == thisMod
        then Haskell.LocalNamedType tyName
        else Haskell.GlobalNamedType Name.GlobalQ
                { globalNS = Name.NS [ "Capnp", "Gen", T.pack $ printf "X%x" tyModule ]
                , local = tyName
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
