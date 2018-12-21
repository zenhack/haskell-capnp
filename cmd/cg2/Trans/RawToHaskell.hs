{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.RawToHaskell (fileToModule) where

import Data.Char       (toUpper)
import System.FilePath (splitDirectories)

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
fileToModule Raw.File{fileName, decls} =
    Haskell.Module
        { modName = makeModName fileName
        , modDecls = map declToDecl decls
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

declToDecl :: Raw.Decl -> Haskell.Decl
declToDecl Raw.Enum{typeCtor, dataCtors} =
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
declToDecl Raw.StructWrapper{ctorName} =
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
declToDecl Raw.Getter{fieldName, fieldLocType, containerType} =
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
                    [ typeToType (C.fieldType fieldLocType) "msg"
                    ]
                ]
            )
        }

typeToType :: C.Type r -> T.Text -> Haskell.Type
typeToType C.VoidType _var = Haskell.UnitType
typeToType (C.WordType (C.PrimWord ty)) _var = Haskell.PrimType ty
typeToType (C.PtrType (C.PrimPtr C.PrimText)) var =
    Haskell.TypeApp
        ( Haskell.GlobalNamedType Name.GlobalQ
            { globalNS = Name.NS [ "Basics" ]
            , local = Name.mkLocal Name.emptyNS "Text"
            }
        )
        [Haskell.TypeVar var]
typeToType ty _ = error $ "TODO: " ++ show ty

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
