{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.RawToHaskell (fileToModule) where

import Data.Char       (toUpper)
import System.FilePath (splitDirectories)

import qualified Data.Text as T

import IR.Common (PrimType(..))

import qualified IR.Haskell as Haskell
import qualified IR.Name    as Name
import qualified IR.Raw     as Raw

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
            ]
        }

declToDecl :: Raw.Decl -> Haskell.Decl
declToDecl Raw.Enum{typeCtor, dataCtors} =
    Haskell.DataDecl
        { Haskell.dataName = Name.UnQ (Name.renderLocalQ typeCtor)
        , Haskell.dataVariants = map enumerantToVariant dataCtors
        , Haskell.derives = [ "Show", "Eq", "Enum" ]
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
        , dataVariant = Haskell.DataVariant
            { dvCtorName = name
            , dvArgs = Haskell.PosArgs [ PTyVoid ] -- TODO: change this to struct.
            }
        , derives = [ "Show", "Eq" ]
        }
declToDecl Raw.Getter{fieldName} =
    Haskell.ValueDecl
        { name = Name.UnQ $
            "get_" <> Name.renderLocalQ fieldName
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
