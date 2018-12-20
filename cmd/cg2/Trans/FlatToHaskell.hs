{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.FlatToHaskell (fileToModule) where


import Data.Char       (toUpper)
import System.FilePath (splitDirectories)

import qualified Data.Text as T

import qualified IR.Flat    as Flat
import qualified IR.Haskell as Haskell
import qualified IR.Name    as Name

fileToModule :: Flat.File -> Haskell.Module
fileToModule Flat.File{nodes, fileName} =
    Haskell.Module
        { modName = makeModName fileName
        , modDecls = map nodeToDecl nodes
        }

nodeToDecl :: (Name.LocalQ, Flat.Node) -> Haskell.Decl
nodeToDecl (nodeName, Flat.Enum enumerants) =
    Haskell.DataDecl
        { Haskell.dataName = Name.UnQ (Name.renderLocalQ nodeName)
        , Haskell.dataVariants = map (enumerantToVariant nodeName) enumerants
        , Haskell.derives = [ "Show", "Eq", "Enum" ]
        }

enumerantToVariant :: Name.LocalQ -> Name.UnQ -> Haskell.DataVariant
enumerantToVariant nodeName variantName =
    Haskell.DataVariant
        { Haskell.dvCtorName =
            Name.UnQ $ Name.renderLocalQ $ Name.mkLocal (Name.localQToNS nodeName) variantName
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
