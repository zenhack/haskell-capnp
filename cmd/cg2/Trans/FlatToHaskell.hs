{-# LANGUAGE OverloadedStrings #-}
module Trans.FlatToHaskell where

import qualified IR.Flat    as Flat
import qualified IR.Haskell as Haskell
import qualified IR.Name    as Name

fileToModule :: Flat.File -> Haskell.Module
fileToModule file =
    Haskell.Module
        { Haskell.modDecls = map nodeToDecl (Flat.nodes file)
        }

nodeToDecl :: (Name.LocalQ, Flat.Node) -> Haskell.Decl
nodeToDecl (nodeName, Flat.Enum enumerants) =
    Haskell.DataDecl
        { Haskell.dataName = Name.UnQ (Name.renderLocalQ nodeName)
        , Haskell.dataVariants = map Haskell.DataVariant enumerants
        , Haskell.derives = [ "Show", "Eq", "Enum" ]
        }
