{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Trans.Stage1ToFlat where

import qualified IR.Flat   as Flat
import qualified IR.Name   as Name
import qualified IR.Stage1 as Stage1

fileToFile :: Stage1.File -> Flat.File
fileToFile Stage1.File{fileNodes} =
    Flat.File { nodes = concatMap (nodeToNodes Name.emptyNS) fileNodes }

nodeToNodes :: Name.NS -> (Name.UnQ, Stage1.Node) -> [(Name.LocalQ, Flat.Node)]
nodeToNodes ns (unQ, Stage1.Node{nodeNested, nodeUnion}) =
    let name = Name.mkLocal ns unQ
        kids = concatMap (nodeToNodes (Name.localQToNS name)) nodeNested
    in case nodeUnion of
        Stage1.Enum enumerants -> (name, Flat.Enum enumerants) : kids
        Stage1.Other           -> kids