{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Trans.FlatToNew (cgrToFiles) where

import qualified Capnp.Repr as R
import qualified IR.Common  as C
import qualified IR.Flat    as Flat
import qualified IR.Name    as Name
import qualified IR.New     as New

cgrToFiles :: Flat.CodeGenReq -> [New.File]
cgrToFiles = map fileToFile . Flat.reqFiles

fileToFile :: Flat.File -> New.File
fileToFile Flat.File{fileId, nodes} =
    New.File
        { fileId
        , decls = concatMap nodeToDecls nodes
        }

nodeToDecls :: Flat.Node -> [New.Decl]
nodeToDecls Flat.Node{nodeId, name=Name.CapnpQ{local}, typeParams, union_} =
    let mkType repr =
            [ New.TypeDecl
                { name = local
                , nodeId
                , params = map C.paramName typeParams
                , repr
                }
            ]
    in
    case union_ of
        Flat.Other       -> []
        Flat.Constant _  -> []
        Flat.Enum _      -> mkType (R.Data R.Sz16)
        Flat.Struct{}    -> mkType (R.Ptr (Just R.Struct))
        Flat.Interface{} -> mkType (R.Ptr (Just R.Cap))
