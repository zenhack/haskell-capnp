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
fileToFile Flat.File{fileId, fileName, nodes} =
    New.File
        { fileId
        , fileName
        , decls = concatMap nodeToDecls nodes
        }

nodeToDecls :: Flat.Node -> [New.Decl]
nodeToDecls Flat.Node{nodeId, name=Name.CapnpQ{local}, typeParams, union_} =
    let mkType repr =
            New.TypeDecl
                { name = local
                , nodeId
                , params = map C.paramName typeParams
                , repr
                }
        mkField field =
            fieldToDecl local typeParams field
    in
    case union_ of
        Flat.Other       -> []
        Flat.Constant _  -> []
        Flat.Enum _      -> [ mkType (R.Data R.Sz16) ]
        Flat.Interface{} -> [ mkType (R.Ptr (Just R.Cap)) ]
        Flat.Struct{fields} ->
            mkType (R.Ptr (Just R.Struct))
            : map mkField fields

fieldToDecl :: Name.LocalQ -> [C.TypeParamRef Flat.Node]-> Flat.Field -> New.Decl
fieldToDecl containerType typeParams Flat.Field{fieldName, fieldLocType} =
    New.FieldDecl
        { containerType
        , typeParams = map C.paramName typeParams
        , fieldName = Name.getUnQ fieldName
        , fieldLocType = error "TODO" fieldLocType
        }
