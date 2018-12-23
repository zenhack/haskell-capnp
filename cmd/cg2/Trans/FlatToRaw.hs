{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Trans.FlatToRaw (filesToCgr) where

import qualified Data.Map.Strict as M

import qualified IR.Common as C
import qualified IR.Flat   as Flat
import qualified IR.Name   as Name
import qualified IR.Raw    as Raw

filesToCgr :: [Flat.File] -> Raw.CgReq
filesToCgr files =
    Raw.CgReq
        { files = map fileToFile files
        , typeMap = M.unions $ map collectTypes files
        }

collectTypes :: Flat.File -> M.Map C.TypeId Raw.TypeRef
collectTypes Flat.File{fileId, nodes} = M.fromList $ map collectNode nodes
  where
    collectNode Flat.Node{name, nodeId} =
        ( C.TypeId nodeId
        , Raw.TypeRef{ tyName = name, tyModule = fileId }
        )

fileToFile :: Flat.File -> Raw.File
fileToFile Flat.File{nodes, fileId, fileName} =
    Raw.File
        { fileName
        , fileId
        , decls = concatMap nodeToDecls nodes
        }

nodeToDecls :: Flat.Node -> [Raw.Decl]
nodeToDecls Flat.Node{name, union_} = case union_ of
    Flat.Enum variants ->
        [ Raw.Enum
            { typeCtor = name
            , dataCtors = map (Name.mkSub name) variants
            }
        ]
    Flat.Struct{fields} ->
        Raw.StructWrapper
            { ctorName = name
            }
        : concatMap (fieldToDecls name) fields
    Flat.Interface ->
        [ Raw.InterfaceWrapper
            { ctorName = name
            }
        ]

fieldToDecls :: Name.LocalQ -> Flat.Field -> [Raw.Decl]
fieldToDecls containerType Flat.Field{fieldName, fieldLocType} =
    [ Raw.Getter
        { fieldName = Name.mkSub containerType fieldName
        , containerType
        , fieldLocType
        }
    ]
