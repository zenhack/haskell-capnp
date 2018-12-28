{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Trans.FlatToPure where

import qualified IR.Common as C
import qualified IR.Flat   as Flat
import qualified IR.Name   as Name
import qualified IR.Pure   as Pure

fileToFile :: Flat.File -> Pure.File
fileToFile Flat.File{nodes, fileId, fileName} =
    Pure.File
        { fileId
        , fileName
        , decls = concatMap nodeToDecls nodes
        }

nodeToDecls :: Flat.Node -> [Pure.Decl]
nodeToDecls Flat.Node{name=Name.CapnpQ{local}, union_} = case union_ of
    Flat.Enum _ -> [] -- TODO
    Flat.Union{} -> [] -- TODO
    Flat.Interface{} -> [] -- TODO
    Flat.Struct{fields} ->
        [ Pure.DStruct
            { typeName = local
            , fields = map fieldToField fields
            }
        ]

fieldToField :: Flat.Field -> Pure.Field
fieldToField Flat.Field{fieldName, fieldLocType} = Pure.Field
    { name = Name.getUnQ fieldName
    , type_ = fmap
        (\Flat.Node{name} -> name)
        (C.fieldType fieldLocType)
    }
