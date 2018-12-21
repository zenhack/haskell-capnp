{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Trans.FlatToRaw where

import qualified IR.Flat as Flat
import qualified IR.Name as Name
import qualified IR.Raw  as Raw

fileToFile :: Flat.File -> Raw.File
fileToFile Flat.File{nodes, fileId, fileName} =
    Raw.File
        { fileName
        , fileId
        , decls = concatMap (uncurry nodeToDecls) nodes
        }

nodeToDecls :: Name.LocalQ -> Flat.Node -> [Raw.Decl]
nodeToDecls name (Flat.Enum variants) =
    [ Raw.Enum
        { typeCtor = name
        , dataCtors = map (Name.mkSub name) variants
        }
    ]
nodeToDecls name Flat.Struct{fields} =
    Raw.StructWrapper
        { ctorName = name
        }
    : concatMap (fieldToDecls name) fields

fieldToDecls :: Name.LocalQ -> Flat.Field -> [Raw.Decl]
fieldToDecls containerType Flat.Field{fieldName, fieldLocType} =
    [ Raw.Getter
        { fieldName = Name.mkSub containerType fieldName
        , containerType
        , fieldLocType
        }
    ]
