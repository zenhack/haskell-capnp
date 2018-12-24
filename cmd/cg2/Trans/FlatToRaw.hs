{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Trans.FlatToRaw (fileToFile) where

import qualified IR.Flat as Flat
import qualified IR.Name as Name
import qualified IR.Raw  as Raw

fileToFile :: Flat.File -> Raw.File
fileToFile Flat.File{nodes, fileId, fileName} =
    Raw.File
        { fileName
        , fileId
        , decls = concatMap nodeToDecls nodes
        }

nodeToDecls :: Flat.Node -> [Raw.Decl]
nodeToDecls Flat.Node{name=Name.CapnpQ{local}, union_} = case union_ of
    Flat.Enum variants ->
        [ Raw.Enum
            { typeCtor = local
            , dataCtors = map (Name.mkSub local) variants
            }
        ]
    Flat.Struct{fields, isGroup, dataWordCount, pointerCount} ->
        concat
            [ [ Raw.StructWrapper { ctorName = local } ]
            , if isGroup
                then
                    []
                else
                    [ Raw.StructInstances
                        { ctorName = local
                        , dataWordCount
                        , pointerCount
                        }
                    ]
            , concatMap (fieldToDecls local) fields
            ]
    Flat.Union{} ->
        [ -- TODO
        ]
    Flat.Interface ->
        [ Raw.InterfaceWrapper
            { ctorName = local
            }
        ]

fieldToDecls :: Name.LocalQ -> Flat.Field -> [Raw.Decl]
fieldToDecls containerType Flat.Field{fieldName, fieldLocType} =
    [ Raw.Getter
        { fieldName = Name.mkSub containerType (Name.getUnQ fieldName)
        , containerType
        , fieldLocType = fmap Flat.name fieldLocType
        }
    ]
