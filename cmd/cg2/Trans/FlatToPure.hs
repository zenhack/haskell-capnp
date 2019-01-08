{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.FlatToPure where

import qualified IR.Common as C
import qualified IR.Flat   as Flat
import qualified IR.Name   as Name
import qualified IR.Pure   as Pure

fileToFile :: Flat.File -> Pure.File
fileToFile Flat.File{nodes, fileId, fileName, fileImports} =
    Pure.File
        { fileId
        , fileName
        , fileImports
        , decls = concatMap nodeToDecls nodes
        }

nodeToDecls :: Flat.Node -> [Pure.Decl]
nodeToDecls Flat.Node{name=name@Name.CapnpQ{local}, union_} = case union_ of
    Flat.Enum _ -> [] -- TODO
    Flat.Union{variants} ->
        [ Pure.Data
            { typeName = local
            , variants =
                [ Pure.Variant
                    { name = variantName
                    , fields = case fieldLocType of
                        C.VoidField -> []
                        _           -> [fieldToField field]
                    }
                | Flat.Variant
                    { field=field@Flat.Field
                        { fieldName=Name.CapnpQ{local=variantName}
                        , fieldLocType
                        }
                    } <- variants
                ]
            , isUnion = True
            }
        ]
    Flat.Interface{} -> [] -- TODO
    Flat.Struct{fields, union} ->
        Pure.Data
            { typeName = local
            , variants =
                [ Pure.Variant
                    { name = local
                    , fields =
                        map fieldToField fields
                        ++ case union of
                            Nothing ->
                                []
                            Just Flat.Node{union_=Flat.Union{}} ->
                                [ Pure.Field
                                    { name = "union'"
                                    , type_ = C.CompositeType $ C.StructType $ Name.mkSub name ""
                                    }
                                ]
                            Just _ ->
                                error "This should never happen!"
                    }
                ]
            , isUnion = False
            }
        : case union of
            Just u ->
                nodeToDecls u { Flat.name = Name.mkSub name "" }
            Nothing ->
                []

fieldToField :: Flat.Field -> Pure.Field
fieldToField Flat.Field{fieldName, fieldLocType} = Pure.Field
    { name = Name.getUnQ fieldName
    , type_ = fmap
        (\Flat.Node{name} -> name)
        (C.fieldType fieldLocType)
    }
