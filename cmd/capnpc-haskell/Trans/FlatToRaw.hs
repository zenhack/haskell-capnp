{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Trans.FlatToRaw (cgrToFiles) where

import qualified IR.Common as C
import qualified IR.Flat   as Flat
import qualified IR.Name   as Name
import qualified IR.Raw    as Raw

cgrToFiles :: Flat.CodeGenReq -> [Raw.File]
cgrToFiles Flat.CodeGenReq{reqFiles} = map fileToFile reqFiles

fileToFile :: Flat.File -> Raw.File
fileToFile Flat.File{nodes, fileId, fileName} =
    Raw.File
        { fileName
        , fileId
        , decls = concatMap nodeToDecls nodes
        }

nodeToDecls :: Flat.Node -> [Raw.Decl]
nodeToDecls Flat.Node{name=Name.CapnpQ{fileId, local}, union_, typeParams} =
  let typeParams' = map C.paramName typeParams in
  case union_ of
    Flat.Enum variants ->
        [ Raw.Enum
            { typeCtor = local
            , dataCtors = map (Name.mkSub local) variants
            }
        ]
    Flat.Struct{fields, isGroup, dataWordCount, pointerCount, union} ->
        concat
            [ [ Raw.StructWrapper { typeCtor = local, typeParams = typeParams' } ]
            , if isGroup
                then
                    []
                else
                    [ Raw.StructInstances
                        { typeCtor = local
                        , typeParams = typeParams'
                        , dataWordCount
                        , pointerCount
                        }
                    ]
            , concatMap (fieldToDecls local typeParams') fields
            , case union of
                Nothing -> []
                Just Flat.Union{variants, tagOffset} ->
                    let local' = Name.mkSub local "" in
                    [ Raw.UnionVariant
                        { parentTypeCtor = local
                        , typeParams = typeParams'
                        , tagOffset
                        , unionDataCtors =
                            [ Raw.Variant
                                { name = local
                                , tagValue
                                , locType = C.bothMap (\Flat.Node{name} -> name) fieldLocType
                                }
                            | Flat.Variant
                                { field = Flat.Field
                                    { fieldName = Name.CapnpQ{local}
                                    , fieldLocType
                                    }
                                , tagValue
                                } <- variants
                            ]
                        }
                    , Raw.Getter
                        { fieldName = local'
                        , typeParams = typeParams'
                        , containerType = local
                        , fieldLocType = C.HereField $ C.StructType Name.CapnpQ
                            { fileId
                            , local = local'
                            }
                            (C.ListBrand
                                [ C.PtrParam C.TypeParamRef
                                    { paramName
                                    , paramIndex
                                    , paramScope = scopeName
                                    }
                                | C.TypeParamRef
                                    { paramName
                                    , paramIndex
                                    , paramScope=Flat.Node{name=scopeName}
                                    } <- typeParams
                                ]
                            )
                        }
                    ] ++
                    [ Raw.Setter
                        { fieldName
                        , typeParams = typeParams'
                        , containerType = local
                        , fieldLocType = C.bothMap (\Flat.Node{name} -> name) fieldLocType
                        , tag = Just Raw.TagSetter
                            { tagOffset
                            , tagValue
                            }
                        }
                    | Flat.Variant
                        { field = Flat.Field
                            { fieldName = Name.CapnpQ { local = fieldName }
                            , fieldLocType
                            }
                        , tagValue
                        } <- variants
                    ] ++
                    -- This is kindof another tag setter, but it has to work a bit
                    -- differently for the unknown' variant, because it takes an
                    -- argument for what the tag should be, but also doesn't need to
                    -- set any other values. We can treat is as just a setter for a
                    -- uint16 field in the same spot:
                    [ Raw.Setter
                        { fieldName = Name.mkSub local "unknown'"
                        , typeParams = typeParams'
                        , containerType = local
                        , fieldLocType = C.DataField
                            (Raw.tagOffsetToDataLoc tagOffset)
                            (C.PrimWord $ C.PrimInt $ C.IntType C.Unsigned C.Sz16)
                        , tag = Nothing
                        }
                    ]
        ]
    Flat.Interface{} ->
        [ Raw.InterfaceWrapper
            { typeCtor = local
            , typeParams = typeParams'
            }
        ]
    Flat.Constant{ value } ->
        [ Raw.Constant
            { name = local
            , value = C.bothMap (\Flat.Node{name} -> name) value
            }
        ]
    Flat.Other -> []

fieldToDecls :: Name.LocalQ -> [Name.UnQ] -> Flat.Field -> [Raw.Decl]
fieldToDecls
    containerType
    typeParams
    Flat.Field{fieldName=Name.CapnpQ{local=fieldName}, fieldLocType}
    =
    [ Raw.Getter
        { fieldName
        , typeParams
        , containerType
        , fieldLocType = C.bothMap (\Flat.Node{name} -> name) fieldLocType
        }
    ]
    ++
    case fieldLocType of
         C.HereField _ -> []
         _ ->
            [ Raw.Setter
                { fieldName
                , containerType
                , typeParams
                , fieldLocType = C.bothMap (\Flat.Node{name} -> name) fieldLocType
                , tag = Nothing
                }
            ]
    ++
    case fieldLocType of
        C.PtrField ptrIndex _ ->
            [ Raw.HasFn
                { fieldName
                , typeParams
                , containerType
                , ptrIndex
                }
            ]
        _ ->
            []
    ++
    case fieldLocType of
        C.PtrField _ (C.ListOf _) ->
            [ Raw.NewFn
                { fieldName
                , typeParams
                , containerType
                , fieldLocType = C.bothMap (\Flat.Node{name} -> name) fieldLocType
                , newFnType = Raw.NewList
                }
            ]
        C.PtrField _ (C.PrimPtr C.PrimText) ->
            [ Raw.NewFn
                { fieldName
                , typeParams
                , containerType
                , fieldLocType = C.bothMap (\Flat.Node{name} -> name) fieldLocType
                , newFnType = Raw.NewText
                }
            ]
        C.PtrField _ (C.PrimPtr C.PrimData) ->
            [ Raw.NewFn
                { fieldName
                , typeParams
                , containerType
                , fieldLocType = C.bothMap (\Flat.Node{name} -> name) fieldLocType
                , newFnType = Raw.NewData
                }
            ]
        C.PtrField _ (C.PtrComposite _) ->
            [ Raw.NewFn
                { fieldName
                , typeParams
                , containerType
                , fieldLocType = C.bothMap (\Flat.Node{name} -> name) fieldLocType
                , newFnType = Raw.NewStruct
                }
            ]
        _ ->
            []
