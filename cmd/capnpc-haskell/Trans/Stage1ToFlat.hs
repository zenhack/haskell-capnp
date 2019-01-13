{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
-- Translate from the 'Stage1' IR to the 'Flat' IR.
--
-- As the name of the latter suggests, this involves flattening the namepace.
module Trans.Stage1ToFlat (filesToFiles) where

import Data.Word

import qualified Data.Map as M

import qualified IR.Common as C
import qualified IR.Flat   as Flat
import qualified IR.Name   as Name
import qualified IR.Stage1 as Stage1

type NodeMap = M.Map Word64 Flat.Node

filesToFiles :: [Stage1.File] -> [Flat.File]
filesToFiles inFiles = outFiles
  where
    outFiles = map (fileToFile nodeMap) inFiles
    allNodes = concat [nodes | Flat.File{nodes} <- outFiles]
    nodeMap = M.fromList [(nodeId, node) | node@Flat.Node{nodeId} <- allNodes]

fileToFile :: NodeMap -> Stage1.File -> Flat.File
fileToFile nodeMap Stage1.File{fileNodes, fileName, fileId, fileImports} =
    Flat.File
        { nodes = nodesToNodes nodeMap fileId fileNodes
        , fileName
        , fileId
        , fileImports
        }

nodesToNodes :: NodeMap -> Word64 -> [(Name.UnQ, Stage1.Node)] -> [Flat.Node]
nodesToNodes nodeMap thisMod = concatMap (go Name.emptyNS)
  where
    go ns (unQName, Stage1.Node{nodeId, nodeNested, nodeUnion}) =
        let localName = Name.mkLocal ns unQName
            kidsNS = Name.localQToNS localName
            kids = concatMap (go kidsNS) nodeNested
            name = Name.CapnpQ
                { local = localName
                , fileId = thisMod
                }
            mine = case nodeUnion of
                Stage1.NodeEnum enumerants ->
                    [ Flat.Node
                        { name
                        , nodeId
                        , union_ = Flat.Enum enumerants
                        }
                    ]
                Stage1.NodeStruct Stage1.Struct
                        { fields
                        , isGroup
                        , dataWordCount
                        , pointerCount
                        , tagOffset
                        } ->
                    let
                        mkField fieldUnQ locType =
                            Flat.Field
                                { fieldName = Name.mkSub name fieldUnQ
                                , fieldLocType = fmap
                                    (\Stage1.Node{nodeId} -> nodeMap M.! nodeId)
                                    locType
                                }
                        variants =
                            [ Flat.Variant
                                { field = mkField fieldUnQ locType
                                , tagValue
                                }
                            | Stage1.Field{name=fieldUnQ, locType, tag=Just tagValue} <- fields
                            ]
                        commonFields =
                            [ mkField fieldUnQ locType
                            | Stage1.Field{name=fieldUnQ, locType, tag=Nothing} <- fields
                            ]
                        fieldNodes =
                            concatMap (fieldToNodes kidsNS) fields

                        commonNode =
                            Flat.Node
                                { name
                                , nodeId
                                , union_ = Flat.Struct
                                    { fields = commonFields
                                    , union =
                                        if null variants then
                                            Nothing
                                        else
                                            Just Flat.Union
                                                { variants
                                                , tagOffset
                                                }
                                    , isGroup
                                    , dataWordCount
                                    , pointerCount
                                    }
                                }
                    in
                    commonNode : fieldNodes
                Stage1.NodeInterface { methods, supers } ->
                    Flat.Node
                        { name
                        , nodeId
                        , union_ = Flat.Interface
                            { methods =
                                [ let Flat.Node{name=paramName} = nodeMap M.! paramId
                                      Flat.Node{name=resultName} = nodeMap M.! resultId
                                  in Flat.Method
                                        { name
                                        , paramType = paramName
                                        , resultType = resultName
                                        }
                                | Stage1.Method
                                    { name
                                    , paramType=Stage1.Node{nodeId=paramId}
                                    , resultType=Stage1.Node{nodeId=resultId}
                                    }
                                <- methods
                                ]
                            , supers =
                                [ nodeMap M.! nodeId | Stage1.Node{nodeId} <- supers ]
                            }
                        }
                    : concatMap (methodToNodes kidsNS) methods

                Stage1.NodeConstant value ->
                    [ Flat.Node
                        { name = Name.CapnpQ
                            { local = Name.unQToLocal $ Name.valueName localName
                            , fileId = thisMod
                            }
                        , nodeId
                        , union_ = Flat.Constant
                            { value = fmap
                                (\Stage1.Node{nodeId} -> nodeMap M.! nodeId)
                                value
                            }
                        }
                    ]
                Stage1.NodeOther ->
                    []
        in mine ++ kids
    fieldToNodes ns Stage1.Field{name, locType} = case locType of
        C.HereField
            (C.StructType
                struct@Stage1.Node
                    { nodeUnion = Stage1.NodeStruct Stage1.Struct{isGroup=True}
                    }
            ) -> go ns (name, struct)
        _ ->
            []
    methodToNodes ns Stage1.Method{ name, paramType, resultType } =
        -- If the parameter and result types are anonymous, we need to generate
        -- structs for them.
        let maybeAnon ty suffix =
                case ty of
                    Stage1.Node{nodeParent=Nothing} ->
                        let localName = Name.mkLocal ns name
                            kidsNS = Name.localQToNS localName
                        in
                        go kidsNS (suffix, ty)
                    _ ->
                        []
        in
        maybeAnon paramType "params" ++ maybeAnon resultType "results"
