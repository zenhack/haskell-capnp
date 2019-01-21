{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
-- Translate from the 'Stage1' IR to the 'Flat' IR.
--
-- As the name of the latter suggests, this involves flattening the namepace.
module Trans.Stage1ToFlat (filesToFiles) where

import Data.Word

import qualified Data.Map as M
import qualified Data.Set as S

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
nodesToNodes nodeMap thisMod = concatMap $ \(unQ, node) ->
    nestedToNodes nodeMap thisMod (Name.unQToLocal unQ) node

-- | Generate @'Flat.Node'@s from a 'Stage1.Node' and its local name.
nestedToNodes :: NodeMap -> Word64 -> Name.LocalQ -> Stage1.Node -> [Flat.Node]
nestedToNodes
    nodeMap
    thisMod
    localName
    Stage1.Node
        { nodeCommon = Stage1.NodeCommon{nodeId, nodeNested}
        , nodeUnion
        }
    =
    mine ++ kids
  where
    kidsNS = Name.localQToNS localName
    kids = concatMap
            (\(unQ, node) ->
                nestedToNodes nodeMap thisMod (Name.mkLocal kidsNS unQ) node
            )
            nodeNested
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
        Stage1.NodeStruct struct ->
            structToNodes nodeMap thisMod nodeId name kidsNS struct
        Stage1.NodeInterface iface ->
            interfaceToNodes nodeMap thisMod nodeId name kidsNS iface
        Stage1.NodeConstant value ->
            [ Flat.Node
                { name = Name.CapnpQ
                    { local = Name.unQToLocal $ Name.valueName localName
                    , fileId = thisMod
                    }
                , nodeId
                , union_ = Flat.Constant
                    { value = fmap
                        (\Stage1.Node{nodeCommon=Stage1.NodeCommon{nodeId}} -> nodeMap M.! nodeId)
                        value
                    }
                }
            ]
        Stage1.NodeOther ->
            []

interfaceToNodes :: NodeMap -> Word64 -> Word64 -> Name.CapnpQ -> Name.NS -> Stage1.Interface -> [Flat.Node]
interfaceToNodes nodeMap thisMod nodeId name kidsNS iface@Stage1.Interface{ methods, supers } =
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
                    , paramType=Stage1.Node{nodeCommon=Stage1.NodeCommon{nodeId=paramId}}
                    , resultType=Stage1.Node{nodeCommon=Stage1.NodeCommon{nodeId=resultId}}
                    }
                <- methods
                ]
            , supers =
                [ nodeMap M.! nodeId | Stage1.Node{nodeCommon=Stage1.NodeCommon{nodeId}} <- supers ]
            , ancestors =
                [ nodeMap M.! supId
                | supId <- S.toList (collectAncestors iface)
                ]
            }
        }
    : concatMap (methodToNodes nodeMap thisMod kidsNS) methods

structToNodes :: NodeMap -> Word64 -> Word64 -> Name.CapnpQ -> Name.NS -> Stage1.Struct -> [Flat.Node]
structToNodes
    nodeMap
    thisMod
    nodeId
    name
    kidsNS
    Stage1.Struct
            { fields
            , isGroup
            , dataWordCount
            , pointerCount
            , tagOffset
            } =
        let
            mkField fieldUnQ locType =
                Flat.Field
                    { fieldName = Name.mkSub name fieldUnQ
                    , fieldLocType = fmap
                        (\Stage1.Node{nodeCommon=Stage1.NodeCommon{nodeId}} -> nodeMap M.! nodeId)
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
                concatMap (fieldToNodes nodeMap thisMod kidsNS) fields

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

fieldToNodes :: NodeMap -> Word64 -> Name.NS -> Stage1.Field -> [Flat.Node]
fieldToNodes nodeMap thisMod ns Stage1.Field{name, locType} = case locType of
    C.HereField
        (C.StructType
            struct@Stage1.Node
                { nodeUnion = Stage1.NodeStruct Stage1.Struct{isGroup=True}
                }
        ) -> nestedToNodes nodeMap thisMod (Name.mkLocal ns name) struct
    _ ->
        []

methodToNodes :: NodeMap -> Word64 -> Name.NS -> Stage1.Method -> [Flat.Node]
methodToNodes nodeMap thisMod ns Stage1.Method{ name, paramType, resultType } =
    -- If the parameter and result types are anonymous, we need to generate
    -- structs for them.
    let maybeAnon ty suffix =
            case ty of
                Stage1.Node{nodeCommon=Stage1.NodeCommon{nodeParent=Nothing}} ->
                    let localName = Name.mkLocal ns name
                        kidsNS = Name.localQToNS localName
                    in
                    nestedToNodes nodeMap thisMod (Name.mkLocal kidsNS suffix) ty
                _ ->
                    []
    in
    maybeAnon paramType "params" ++ maybeAnon resultType "results"


-- | Collect the ids of of the ancestors of an interface, not including itself.
collectAncestors :: Stage1.Interface -> S.Set Word64
collectAncestors Stage1.Interface{supers} =
    -- This could be made faster, in that if there are shared ancestors
    -- (diamonds) we'll traverse them twice, but this is more
    -- straightforward, and with typical interface hierarchies it shouldn't
    -- be a huge problem.
    S.unions $
        S.fromList [ nodeId | Stage1.Node{nodeCommon=Stage1.NodeCommon{nodeId}} <- supers ]
        :
        [ collectAncestors iface | Stage1.Node{nodeUnion=Stage1.NodeInterface iface} <- supers ]
