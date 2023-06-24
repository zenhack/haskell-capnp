{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Translate from the 'Stage1' IR to the 'Flat' IR.
--
-- As the name of the latter suggests, this involves flattening the namepace.
module Trans.Stage1ToFlat (cgrToCgr) where

import qualified Data.Map as M
import Data.Word
import qualified IR.Common as C
import qualified IR.Flat as Flat
import qualified IR.Name as Name
import qualified IR.Stage1 as Stage1

type NodeMap = M.Map Word64 Flat.Node

cgrToCgr :: Stage1.CodeGenReq -> Flat.CodeGenReq
cgrToCgr Stage1.CodeGenReq {allFiles, reqFiles = inFiles} =
  Flat.CodeGenReq
    { reqFiles = outFiles,
      allNodes
    }
  where
    outFiles = map (reqFileToFile fileMap) inFiles
    fileMap =
      M.fromList
        [ (fileId, fileToNodes nodeMap file)
          | file@Stage1.File {fileId} <- allFiles
        ]
    allNodes = concatMap snd (M.toList fileMap)
    nodeMap = M.fromList [(nodeId, node) | node@Flat.Node {nodeId} <- allNodes]

fileToNodes :: NodeMap -> Stage1.File -> [Flat.Node]
fileToNodes nodeMap Stage1.File {fileNodes, fileId} =
  concatMap
    ( \(unQ, node) ->
        nestedToNodes nodeMap fileId (Name.unQToLocal unQ) node []
    )
    fileNodes

reqFileToFile :: M.Map Word64 [Flat.Node] -> Stage1.ReqFile -> Flat.File
reqFileToFile fileMap Stage1.ReqFile {fileName, file = Stage1.File {fileId}} =
  Flat.File
    { nodes = fileMap M.! fileId,
      fileName,
      fileId
    }

paramsToParams :: NodeMap -> Word64 -> [Name.UnQ] -> [C.TypeParamRef Flat.Node]
paramsToParams nodeMap nodeId names =
  [ C.TypeParamRef
      { paramName = name,
        paramIndex = i,
        paramScope = nodeMap M.! nodeId
      }
    | (i, name) <- zip [0 ..] names
  ]

applyBrandNode :: C.MapBrand Flat.Node -> Flat.Node -> C.ListBrand Flat.Node
applyBrandNode m Flat.Node {typeParams} = applyBrandParams typeParams m

applyBrandParams :: [C.TypeParamRef Flat.Node] -> C.MapBrand Flat.Node -> C.ListBrand Flat.Node
applyBrandParams params m = C.ListBrand $ map (`applyBrandParam` m) params

applyBrandParam ::
  C.TypeParamRef Flat.Node ->
  C.MapBrand Flat.Node ->
  C.PtrType (C.ListBrand Flat.Node) Flat.Node
applyBrandParam param@C.TypeParamRef {paramIndex, paramScope = Flat.Node {nodeId}} (C.MapBrand m) =
  case M.lookup nodeId m of
    Nothing -> C.PtrParam param
    Just (C.Bind bindings) ->
      let binding = bindings !! paramIndex
       in case binding of
            C.Unbound -> C.PtrParam param
            C.BoundType ty -> applyBrandPtrType ty

type ApplyBrandFn f =
  f (C.MapBrand Flat.Node) Flat.Node ->
  f (C.ListBrand Flat.Node) Flat.Node

applyBrandCompositeType :: ApplyBrandFn C.CompositeType
applyBrandCompositeType (C.StructType n b) = C.StructType n (applyBrandNode b n)

applyBrandInterfaceType :: ApplyBrandFn C.InterfaceType
applyBrandInterfaceType (C.InterfaceType n b) = C.InterfaceType n (applyBrandNode b n)

applyBrandValue :: ApplyBrandFn C.Value
applyBrandValue = \case
  C.VoidValue -> C.VoidValue
  C.WordValue v t -> C.WordValue v t
  C.PtrValue t p -> C.PtrValue (applyBrandPtrType t) p

applyBrandPtrType :: ApplyBrandFn C.PtrType
applyBrandPtrType = \case
  C.ListOf t -> C.ListOf $ applyBrandType t
  C.PrimPtr p -> C.PrimPtr p
  C.PtrInterface t -> C.PtrInterface (applyBrandInterfaceType t)
  C.PtrComposite t -> C.PtrComposite (applyBrandCompositeType t)
  C.PtrParam p -> C.PtrParam p

applyBrandType :: ApplyBrandFn C.Type
applyBrandType = \case
  C.CompositeType t -> C.CompositeType $ applyBrandCompositeType t
  C.VoidType -> C.VoidType
  C.WordType t -> C.WordType t
  C.PtrType t -> C.PtrType $ applyBrandPtrType t

applyBrandFieldLocType :: ApplyBrandFn C.FieldLocType
applyBrandFieldLocType = \case
  C.DataField l t -> C.DataField l t
  C.PtrField i t -> C.PtrField i $ applyBrandPtrType t
  C.HereField t -> C.HereField $ applyBrandCompositeType t
  C.VoidField -> C.VoidField

-- | Generate @'Flat.Node'@s from a 'Stage1.Node' and its local name.
nestedToNodes :: NodeMap -> Word64 -> Name.LocalQ -> Stage1.Node -> [C.TypeParamRef Flat.Node] -> [Flat.Node]
nestedToNodes
  nodeMap
  thisMod
  localName
  Stage1.Node
    { nodeCommon = Stage1.NodeCommon {nodeId, nodeNested, nodeParams},
      nodeUnion
    }
  typeParams =
    mine ++ kids
    where
      myParams = typeParams ++ paramsToParams nodeMap nodeId nodeParams
      kidsNS = Name.localQToNS localName
      kids =
        concatMap
          ( \(unQ, node) ->
              nestedToNodes nodeMap thisMod (Name.mkLocal kidsNS unQ) node myParams
          )
          nodeNested
      name =
        Name.CapnpQ
          { local = localName,
            fileId = thisMod
          }
      mine = case nodeUnion of
        Stage1.NodeEnum enumerants ->
          [ Flat.Node
              { name,
                nodeId,
                typeParams = myParams,
                union_ = Flat.Enum enumerants
              }
          ]
        Stage1.NodeStruct struct ->
          structToNodes nodeMap thisMod nodeId name kidsNS struct myParams
        Stage1.NodeInterface iface ->
          interfaceToNodes nodeMap thisMod nodeId name kidsNS iface myParams
        Stage1.NodeConstant value ->
          [ Flat.Node
              { name =
                  Name.CapnpQ
                    { local = Name.unQToLocal $ Name.valueName localName,
                      fileId = thisMod
                    },
                nodeId,
                union_ =
                  Flat.Constant
                    { value =
                        applyBrandValue $
                          C.bothMap
                            (\Stage1.Node {nodeCommon = Stage1.NodeCommon {nodeId}} -> nodeMap M.! nodeId)
                            value
                    },
                typeParams = myParams
              }
          ]
        Stage1.NodeOther ->
          [ Flat.Node
              { name,
                nodeId,
                union_ = Flat.Other,
                typeParams = myParams
              }
          ]

interfaceToNodes :: NodeMap -> Word64 -> Word64 -> Name.CapnpQ -> Name.NS -> Stage1.Interface -> [C.TypeParamRef Flat.Node] -> [Flat.Node]
interfaceToNodes nodeMap thisMod nodeId name kidsNS Stage1.Interface {methods, supers} typeParams =
  let translateSuper =
        applyBrandInterfaceType
          . C.bothMap (\Stage1.Node {nodeCommon = Stage1.NodeCommon {nodeId}} -> nodeMap M.! nodeId)
      prType =
        applyBrandCompositeType
          . C.bothMap
            ( \Stage1.Node {nodeCommon = Stage1.NodeCommon {nodeId}} ->
                nodeMap M.! nodeId
            )
   in Flat.Node
        { name,
          nodeId,
          union_ =
            Flat.Interface
              { methods =
                  [ Flat.Method
                      { name,
                        paramType = prType paramType,
                        resultType = prType resultType
                      }
                    | Stage1.Method {name, paramType, resultType} <- methods
                  ],
                supers = map translateSuper supers
              },
          typeParams
        }
        : concatMap (\method -> methodToNodes nodeMap thisMod kidsNS method typeParams) methods

structToNodes :: NodeMap -> Word64 -> Word64 -> Name.CapnpQ -> Name.NS -> Stage1.Struct -> [C.TypeParamRef Flat.Node] -> [Flat.Node]
structToNodes
  nodeMap
  thisMod
  nodeId
  name
  kidsNS
  Stage1.Struct
    { fields,
      isGroup,
      dataWordCount,
      pointerCount,
      tagOffset
    }
  typeParams =
    let mkField fieldUnQ locType =
          Flat.Field
            { fieldName = Name.mkSub name fieldUnQ,
              fieldLocType =
                applyBrandFieldLocType $
                  C.bothMap
                    (\Stage1.Node {nodeCommon = Stage1.NodeCommon {nodeId}} -> nodeMap M.! nodeId)
                    locType
            }
        variants =
          [ Flat.Variant
              { field = mkField fieldUnQ locType,
                tagValue
              }
            | Stage1.Field {name = fieldUnQ, locType, tag = Just tagValue} <- fields
          ]
        commonFields =
          [ mkField fieldUnQ locType
            | Stage1.Field {name = fieldUnQ, locType, tag = Nothing} <- fields
          ]
        fieldNodes =
          concatMap (fieldToNodes nodeMap thisMod kidsNS typeParams) fields

        commonNode =
          Flat.Node
            { name,
              nodeId,
              union_ =
                Flat.Struct
                  { fields = commonFields,
                    union =
                      if null variants
                        then Nothing
                        else
                          Just
                            Flat.Union
                              { variants,
                                tagOffset
                              },
                    isGroup,
                    dataWordCount,
                    pointerCount
                  },
              typeParams
            }
     in commonNode : fieldNodes

fieldToNodes :: NodeMap -> Word64 -> Name.NS -> [C.TypeParamRef Flat.Node] -> Stage1.Field -> [Flat.Node]
fieldToNodes nodeMap thisMod ns typeParams Stage1.Field {name, locType} = case locType of
  C.HereField
    ( C.StructType
        struct@Stage1.Node
          { nodeUnion = Stage1.NodeStruct Stage1.Struct {isGroup = True}
          }
        _ -- Type parameters will be the same as the enclosing scope.
      ) -> nestedToNodes nodeMap thisMod (Name.mkLocal ns name) struct typeParams
  _ ->
    []

methodToNodes :: NodeMap -> Word64 -> Name.NS -> Stage1.Method -> [C.TypeParamRef Flat.Node] -> [Flat.Node]
methodToNodes nodeMap thisMod ns Stage1.Method {name, paramType, resultType} typeParams =
  -- If the parameter and result types are anonymous, we need to generate
  -- structs for them.
  let maybeAnon ty suffix =
        case ty of
          C.StructType node@Stage1.Node {nodeCommon = Stage1.NodeCommon {nodeParent = Nothing}} _ ->
            let localName = Name.mkLocal ns name
                kidsNS = Name.localQToNS localName
             in nestedToNodes nodeMap thisMod (Name.mkLocal kidsNS suffix) node typeParams
          _ ->
            []
   in maybeAnon paramType "params" ++ maybeAnon resultType "results"
