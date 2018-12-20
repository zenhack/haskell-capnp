-- | Module: Trans.CgrToStage1
-- Description: Translate from schema.capnp's codegenerator request to IR.Stage1.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Trans.CgrToStage1 (cgrToFiles) where

import Data.Word

import Data.Maybe (catMaybes)

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V

import IR.Common (IntSize(..), PrimType(..), Sign(..))

import qualified Capnp.Gen.Capnp.Schema.Pure as Schema
import qualified IR.Name                     as Name
import qualified IR.Stage1                   as Stage1

type NodeMap = M.Map Word64 Schema.Node

enumerantToName :: Schema.Enumerant -> Name.UnQ
enumerantToName Schema.Enumerant{name} = Name.UnQ name

fieldToField :: Schema.Field -> Maybe Stage1.Field
fieldToField Schema.Field{name, discriminantValue, union'} =
    case union' of
        Schema.Field'group{} ->
            Nothing
        Schema.Field'unknown' _ ->
            Nothing
        Schema.Field'slot{type_} ->
            flip fmap (typeToType type_) $ \typ ->
                Stage1.Field
                    { name = Name.UnQ name
                    , typ
                    , tag =
                        if discriminantValue == Schema.field'noDiscriminant then
                            Nothing
                        else
                            Just discriminantValue
                    }

nestedToNPair :: NodeMap -> Schema.Node'NestedNode -> (Name.UnQ, Stage1.Node)
nestedToNPair nodeMap Schema.Node'NestedNode{name, id} =
    ( Name.UnQ name
    , nodeToNode nodeMap (nodeMap M.! id)
    )

nodeToNode :: NodeMap -> Schema.Node -> Stage1.Node
nodeToNode nodeMap Schema.Node{id} =
    let Schema.Node{nestedNodes, union'} = nodeMap M.! id
    in Stage1.Node
        { nodeNested = map (nestedToNPair nodeMap) (V.toList nestedNodes)
        , nodeUnion = case union' of
            Schema.Node'enum enumerants ->
                Stage1.NodeEnum $ map enumerantToName $ V.toList enumerants
            Schema.Node'struct
                    { dataWordCount
                    , pointerCount
                    , isGroup
                    , discriminantOffset
                    , fields
                    } ->
                Stage1.NodeStruct Stage1.Struct
                    { dataWordCount
                    , pointerCount
                    , isGroup
                    , tagOffset = discriminantOffset
                    , fields = catMaybes $ map fieldToField (V.toList fields)
                    }
            _ ->
                Stage1.NodeOther
        }

reqFileToFile :: NodeMap -> Schema.CodeGeneratorRequest'RequestedFile -> Stage1.File
reqFileToFile nodeMap Schema.CodeGeneratorRequest'RequestedFile{id, filename} =
    let Stage1.Node{nodeNested} = nodeToNode nodeMap (nodeMap M.! id)
    in Stage1.File
        { fileNodes = nodeNested
        , fileName = T.unpack filename
        , fileId = id
        }

cgrToFiles :: Schema.CodeGeneratorRequest -> [Stage1.File]
cgrToFiles Schema.CodeGeneratorRequest{nodes, requestedFiles} =
    let nodeMap = M.fromList [(id, node) | node@Schema.Node{id} <- V.toList nodes]
    in map (reqFileToFile nodeMap) $ V.toList requestedFiles

typeToType :: Schema.Type -> Maybe Stage1.FieldType
typeToType Schema.Type'void    = Just $ Stage1.Prim PTyVoid
typeToType Schema.Type'bool    = Just $ Stage1.Prim PTyBool
typeToType Schema.Type'int8    = Just $ Stage1.Prim (PTyInt Signed Sz8)
typeToType Schema.Type'int16   = Just $ Stage1.Prim (PTyInt Signed Sz16)
typeToType Schema.Type'int32   = Just $ Stage1.Prim (PTyInt Signed Sz32)
typeToType Schema.Type'int64   = Just $ Stage1.Prim (PTyInt Signed Sz64)
typeToType Schema.Type'uint8   = Just $ Stage1.Prim (PTyInt Unsigned Sz8)
typeToType Schema.Type'uint16  = Just $ Stage1.Prim (PTyInt Unsigned Sz16)
typeToType Schema.Type'uint32  = Just $ Stage1.Prim (PTyInt Unsigned Sz32)
typeToType Schema.Type'uint64  = Just $ Stage1.Prim (PTyInt Unsigned Sz64)
typeToType Schema.Type'float32 = Just $ Stage1.Prim PTyFloat32
typeToType Schema.Type'float64 = Just $ Stage1.Prim PTyFloat64
typeToType _                   = Nothing
