-- | Module: Trans.CgrToStage1
-- Description: Translate from schema.capnp's codegenerator request to IR.Stage1.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Trans.CgrToStage1 (cgrToCgr) where

import Data.Word

import Data.Function        ((&))
import Data.ReinterpretCast (doubleToWord, floatToWord)
import Data.Text.Encoding   (encodeUtf8)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V

import Capnp.Classes (toWord)

import qualified Capnp.Gen.Capnp.Schema.Pure as Schema
import qualified Capnp.Untyped.Pure          as U
import qualified IR.Common                   as C
import qualified IR.Name                     as Name
import qualified IR.Stage1                   as Stage1

-- ID for the 'nullable' annotation's node (found by running capnp decode
-- on the cgr). TODO: actually generate code for annotations, so we don't
-- have to hard-code this ourselves.
nullableAnnotationID :: Word64
nullableAnnotationID = 13247283921496574800

type NodeMap v = M.Map Word64 v

nodesToNodes :: NodeMap Schema.Node -> NodeMap Stage1.Node
nodesToNodes inMap = outMap
  where
    outMap = M.map translate inMap

    translate Schema.Node{scopeId, id, nestedNodes, union'} = Stage1.Node
        { nodeCommon = Stage1.NodeCommon
            { nodeId = id
            , nodeNested =
                [ (Name.UnQ name, outMap M.! id)
                | Schema.Node'NestedNode{name, id} <- V.toList nestedNodes
                ]
            , nodeParent =
                if scopeId == 0 then
                    Nothing
                else
                    Just (outMap M.! id)
            }
        , nodeUnion = case union' of
            Schema.Node'enum Schema.Node'enum'{enumerants} ->
                Stage1.NodeEnum $ map enumerantToName $ V.toList enumerants
            Schema.Node'struct Schema.Node'struct'
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
                    , fields = map (fieldToField outMap) (V.toList fields)
                    }
            Schema.Node'interface Schema.Node'interface'{ methods, superclasses } ->
                Stage1.NodeInterface Stage1.Interface
                    { methods = map (methodToMethod outMap) (V.toList methods)
                    , supers = [ outMap M.! id | Schema.Superclass{id} <- V.toList superclasses ]
                    }
            Schema.Node'const Schema.Node'const'{ type_, value } -> Stage1.NodeConstant $
                let mismatch = error "ERROR: Constant's type and value do not agree"
                in case value of
                    Schema.Value'void ->
                        C.VoidValue

                    Schema.Value'bool v ->
                        C.WordValue (C.PrimWord C.PrimBool) (toWord v)

                    Schema.Value'int8 v ->
                        C.WordValue (C.PrimWord $ C.PrimInt $ C.IntType C.Signed C.Sz8) (toWord v)
                    Schema.Value'int16 v ->
                        C.WordValue (C.PrimWord $ C.PrimInt $ C.IntType C.Signed C.Sz16) (toWord v)
                    Schema.Value'int32 v ->
                        C.WordValue (C.PrimWord $ C.PrimInt $ C.IntType C.Signed C.Sz32) (toWord v)
                    Schema.Value'int64 v ->
                        C.WordValue (C.PrimWord $ C.PrimInt $ C.IntType C.Signed C.Sz64) (toWord v)

                    Schema.Value'uint8 v ->
                        C.WordValue (C.PrimWord $ C.PrimInt $ C.IntType C.Unsigned C.Sz8) (toWord v)
                    Schema.Value'uint16 v ->
                        C.WordValue (C.PrimWord $ C.PrimInt $ C.IntType C.Unsigned C.Sz16) (toWord v)
                    Schema.Value'uint32 v ->
                        C.WordValue (C.PrimWord $ C.PrimInt $ C.IntType C.Unsigned C.Sz32) (toWord v)
                    Schema.Value'uint64 v ->
                        C.WordValue (C.PrimWord $ C.PrimInt $ C.IntType C.Unsigned C.Sz64) (toWord v)

                    Schema.Value'float32 v ->
                        C.WordValue (C.PrimWord C.PrimFloat32) (toWord v)
                    Schema.Value'float64 v ->
                        C.WordValue (C.PrimWord C.PrimFloat64) (toWord v)

                    Schema.Value'text v ->
                        C.PtrValue (C.PrimPtr C.PrimText) $ Just $ U.PtrList $ U.List8 $
                            encodeUtf8 v
                            & BS.unpack
                            & (++ [0])
                            & V.fromList
                    Schema.Value'data_ v ->
                        C.PtrValue (C.PrimPtr C.PrimText) $ Just $ U.PtrList $ U.List8 $
                            BS.unpack v
                            & V.fromList

                    Schema.Value'list v ->
                        case type_ of
                            Schema.Type'list Schema.Type'list'{ elementType } ->
                                C.PtrValue
                                    (C.ListOf (typeToType outMap elementType))
                                    v
                            _ ->
                                mismatch

                    Schema.Value'enum v ->
                        case type_ of
                            -- TODO: brand
                            Schema.Type'enum Schema.Type'enum'{ typeId } ->
                                C.WordValue (C.EnumType (outMap M.! typeId)) (toWord v)
                            _ ->
                                mismatch

                    Schema.Value'struct v ->
                        case type_ of
                            -- TODO: brand
                            Schema.Type'struct Schema.Type'struct'{ typeId } -> C.PtrValue
                                (C.PtrComposite (C.StructType (outMap M.! typeId)))
                                v
                            _ ->
                                mismatch

                    Schema.Value'interface ->
                        case type_ of
                            Schema.Type'interface Schema.Type'interface'{ typeId } ->
                                C.PtrValue (C.PtrInterface (outMap M.! typeId)) Nothing
                            _ ->
                                mismatch

                    Schema.Value'anyPointer v ->
                        C.PtrValue (C.PrimPtr (C.PrimAnyPtr C.Ptr)) v

                    Schema.Value'unknown' tag ->
                        error $ "Unknown variant for Value #" ++ show tag
            _ ->
                Stage1.NodeOther
        }


methodToMethod :: NodeMap Stage1.Node -> Schema.Method -> Stage1.Method
methodToMethod nodeMap Schema.Method{ name, paramStructType, resultStructType } =
    Stage1.Method
        { name = Name.UnQ name
        , paramType = nodeMap M.! paramStructType
        , resultType = nodeMap M.! resultStructType
        }

enumerantToName :: Schema.Enumerant -> Name.UnQ
enumerantToName Schema.Enumerant{name} = Name.UnQ name

fieldToField :: NodeMap Stage1.Node -> Schema.Field -> Stage1.Field
fieldToField nodeMap Schema.Field{name, discriminantValue, union', annotations} =
    let nullable = or
            [ id == nullableAnnotationID
            | Schema.Annotation{id, value = Schema.Value'bool True} <- V.toList annotations
            ]
    in
    Stage1.Field
        { name = Name.UnQ name
        , tag =
            if discriminantValue == Schema.field'noDiscriminant then
                Nothing
            else
                Just discriminantValue
        , locType = getFieldLocType nodeMap nullable union'
        }

getFieldLocType :: NodeMap Stage1.Node -> Bool -> Schema.Field' -> C.FieldLocType Stage1.Node
getFieldLocType nodeMap nullable = \case
    Schema.Field'slot Schema.Field'slot'{type_, defaultValue, offset} ->
        case typeToType nodeMap type_ of
            C.VoidType ->
                C.VoidField
            C.PtrType ty ->
                C.PtrField
                    { ptrFieldIndex = fromIntegral offset
                    , ptrFieldType = ty
                    , ptrFieldNullable = nullable
                    }
            C.WordType ty ->
                case valueBits defaultValue of
                    Nothing -> error $
                        "Invlaid schema: a field in a struct's data section " ++
                        "had an illegal (non-data) default value."
                    Just defaultVal ->
                        C.DataField
                            (dataLoc offset ty defaultVal)
                            ty
            C.CompositeType ty ->
                C.PtrField
                    { ptrFieldIndex = fromIntegral offset
                    , ptrFieldType = C.PtrComposite ty
                    , ptrFieldNullable = nullable
                    }
    Schema.Field'group Schema.Field'group'{typeId} ->
        C.HereField $ C.StructType $ nodeMap M.! typeId
    Schema.Field'unknown' _ ->
        -- Don't know how to interpret this; we'll have to leave the argument
        -- opaque.
        C.VoidField

-- | Given the offset field from the capnp schema, a type, and a
-- default value, return a DataLoc describing the location of a field.
dataLoc :: Word32 -> C.WordType Stage1.Node -> Word64 -> C.DataLoc
dataLoc offset ty defaultVal =
    let bitsOffset = fromIntegral offset * C.dataFieldSize ty
    in C.DataLoc
        { dataIdx = bitsOffset `div` 64
        , dataOff = bitsOffset `mod` 64
        , dataDef = defaultVal
        }

-- | Return the raw bit-level representation of a value that is stored
-- in a struct's data section.
--
-- returns Nothing if the value is a non-word type.
valueBits :: Schema.Value -> Maybe Word64
valueBits = \case
    Schema.Value'bool b -> Just $ fromIntegral $ fromEnum b
    Schema.Value'int8 n -> Just $ fromIntegral n
    Schema.Value'int16 n -> Just $ fromIntegral n
    Schema.Value'int32 n -> Just $ fromIntegral n
    Schema.Value'int64 n -> Just $ fromIntegral n
    Schema.Value'uint8 n -> Just $ fromIntegral n
    Schema.Value'uint16 n -> Just $ fromIntegral n
    Schema.Value'uint32 n -> Just $ fromIntegral n
    Schema.Value'uint64 n -> Just n
    Schema.Value'float32 n -> Just $ fromIntegral $ floatToWord n
    Schema.Value'float64 n -> Just $ doubleToWord n
    Schema.Value'enum n -> Just $ fromIntegral n
    _ -> Nothing -- some non-word type.

reqFileToReqFile :: NodeMap Stage1.Node -> Schema.CodeGeneratorRequest'RequestedFile -> Stage1.ReqFile
reqFileToReqFile nodeMap Schema.CodeGeneratorRequest'RequestedFile{id, filename} =
    let Stage1.Node{nodeCommon=Stage1.NodeCommon{nodeNested}} = nodeMap M.! id
    in Stage1.ReqFile
        { fileName = T.unpack filename
        , file = Stage1.File
            { fileNodes = nodeNested
            , fileId = id
            }
        }

cgrToCgr :: Schema.CodeGeneratorRequest -> Stage1.CodeGenReq
cgrToCgr Schema.CodeGeneratorRequest{nodes, requestedFiles} =
    Stage1.CodeGenReq{allFiles, reqFiles}
  where
    nodeMap = nodesToNodes $ M.fromList [(id, node) | node@Schema.Node{id} <- V.toList nodes]
    reqFiles = map (reqFileToReqFile nodeMap) $ V.toList requestedFiles
    allFiles =
        [ let fileNodes =
                [ (Name.UnQ name, nodeMap M.! id)
                | Schema.Node'NestedNode{name, id} <- V.toList nestedNodes

                -- If the file is an import (i.e. not part of requestedFiles), then
                -- the code generator will sometimes omit parts of it that are not
                -- used. We need to check that the nestedNodes are actually included;
                -- if not, we omit them from the otuput as well.
                , M.member id nodeMap
                ]
          in
          Stage1.File{fileId, fileNodes}
        | Schema.Node{union'=Schema.Node'file, id=fileId, nestedNodes} <- V.toList nodes
        ]

typeToType :: NodeMap Stage1.Node -> Schema.Type -> C.Type Stage1.Node
typeToType nodeMap = \case
    Schema.Type'void       -> C.VoidType
    Schema.Type'bool       -> C.WordType $ C.PrimWord C.PrimBool
    Schema.Type'int8       -> C.WordType $ C.PrimWord $ C.PrimInt $ C.IntType C.Signed C.Sz8
    Schema.Type'int16      -> C.WordType $ C.PrimWord $ C.PrimInt $ C.IntType C.Signed C.Sz16
    Schema.Type'int32      -> C.WordType $ C.PrimWord $ C.PrimInt $ C.IntType C.Signed C.Sz32
    Schema.Type'int64      -> C.WordType $ C.PrimWord $ C.PrimInt $ C.IntType C.Signed C.Sz64
    Schema.Type'uint8      -> C.WordType $ C.PrimWord $ C.PrimInt $ C.IntType C.Unsigned C.Sz8
    Schema.Type'uint16     -> C.WordType $ C.PrimWord $ C.PrimInt $ C.IntType C.Unsigned C.Sz16
    Schema.Type'uint32     -> C.WordType $ C.PrimWord $ C.PrimInt $ C.IntType C.Unsigned C.Sz32
    Schema.Type'uint64     -> C.WordType $ C.PrimWord $ C.PrimInt $ C.IntType C.Unsigned C.Sz64
    Schema.Type'float32    -> C.WordType $ C.PrimWord C.PrimFloat32
    Schema.Type'float64    -> C.WordType $ C.PrimWord C.PrimFloat64
    Schema.Type'text       -> C.PtrType $ C.PrimPtr C.PrimText
    Schema.Type'data_      -> C.PtrType $ C.PrimPtr C.PrimData
    Schema.Type'list Schema.Type'list'{elementType} ->
        C.PtrType $ C.ListOf (typeToType nodeMap elementType)
    -- TODO: use 'brand' to generate type parameters.
    Schema.Type'enum Schema.Type'enum'{typeId} ->
        C.WordType $ C.EnumType $ nodeMap M.! typeId
    Schema.Type'struct Schema.Type'struct'{typeId} ->
        C.CompositeType $ C.StructType $ nodeMap M.! typeId
    Schema.Type'interface Schema.Type'interface'{typeId} ->
        C.PtrType $ C.PtrInterface $ nodeMap M.! typeId
    Schema.Type'anyPointer anyPtr -> C.PtrType $ C.PrimPtr $ C.PrimAnyPtr $
        case anyPtr of
            Schema.Type'anyPointer'unconstrained Schema.Type'anyPointer'unconstrained'anyKind ->
                C.Ptr
            Schema.Type'anyPointer'unconstrained Schema.Type'anyPointer'unconstrained'struct ->
                C.Struct
            Schema.Type'anyPointer'unconstrained Schema.Type'anyPointer'unconstrained'list ->
                C.List
            Schema.Type'anyPointer'unconstrained Schema.Type'anyPointer'unconstrained'capability ->
                C.Cap
            _ ->
                -- Something we don't know about; assume it could be anything.
                C.Ptr
    _ -> C.VoidType -- TODO: constrained anyPointers
