-- | Module: Trans.CgrToStage1
-- Description: Translate from schema.capnp's codegenerator request to IR.Stage1.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Trans.CgrToStage1 (cgrToFiles) where

import Data.Word

import Data.ReinterpretCast (doubleToWord, floatToWord)

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V

import qualified Capnp.Gen.Capnp.Schema.Pure as Schema
import qualified IR.Common                   as C
import qualified IR.Name                     as Name
import qualified IR.Stage1                   as Stage1

type NodeMap v = M.Map Word64 v

nodesToNodes :: NodeMap Schema.Node -> NodeMap Stage1.Node
nodesToNodes inMap = outMap
  where
    outMap = M.map translate inMap

    translate Schema.Node{scopeId, id, nestedNodes, union'} = Stage1.Node
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
                    , fields = map (fieldToField outMap) (V.toList fields)
                    }
            Schema.Node'interface{} ->
                Stage1.NodeInterface
            _ ->
                Stage1.NodeOther
        }

enumerantToName :: Schema.Enumerant -> Name.UnQ
enumerantToName Schema.Enumerant{name} = Name.UnQ name

fieldToField :: NodeMap Stage1.Node -> Schema.Field -> Stage1.Field
fieldToField nodeMap Schema.Field{name, discriminantValue, union'} =
    Stage1.Field
        { name = Name.UnQ name
        , tag =
            if discriminantValue == Schema.field'noDiscriminant then
                Nothing
            else
                Just discriminantValue
        , locType = getFieldLocType nodeMap union'
        }

getFieldLocType :: NodeMap  Stage1.Node -> Schema.Field' -> C.FieldLocType Stage1.Node
getFieldLocType nodeMap = \case
    Schema.Field'slot{type_, defaultValue, hadExplicitDefault, offset} ->
        case typeToType nodeMap type_ of
            C.VoidType ->
                C.VoidField
            C.PtrType ty
                | hadExplicitDefault -> error $
                    "Error: capnpc-haskell does not support explicit default " ++
                    "field values for pointer types. See:\n" ++
                    "\n" ++
                    "    https://github.com/zenhack/haskell-capnp/issues/28"
                | otherwise ->
                    C.PtrField (fromIntegral offset) ty
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
                C.PtrField (fromIntegral offset) (C.PtrComposite ty)
    Schema.Field'group{typeId} ->
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

reqFileToFile :: NodeMap Stage1.Node -> Schema.CodeGeneratorRequest'RequestedFile -> Stage1.File
reqFileToFile nodeMap Schema.CodeGeneratorRequest'RequestedFile{id, filename, imports} =
    let Stage1.Node{nodeNested} = nodeMap M.! id
    in Stage1.File
        { fileNodes = nodeNested
        , fileName = T.unpack filename
        , fileId = id
        , fileImports =
            [ id
            | Schema.CodeGeneratorRequest'RequestedFile'Import{id} <- V.toList imports
            ]
        }

cgrToFiles :: Schema.CodeGeneratorRequest -> [Stage1.File]
cgrToFiles Schema.CodeGeneratorRequest{nodes, requestedFiles} =
    let nodeMap = nodesToNodes $ M.fromList [(id, node) | node@Schema.Node{id} <- V.toList nodes]
    in map (reqFileToFile nodeMap) $ V.toList requestedFiles

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
    Schema.Type'list elt   -> C.PtrType $ C.ListOf (typeToType nodeMap elt)
    -- TODO: use 'brand' to generate type parameters.
    Schema.Type'enum{typeId} -> C.WordType $ C.EnumType $ nodeMap M.! typeId
    Schema.Type'struct{typeId} -> C.CompositeType $ C.StructType $ nodeMap M.! typeId
    Schema.Type'interface{typeId} -> C.PtrType $ C.PtrInterface $ nodeMap M.! typeId
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
