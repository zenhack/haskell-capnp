{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module FrontEnd
    ( cgrToIR
    ) where

import Data.Word

import Data.Char            (toUpper)
import Data.Function        ((&))
import Data.List            (partition)
import Data.ReinterpretCast (doubleToWord, floatToWord)
import Data.Text.Encoding   (encodeUtf8)
import Util                 (Id, splitOn)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V

import Capnp.Gen.Capnp.Schema.Pure

import Backends.Common (dataFieldSize)

import qualified Capnp.Untyped.Pure as Untyped
import qualified IR

type NodeMap = M.Map Id NodeMetaData

-- | Some useful metadata about a node.
data NodeMetaData = NodeMetaData
    { moduleId  :: Id
    -- ^ The id of the module that this node belongs to.
    , namespace :: [T.Text]
    -- ^ The namespace within a file that the node is defined.
    , node      :: Node
    -- ^ The node itself.
    }
    deriving(Show, Read, Eq)

-- | @'identifierFromMetaData' meta@ return a haskell identifier
-- for a node based on the metadata @meta@, and @thisModule@, the id for
-- the module in which the name will be used.
identifierFromMetaData :: NodeMetaData -> IR.Name
identifierFromMetaData NodeMetaData{moduleId, namespace=(unqualified:localNS)} =
    IR.Name
        { nameModule = IR.ByCapnpId moduleId
        , nameLocalNS = IR.Namespace $ reverse localNS
        , nameUnqualified = unqualified
        }
identifierFromMetaData meta =
    -- TODO: rule out this possibility statically; shouldn't be too hard.
    error $ "Node metadata had an empty namespace field: " ++ show meta

-- Helper for makeNodeMap; recursively collect metadata for a node and
-- all of its descendants in the tree.
collectMetaData :: M.Map Id Node -> NodeMetaData -> [(Id, NodeMetaData)]
collectMetaData nodeMap meta@NodeMetaData{node=node@Node{..}, ..} = concat
    [ [(id, meta)]
    , concatMap collectNested $ V.toList nestedNodes
    , case union' of
            Node'struct{..} ->
                -- Child field nodes can be in two places: most are in nestedNodes,
                -- but group fields are not, and can only be found in the fields of
                -- a struct union.
                concatMap collectField $ V.toList fields
            Node'interface{..} ->
                -- If the interface has methods with anonymous parameter or result
                -- types, we need to collect them, since they aren't otherwise
                -- reachable.
                concatMap collectMethod $ V.toList methods
            _ ->
                []
    ]
  where
    -- Collect metadata for nodes under a Field.
    collectField Field{..} = case union' of
        Field'group{..} -> collectMetaData nodeMap
            meta
                { node = nodeMap M.! typeId
                -- in this case, name comes from the field, so for a field bar in a
                -- struct Foo, we'll end up with a type for the group named Foo'bar.
                , namespace = makeLegalName name : namespace
                }
        _ ->
            []
    -- Collect metadata for nodes under a Method.
    collectMethod Method{..} =
        collectMethodStruct name paramStructType "params" ++
        collectMethodStruct name resultStructType "results"

    -- Collect metadata for a struct that is a method parameter or result.
    collectMethodStruct name nodeId paramOrResult = case M.lookup nodeId nodeMap of
        Just node@Node{scopeId} | scopeId == 0 ->
            -- This is an anonymous struct; generate a node with a name for it.
            collectMetaData nodeMap NodeMetaData
                { namespace = paramOrResult : name : namespace
                , node
                , moduleId
                }
        _ ->
            -- This isn't an anonymous struct; it will get picked up somewhere
            -- else.
            []

    -- Collect metadata for nodes under a NestedNode.
    collectNested nn@Node'NestedNode{..} = case M.lookup id nodeMap of
        Just node -> collectMetaData nodeMap
            meta
                { node = node
                , namespace = makeLegalName name : namespace
                }
        Nothing ->
            -- Imperically, this can happen if e.g. the node isn't in a subtree
            -- of a RequestedFile, and not actually used anywhere else. This
            -- crops up with c++.capnp:name when processing schema.capnp, for
            -- example. In this case, just leave it out of the map:
            []

-- | Convert the argument into a valid haskell identifier. This doesn't handle
-- every possible violation, just ones that might occur in legal schema.
makeLegalName :: T.Text -> T.Text
makeLegalName txt
    | txt `elem` keywords = txt <> "_"
    | otherwise = txt
  where
    keywords =
        [ "as", "case", "of", "class", "data", "family", "instance", "default"
        , "deriving", "do", "forall", "foreign", "hiding", "if", "then", "else"
        , "import", "infix", "infixl", "infixr", "let", "in", "mdo", "module"
        , "newtype", "proc", "qualified", "rec", "type", "where"
        ]

-- | Build a NodeMap for all of the nodes in the CodeGeneratorRequest.
makeNodeMap :: CodeGeneratorRequest -> NodeMap
makeNodeMap CodeGeneratorRequest{..} =
    V.map (\node@Node{..} -> collectMetaData baseMap NodeMetaData
        { moduleId = id
        , namespace = []
        , node = node
        })
        rootNodes
    & V.toList
    & concat
    & M.fromList
  where
    rootNodes = nodes & V.filter (\case
        Node{union'=Node'file, scopeId=0} ->
            True
        _ ->
            False)
    baseMap =
        V.toList nodes
        & map (\node@Node{..} -> (id, node))
        & M.fromList

generateModule :: NodeMap -> CodeGeneratorRequest'RequestedFile -> IR.Module
generateModule nodeMap CodeGeneratorRequest'RequestedFile{..} =
    IR.Module
        { modId = id
        , modName = IR.Namespace
            $ map (T.pack . mangleFileName)
            $ filter (/= "")
            $ splitOn '/' (T.unpack filename)
        , modFile = filename
        , modImports = map generateImport $ V.toList imports
        , modDecls = M.fromList $ concatMap (generateDecls id nodeMap)
            $ filter (\NodeMetaData{..} -> moduleId == id)
            $ map snd
            $ M.toList nodeMap
        }
  where
    -- Transform the file name into a valid haskell module name.
    -- TODO: this is a best-effort transformation; it gives good
    -- results on the schema I've found in the wild, but may fail
    -- to generate valid/non-overlapping module names in all cases.
    mangleFileName "c++.capnp" = "Cxx"
    mangleFileName ""          = error "Unexpected empty file name"
    mangleFileName (c:cs)      = go (toUpper c : cs) where
        go ('-':c:cs) = toUpper c : go cs
        go ".capnp"   = ""
        go []         = ""
        go (c:cs)     = c : go cs

-- | Check whether the node's parent scope actually needs a type definition for
-- the node. This is true unless it is a group belonging to a union, which itself
-- has no anonymous union. In that case we just inline the fields, like:
--
-- @@@
-- data MyUnion =
--     MyUnion'variant1
--        { foo :: Bar
--        , baz :: Quux
--        }
-- @@@
--
-- ...and thus don't need an intervening type definition.
neededByParent :: NodeMap -> Node -> Bool
neededByParent nodeMap Node{id,scopeId,union'=Node'struct{isGroup,discriminantCount}} | isGroup =
    case nodeMap M.! scopeId of
        NodeMetaData{node=Node{union'=Node'struct{fields}}} ->
            let me = V.filter
                        (\case
                            Field{union'=Field'group{typeId}} -> typeId == id
                            _ -> False)
                        fields
            in if V.length me /= 1
                then error "Invalid schema; group matched multiple fields in its scopeId!"
                else not (isUnionField (me V.! 0) && discriminantCount == 0)
        _ -> error "Invalid schema; group's scopeId references something that is not a struct!"
neededByParent _ _ = True

generateMethodStructs :: Id -> NodeMap -> Method -> [(IR.Name, IR.Decl)]
generateMethodStructs thisMod nodeMap Method{..} =
    go paramStructType ++ go resultStructType
  where
    go structId = case M.lookup structId nodeMap of
        Just meta@NodeMetaData{node=Node{id,scopeId}} | scopeId == 0 ->
            -- anonymous struct; this isn't reachable from anywhere else.
            generateDecls thisMod (M.insert id meta nodeMap) meta
        _ ->
            -- not an anonymous struct; will be taken care of elsewhere.
            []

generateMethod :: Id -> NodeMap -> IR.Name -> Word16 -> Method -> IR.Method
generateMethod thisMod nodeMap parentName ordinal Method{..} =
    IR.Method
        { methodName = IR.subName parentName name
        , ordinal = ordinal
        , paramType = typeFromId paramStructType
        , resultType = typeFromId resultStructType
        }
  where
    typeFromId typeId =
        IR.StructType (identifierFromMetaData (nodeMap M.! typeId)) []

generateDecls :: Id -> NodeMap -> NodeMetaData -> [(IR.Name, IR.Decl)]
generateDecls thisModule nodeMap meta@NodeMetaData{..} =
    let Node{..} = node
        name = identifierFromMetaData meta
    in case union' of
        Node'struct{..} | neededByParent nodeMap node ->
            let allFields = V.toList fields
                (unionFields, commonFields) = partition isUnionField allFields
                typeName = name
                unionName = IR.subName name ""

                structDef = IR.StructDef
                    { fields = formatStructBody thisModule nodeMap typeName allFields
                    , info = if isGroup
                        then IR.IsGroup
                        else IR.IsStandalone
                            { dataSz = dataWordCount
                            , ptrSz = pointerCount
                            }
                    }
                bodyFields = IR.DeclDef (IR.DefStruct structDef)

                bodyUnion = IR.DeclDef IR.DefUnion
                    { dataVariants =
                        map (generateVariant thisModule nodeMap name) unionFields
                    , dataTagLoc = dataLoc
                        discriminantOffset
                        (IR.PrimWord IR.PrimInt{isSigned = False, size = 16})
                        -- The default value for a union tag is always zero:
                        (Value'uint16 0)
                    , parentStructName = name
                    , parentStruct = structDef
                    }

            in case (unionFields, commonFields) of
                ([], _) ->
                    -- There's no anonymous union; just declare the fields. Note that
                    -- this covers the case where there are no fields at all.
                    [ ( typeName, bodyFields ) ]
                (_:_, []) ->
                    -- The struct is just one big anonymous union; expand the variants
                    -- in-line, rather than making a wrapper.
                    [ ( typeName, bodyUnion ) ]
                (_:_, _:_) ->
                    -- There are both common fields and an anonymous union. Generate
                    -- an auxiliary type for the union.
                    [ ( typeName, bodyFields )
                    , ( unionName, bodyUnion )
                    ]
        Node'interface{..} ->
            ( name
            , IR.DeclDef $ IR.DefInterface $ IR.InterfaceDef
                { interfaceId = id
                , methods = zipWith
                    (generateMethod thisModule nodeMap name)
                    [0..] (V.toList
                    methods)
                }
            )
            : concatMap (generateMethodStructs thisModule nodeMap) methods
        Node'enum{..} ->
            [ ( name
              , IR.DeclDef $ IR.DefEnum $ map
                    (\Enumerant{name=variantName} ->
                        IR.subName name variantName
                    )
                    (V.toList enumerants)
              )
            ]
        Node'const{type_=Type'void,value=Value'void} ->
            [(name, IR.DeclConst IR.VoidConst)]
        Node'const{type_=Type'bool,value=Value'bool v} ->
            [(name, primWordConst IR.PrimBool (fromEnum v))]
        Node'const{type_=Type'int8,value=Value'int8 v} ->
            [(name, primWordConst IR.PrimInt { isSigned = True, size = 8 } v)]
        Node'const{type_=Type'int16,value=Value'int16 v} ->
            [(name, primWordConst IR.PrimInt { isSigned = True, size = 16 } v)]
        Node'const{type_=Type'int32,value=Value'int32 v} ->
            [(name, primWordConst IR.PrimInt { isSigned = True, size = 32 } v)]
        Node'const{type_=Type'int64,value=Value'int64 v} ->
            [(name, primWordConst IR.PrimInt { isSigned = True, size = 64 } v)]
        Node'const{type_=Type'uint8,value=Value'uint8 v} ->
            [(name, primWordConst IR.PrimInt { isSigned = False, size = 8 } v)]
        Node'const{type_=Type'uint16,value=Value'uint16 v} ->
            [(name, primWordConst IR.PrimInt { isSigned = False, size = 16 } v)]
        Node'const{type_=Type'uint32,value=Value'uint32 v} ->
            [(name, primWordConst IR.PrimInt { isSigned = False, size = 32 } v)]
        Node'const{type_=Type'uint64,value=Value'uint64 v} ->
            [(name, primWordConst IR.PrimInt { isSigned = False, size = 64 } v)]
        Node'const{type_=Type'float32,value=Value'float32 v} ->
            [(name, primWordConst IR.PrimFloat32 (floatToWord v))]
        Node'const{type_=Type'float64,value=Value'float64 v} ->
            [(name, primWordConst IR.PrimFloat64 (doubleToWord v))]
        Node'const{type_=Type'text,value=Value'text v} ->
            [ ( name
              , IR.DeclConst IR.PtrConst
                { ptrType = IR.PrimPtr IR.PrimText
                , ptrValue = Just $ Untyped.PtrList $ Untyped.List8 $
                    encodeUtf8 v
                    & BS.unpack
                    & (++ [0])
                    & V.fromList
                }
              )
            ]
        Node'const{type_=Type'data_,value=Value'data_ v} ->
            [ ( name
              , IR.DeclConst IR.PtrConst
                { ptrType = IR.PrimPtr IR.PrimData
                , ptrValue = Just $ Untyped.PtrList $ Untyped.List8 $
                    BS.unpack v
                    & V.fromList
                }
              )
            ]
        Node'const{type_=Type'list{elementType},value=Value'list v} ->
            [ ( name
              , IR.DeclConst IR.PtrConst
                { ptrType = IR.ListOf (formatType thisModule nodeMap elementType)
                , ptrValue = v
                }
              )
            ]
        Node'const{type_=Type'enum{typeId},value=Value'enum v} ->
            -- TODO: do something with brand.
            [ ( name
              , IR.DeclConst IR.WordConst
                { wordValue = fromIntegral v
                , wordType = IR.EnumType $ identifierFromMetaData (nodeMap M.! typeId)
                }
              )
            ]
        -- TODO: group constants?
        Node'const{type_=Type'struct{typeId},value=Value'struct v} ->
            [ ( name
              , IR.DeclConst IR.PtrConst
                { ptrType = IR.PtrComposite $ IR.StructType
                    (identifierFromMetaData (nodeMap M.! typeId))
                    []
                , ptrValue = v
                }
              )
            ]
        -- TODO: interface constants
        Node'const
            { type_=Type'anyPointer{union'=Type'anyPointer'unconstrained{union'}}
            , value=Value'anyPointer v
            } ->
            [ ( name
              , IR.DeclConst IR.PtrConst
                { ptrValue = v
                , ptrType = IR.PrimPtr $ IR.PrimAnyPtr $ case union' of
                    Type'anyPointer'unconstrained'anyKind    -> IR.Ptr
                    Type'anyPointer'unconstrained'struct     -> IR.Struct
                    Type'anyPointer'unconstrained'list       -> IR.List

                    -- TODO: we'll want to restrict this once we actually
                    -- support interfaces:
                    Type'anyPointer'unconstrained'capability -> IR.Ptr

                    Type'anyPointer'unconstrained'unknown' _ -> IR.Ptr
                }
              )
            ]
        -- TODO: "constrained" anyPointer constants. This has to wait until
        -- we support type parameters.
        _ -> [] -- TODO

primWordConst :: Integral a => IR.PrimWord -> a -> IR.Decl
primWordConst ty val = IR.DeclConst IR.WordConst
    { wordValue = fromIntegral val
    , wordType = IR.PrimWord ty
    }

-- | Given the offset field from the capnp schema, a type, and a
-- default value, return a DataLoc describing the location of a field.
dataLoc :: Word32 -> IR.WordType -> Value -> IR.DataLoc
dataLoc offset ty defaultVal =
    let bitsOffset = fromIntegral offset * dataFieldSize ty
    in IR.DataLoc
        { dataIdx = bitsOffset `div` 64
        , dataOff = bitsOffset `mod` 64
        , dataDef = valueBits defaultVal
        }

-- | Return whether the field is part of a union within its struct.
isUnionField :: Field -> Bool
isUnionField Field{..} = discriminantValue /= field'noDiscriminant

formatStructBody :: Id -> NodeMap -> IR.Name -> [Field] -> [IR.Field]
formatStructBody thisModule nodeMap parentName fields =
    let (unionFields, commonFields) = partition isUnionField fields in
    map (generateField thisModule nodeMap) commonFields
    <> case unionFields of
        [] -> [] -- no union
        _  ->
            [ IR.Field
                { fieldName = "union'"
                , fieldLocType =
                    -- This could use a bit of refactoring. Right now we call
                    -- formatSturctBody in two cases:
                    let fieldTypeName = case commonFields of
                            -- 1. A top-level struct, which also has non anonymous-union
                            --    fields. In this case, the anonymous union for a struct
                            --    named Foo will have a type named Foo'; we add an empty
                            --    segment to the union' field's type name.

                            _:_ -> IR.subName parentName ""

                            -- 2. An argument of a named union variant, which itself is
                            --    a union. In this case, parentName is the name of the outer
                            --    union's data constructor, and the *inner* union's type
                            --    constructor; we use parentName unchanged.

                            []  -> parentName

                    in IR.HereField $ IR.StructType fieldTypeName []
                }
            ]

-- | Generate a variant of a type corresponding to an anonymous union in a
-- struct.
generateVariant :: Id -> NodeMap -> IR.Name -> Field -> IR.Variant
generateVariant thisModule nodeMap parentName Field{..} = case union' of
    Field'slot{..} -> IR.Variant
        { variantName
        , variantParams = IR.Unnamed
            (formatType thisModule nodeMap type_)
            (getFieldLoc thisModule nodeMap union')
        , variantTag = discriminantValue
        }
    Field'group{..} ->
        let NodeMetaData{node=node@Node{..},..} = nodeMap M.! typeId
        in case union' of
            Node'struct{..} -> IR.Variant
                { variantName
                , variantParams =
                    IR.Record $ formatStructBody thisModule nodeMap variantName $ V.toList fields
                , variantTag = discriminantValue
                }
            _               ->
                error "A group field referenced a non-struct node."
    Field'unknown' _ ->
        -- Some sort of field we don't know about (newer version of capnp probably).
        -- Generate the variant, but we don't know what the argument type should be,
        -- so leave it out.
        IR.Variant
            { variantName
            , variantParams = IR.Unnamed IR.VoidType IR.VoidField
            , variantTag = discriminantValue
            }
  where
    variantName = IR.subName parentName (makeLegalName name)


generateField :: Id -> NodeMap -> Field -> IR.Field
generateField thisModule nodeMap Field{..} =
    IR.Field
        { fieldName = makeLegalName name
        , fieldLocType = getFieldLoc thisModule nodeMap union'
        }

getFieldLoc :: Id -> NodeMap -> Field' -> IR.FieldLocType
getFieldLoc thisModule nodeMap = \case
    Field'slot{..} ->
        case formatType thisModule nodeMap type_ of
            IR.VoidType ->
                IR.VoidField
            IR.PtrType ty
                | hadExplicitDefault -> error $
                    "Error: capnpc-haskell does not support explicit default " ++
                    "field values for pointer types. See:\n" ++
                    "\n" ++
                    "    https://github.com/zenhack/haskell-capnp/issues/28"
                | otherwise ->
                    IR.PtrField (fromIntegral offset) ty
            IR.WordType ty ->
                IR.DataField
                    (dataLoc offset ty defaultValue)
                    ty
            IR.CompositeType ty ->
                IR.PtrField (fromIntegral offset) (IR.PtrComposite ty)
    Field'group{..} ->
        IR.HereField $ IR.StructType (identifierFromMetaData (nodeMap M.! typeId)) []
    Field'unknown' _ ->
        -- Don't know how to interpret this; we'll have to leave the argument
        -- opaque.
        IR.VoidField

-- | Return the raw bit-level representation of a value that is stored
-- in a struct's data section.
--
-- Returns 0 for any non-data values. TODO: would be nice to not have
-- an arbitrary default here.
valueBits :: Value -> Word64
valueBits = \case
    Value'bool b -> fromIntegral $ fromEnum b
    Value'int8 n -> fromIntegral n
    Value'int16 n -> fromIntegral n
    Value'int32 n -> fromIntegral n
    Value'int64 n -> fromIntegral n
    Value'uint8 n -> fromIntegral n
    Value'uint16 n -> fromIntegral n
    Value'uint32 n -> fromIntegral n
    Value'uint64 n -> n
    Value'float32 n -> fromIntegral $ floatToWord n
    Value'float64 n -> doubleToWord n
    Value'enum n -> fromIntegral n
    _ -> 0 -- some non-word type.

formatType :: Id -> NodeMap -> Type -> IR.Type
formatType thisModule nodeMap ty = case ty of
    Type'void       -> IR.VoidType
    Type'bool       -> IR.WordType $ IR.PrimWord IR.PrimBool
    Type'int8       -> IR.WordType $ IR.PrimWord IR.PrimInt {isSigned = True, size = 8}
    Type'int16      -> IR.WordType $ IR.PrimWord IR.PrimInt {isSigned = True, size = 16}
    Type'int32      -> IR.WordType $ IR.PrimWord IR.PrimInt {isSigned = True, size = 32}
    Type'int64      -> IR.WordType $ IR.PrimWord IR.PrimInt {isSigned = True, size = 64}
    Type'uint8      -> IR.WordType $ IR.PrimWord IR.PrimInt {isSigned = False, size = 8}
    Type'uint16     -> IR.WordType $ IR.PrimWord IR.PrimInt {isSigned = False, size = 16}
    Type'uint32     -> IR.WordType $ IR.PrimWord IR.PrimInt {isSigned = False, size = 32}
    Type'uint64     -> IR.WordType $ IR.PrimWord IR.PrimInt {isSigned = False, size = 64}
    Type'float32    -> IR.WordType $ IR.PrimWord IR.PrimFloat32
    Type'float64    -> IR.WordType $ IR.PrimWord IR.PrimFloat64
    Type'text       -> IR.PtrType $ IR.PrimPtr IR.PrimText
    Type'data_      -> IR.PtrType $ IR.PrimPtr IR.PrimData
    Type'list elt   -> IR.PtrType $ IR.ListOf (formatType thisModule nodeMap elt)
    -- TODO: use 'brand' to generate type parameters.
    Type'enum{..}   -> IR.WordType $ IR.EnumType (typeName typeId)
    Type'struct{..} -> IR.CompositeType $ IR.StructType (typeName typeId) []
    Type'interface{..} -> IR.PtrType $ IR.PtrInterface (typeName typeId)
    Type'anyPointer anyPtr -> IR.PtrType $ IR.PrimPtr $ IR.PrimAnyPtr $
        case anyPtr of
            Type'anyPointer'unconstrained Type'anyPointer'unconstrained'anyKind ->
                IR.Ptr
            Type'anyPointer'unconstrained Type'anyPointer'unconstrained'struct ->
                IR.Struct
            Type'anyPointer'unconstrained Type'anyPointer'unconstrained'list ->
                IR.List
            Type'anyPointer'unconstrained Type'anyPointer'unconstrained'capability ->
                IR.Cap
            _ ->
                -- Something we don't know about; assume it could be anything.
                IR.Ptr
    _ -> IR.VoidType -- TODO: constrained anyPointers
  where
    typeName typeId =
        identifierFromMetaData (nodeMap M.! typeId)

generateImport :: CodeGeneratorRequest'RequestedFile'Import -> IR.Import
generateImport CodeGeneratorRequest'RequestedFile'Import{..} =
    IR.Import (IR.ByCapnpId id)

cgrToIR :: CodeGeneratorRequest -> [IR.Module]
cgrToIR cgr@CodeGeneratorRequest{requestedFiles} =
    map (generateModule $ makeNodeMap cgr) $ V.toList requestedFiles
