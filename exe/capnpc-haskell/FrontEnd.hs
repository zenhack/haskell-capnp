{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FrontEnd
    ( cgrToIR
    ) where

import Capnp.Capnp.Schema.Pure
import Data.Word

import Data.Char            (toUpper)
import Data.Function        ((&))
import Data.Monoid          ((<>))
import Data.ReinterpretCast (doubleToWord, floatToWord)
import Util                 (Id, splitOn)

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V

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

-- | @'identifierFromMetaData' thisModule meta@ return a haskell identifier
-- for a node based on the metadata @meta@, and @thisModule@, the id for
-- the module in which the name will be used.
identifierFromMetaData :: Id -> NodeMetaData -> IR.Name
identifierFromMetaData _ NodeMetaData{moduleId, namespace=(unqualified:localNS)} =
    IR.Name
        { nameModule = IR.ByCapnpId moduleId
        , nameLocalNS = IR.Namespace $ reverse localNS
        , nameUnqualified = unqualified
        }
identifierFromMetaData _ meta =
    -- TODO: rule out this possibility statically; shouldn't be too hard.
    error $ "Node metadata had an empty namespace field: " ++ show meta

-- Helper for makeNodeMap; recursively collect metadata for a node and
-- all of its descendants in the tree.
collectMetaData :: M.Map Id Node -> NodeMetaData -> [(Id, NodeMetaData)]
collectMetaData nodeMap meta@NodeMetaData{node=node@Node'{..}, ..} = concat
    [ [(id, meta)]
    , concatMap collectNested $ V.toList nestedNodes
    -- Child nodes can be in two places: most are in nestedNodes, but
    -- group fields are not, and can only be found in the fields of
    -- a struct union.
    , case union' of
            Node'struct{..} ->
                concatMap collectField $ V.toList fields
            _ ->
                []
    ]
  where
    -- Collect metadata for nodes under a Field.
    collectField Field'{..} = case union' of
        Field'group{..} -> collectMetaData nodeMap
            meta
                { node = nodeMap M.! typeId
                -- in this case, name comes from the field, so for a field bar in a
                -- struct Foo, we'll end up with a type for the group named Foo'bar.
                , namespace = makeLegalName name : namespace
                }
        _ ->
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
    V.map (\node@Node'{..} -> collectMetaData baseMap NodeMetaData
        { moduleId = id
        , namespace = []
        , node = node
        })
        rootNodes
    & V.toList
    & concat
    & M.fromList
  where
    rootNodes = V.filter (\Node'{..} -> scopeId == 0) nodes
    baseMap =
        V.toList nodes
        & map (\node@Node'{..} -> (id, node))
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
        , modDecls = concatMap (generateDecls id nodeMap)
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
-- data MyUnion
--     MyUnion'variant1
--        { foo :: Bar
--        , baz :: Quux
--        }
-- @@@
--
-- ...and thus don't need an intervening type definition.
neededByParent :: NodeMap -> Node -> Bool
neededByParent nodeMap Node'{id,scopeId,union'=Node'struct{isGroup,discriminantCount}} | isGroup =
    case nodeMap M.! scopeId of
        NodeMetaData{node=Node'{union'=Node'struct{fields}}} ->
            let me = V.filter
                        (\case
                            Field'{union'=Field'group{typeId}} -> typeId == id
                            _ -> False)
                        fields
            in if V.length me /= 1
                then error "Invalid schema; group matched multiple fields in its scopeId!"
                else not (isUnionField (me V.! 0) && discriminantCount == 0)
        _ -> error "Invalid schema; group's scopeId references something that is not a struct!"
neededByParent _ _ = True

generateDecls :: Id -> NodeMap -> NodeMetaData -> [(IR.Name, IR.Decl)]
generateDecls thisModule nodeMap meta@NodeMetaData{..} =
    let Node'{..} = node
        name = identifierFromMetaData moduleId meta
    in case union' of
        Node'struct{..} | neededByParent nodeMap node ->
            let allFields = V.toList fields
                unionFields = filter isUnionField allFields
                commonFields = filter (not . isUnionField) allFields
                typeName = name
                -- variants to generate that go inside the union:
                unionVariants =
                    map (generateVariant thisModule nodeMap name) unionFields
                    ++
                    -- Every union gets an extra "unknown" varaint, which is used
                    -- whenever what's on the wire has a discriminant that's not
                    -- in our schema.
                    [ IR.Variant
                        { variantName = IR.subName name "unknown'"
                        , variantParams = IR.Unnamed
                            (IR.PrimType IR.PrimInt{isSigned=False, size=16})
                            IR.VoidField -- We won't end up actually fetching this from anywhere.
                        , variantTag = Nothing
                        }
                    ]
            in case (unionFields, commonFields) of
                ([], []) ->
                    -- I(zenhack) don't fully understand this case. It seems like
                    -- it should apply to struct Foo {}, but it also imperically
                    -- shows up for type aliases in the schema. In this case, the
                    -- schema should still build without them since our generated
                    -- code just uses the original type. Furthermore, right now
                    -- our ast output doesn't handle types with no variants, so
                    -- we just don't output any code.
                    []
                ([], _:_) ->
                    -- There's no anonymous union; just declare the fields.
                    [ ( typeName
                      , IR.DeclDef IR.DataDef
                            { dataVariants =
                                [ IR.Variant
                                    { variantName = typeName
                                    , variantParams = formatStructBody thisModule nodeMap typeName allFields
                                    , variantTag = Nothing
                                    }
                                ]
                            , dataTagLoc = Nothing
                            , dataCerialType = IR.CTyStruct
                            }
                      )
                    ]
                (_:_, []) ->
                    -- The struct is just one big anonymous union; expand the variants
                    -- in-line, rather than making a wrapper.
                    [ ( typeName
                      , IR.DeclDef IR.DataDef
                            { dataVariants = unionVariants
                            , dataTagLoc = Just $ dataLoc
                                discriminantOffset
                                Type'uint16
                                -- The default value for a union tag is always zero:
                                (Value'uint16 0)
                            , dataCerialType = IR.CTyStruct
                            }
                      )
                    ]
                (_:_, _:_) ->
                    -- There are both common fields and an anonymous union. Generate
                    -- an auxiliary type for the union.
                    let unionName = IR.subName name ""
                    in  [ ( typeName
                          , IR.DeclDef IR.DataDef
                                { dataVariants =
                                    [ IR.Variant
                                        { variantName = unionName
                                        , variantParams = formatStructBody thisModule nodeMap unionName allFields
                                        , variantTag = Nothing
                                        }
                                    ]
                                , dataTagLoc = Nothing
                                , dataCerialType = IR.CTyStruct
                                }
                          )
                        , ( unionName
                          , IR.DeclDef IR.DataDef
                                { dataVariants = unionVariants
                                , dataTagLoc = Just $ dataLoc
                                    discriminantOffset
                                    Type'uint16
                                    -- Default value for a union tag is always zero:
                                    (Value'uint16 0)
                                , dataCerialType = IR.CTyStruct
                                }
                          )
                        ]
        Node'enum{..} ->
            [ ( name
              , IR.DeclDef IR.DataDef
                    { dataVariants =
                        map (generateEnum thisModule nodeMap name) (V.toList enumerants)
                        <> [ IR.Variant
                                { variantName = IR.subName name "unknown'"
                                , variantParams = IR.Unnamed
                                    (IR.PrimType IR.PrimInt {isSigned=False, size=16})
                                    IR.VoidField
                                , variantTag = Nothing
                                }
                           ]
                    , dataTagLoc = Nothing
                    , dataCerialType = IR.CTyEnum
                    }
              )
            ]
        Node'const{type_=Type'void,value=Value'void} ->
            [(name, primTypeConst IR.PrimVoid 0)]
        Node'const{type_=Type'bool,value=Value'bool v} ->
            [(name, primTypeConst IR.PrimBool (fromEnum v))]
        Node'const{type_=Type'int8,value=Value'int8 v} ->
            [(name, primTypeConst IR.PrimInt { isSigned = True, size = 8 } v)]
        Node'const{type_=Type'int16,value=Value'int16 v} ->
            [(name, primTypeConst IR.PrimInt { isSigned = True, size = 16 } v)]
        Node'const{type_=Type'int32,value=Value'int32 v} ->
            [(name, primTypeConst IR.PrimInt { isSigned = True, size = 32 } v)]
        Node'const{type_=Type'int64,value=Value'int64 v} ->
            [(name, primTypeConst IR.PrimInt { isSigned = True, size = 64 } v)]
        Node'const{type_=Type'uint8,value=Value'uint8 v} ->
            [(name, primTypeConst IR.PrimInt { isSigned = False, size = 8 } v)]
        Node'const{type_=Type'uint16,value=Value'uint16 v} ->
            [(name, primTypeConst IR.PrimInt { isSigned = False, size = 16 } v)]
        Node'const{type_=Type'uint32,value=Value'uint32 v} ->
            [(name, primTypeConst IR.PrimInt { isSigned = False, size = 32 } v)]
        Node'const{type_=Type'uint64,value=Value'uint64 v} ->
            [(name, primTypeConst IR.PrimInt { isSigned = False, size = 64 } v)]
        Node'const{type_=Type'float32,value=Value'float32 v} ->
            [(name, primTypeConst IR.PrimFloat32 (floatToWord v))]
        Node'const{type_=Type'float64,value=Value'float64 v} ->
            [(name, primTypeConst IR.PrimFloat64 (doubleToWord v))]
        -- TODO: other constants.
        _ -> [] -- TODO

primTypeConst :: Integral a => IR.PrimType -> a -> IR.Decl
primTypeConst ty val = IR.DeclConst IR.WordConst
    { wordValue = fromIntegral val
    , wordType = IR.PrimType ty
    }

-- | Given the offset field from the capnp schema, a type, and a
-- default value, return a DataLoc describing the location of a field.
dataLoc :: Word32 -> Type -> Value -> IR.DataLoc
dataLoc offset ty defaultVal =
    let bitsOffset = fromIntegral offset * typeSize ty
    in IR.DataLoc
        { dataIdx = bitsOffset `div` 64
        , dataOff = bitsOffset `mod` 64
        , dataDef = valueBits defaultVal
        }

generateEnum :: Id -> NodeMap -> IR.Name -> Enumerant -> IR.Variant
generateEnum thisModule nodeMap parentName Enumerant{..} =
    IR.Variant
        { variantName = IR.subName parentName name
        , variantParams = IR.NoParams
        , variantTag = Just codeOrder
        }

-- | Return whether the field is part of a union within its struct.
isUnionField :: Field -> Bool
isUnionField Field'{..} = discriminantValue /= field'noDiscriminant

formatStructBody :: Id -> NodeMap -> IR.Name -> [Field] -> IR.VariantParams
formatStructBody thisModule nodeMap parentName fields = IR.Record $
    map (generateField thisModule nodeMap) (filter (not . isUnionField) fields)
    <> case filter isUnionField fields of
        [] -> [] -- no union
        _  ->
            [ IR.Field
                { fieldName = "union'"
                , fieldType = IR.StructType parentName []
                , fieldLoc = IR.HereField
                }
            ]

-- | Generate a variant of a type corresponding to an anonymous union in a
-- struct.
generateVariant :: Id -> NodeMap -> IR.Name -> Field -> IR.Variant
generateVariant thisModule nodeMap parentName Field'{..} = case union' of
    Field'slot{..} -> IR.Variant
        { variantName
        , variantParams = case type_ of
            Type'void -> IR.NoParams
            _         -> IR.Unnamed (formatType thisModule nodeMap type_) (getFieldLoc union')
        , variantTag = Just discriminantValue
        }
    Field'group{..} ->
        let NodeMetaData{node=node@Node'{..},..} = nodeMap M.! typeId
        in case union' of
            Node'struct{..} -> IR.Variant
                { variantName = variantName
                , variantParams =
                    formatStructBody thisModule nodeMap variantName $ V.toList fields
                , variantTag = Just discriminantValue
                }
            _               ->
                error "A group field referenced a non-struct node."
    Field'unknown' _ ->
        -- Some sort of field we don't know about (newer version of capnp probably).
        -- Generate the variant, but we don't know what the argument type should be,
        -- so leave it out.
        IR.Variant
            { variantName
            , variantParams = IR.NoParams
            , variantTag = Nothing
            }
  where
    variantName = IR.subName parentName (makeLegalName name)


generateField :: Id -> NodeMap -> Field -> IR.Field
generateField thisModule nodeMap Field'{..} =
    IR.Field
        { fieldName = makeLegalName name
        , fieldType = case union' of
            Field'slot{..}   -> formatType thisModule nodeMap type_
            Field'group{..} ->
                IR.StructType (identifierFromMetaData thisModule (nodeMap M.! typeId)) []
            Field'unknown' _ ->
                -- Don't know how to interpret this; we'll have to leave the argument
                -- opaque.
                IR.PrimType IR.PrimVoid
        , fieldLoc = getFieldLoc union'
        }

getFieldLoc :: Field' -> IR.FieldLoc
getFieldLoc Field'group{} = IR.HereField
getFieldLoc Field'slot{offset,type_,defaultValue,hadExplicitDefault} =
    case typeSection type_ of
        VoidSec ->
            IR.VoidField
        PtrSec
            | hadExplicitDefault -> error $
                "Error: capnpc-haskell does not support explicit default " ++
                "field values for pointer types. See:\n" ++
                "\n" ++
                "    https://github.com/zenhack/haskell-capnp/issues/28"
            | otherwise -> IR.PtrField (fromIntegral offset)
        DataSec ->
            IR.DataField (dataLoc offset type_ defaultValue)
getFieldLoc (Field'unknown' _) =
    -- Some field type we don't know about; we can't
    -- give a location for it, so call it void
    IR.VoidField

-- | Return the size of the type in units of the minimum size that makes
-- sense for the section of a struct in which it belongs -- bits for the
-- data section, and pointers for the pointer section.
typeSize :: Type -> Int
typeSize = \case
    Type'void -> 0
    Type'bool -> 1
    Type'int8 -> 8
    Type'int16 -> 16
    Type'int32 -> 32
    Type'int64 -> 64
    Type'uint8 -> 8
    Type'uint16 -> 16
    Type'uint32 -> 32
    Type'uint64 -> 64
    Type'float32 -> 32
    Type'float64 -> 64
    Type'text -> 1
    Type'enum{} -> 16
    Type'data_ -> 1
    Type'list{} -> 1
    Type'struct{} -> 1
    Type'interface{} -> 1
    Type'anyPointer{} -> 1
    -- something we don't know about; just call it 0, since we're not
    -- generating code for it anyway.
    Type'unknown' _ -> 0

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

typeSection :: Type -> Section
typeSection ty = case (typeSize ty, ty) of
    (0, _)         -> VoidSec
    (_, Type'bool) -> DataSec
    (1, _)         -> PtrSec
    _              -> DataSec

data Section = DataSec | PtrSec | VoidSec

formatType :: Id -> NodeMap -> Type -> IR.Type
formatType thisModule nodeMap ty = case ty of
    Type'void       -> IR.PrimType IR.PrimVoid
    Type'bool       -> IR.PrimType IR.PrimBool
    Type'int8       -> IR.PrimType IR.PrimInt {isSigned = True, size = 8}
    Type'int16      -> IR.PrimType IR.PrimInt {isSigned = True, size = 16}
    Type'int32      -> IR.PrimType IR.PrimInt {isSigned = True, size = 32}
    Type'int64      -> IR.PrimType IR.PrimInt {isSigned = True, size = 64}
    Type'uint8      -> IR.PrimType IR.PrimInt {isSigned = False, size = 8}
    Type'uint16     -> IR.PrimType IR.PrimInt {isSigned = False, size = 16}
    Type'uint32     -> IR.PrimType IR.PrimInt {isSigned = False, size = 32}
    Type'uint64     -> IR.PrimType IR.PrimInt {isSigned = False, size = 64}
    Type'float32    -> IR.PrimType IR.PrimFloat32
    Type'float64    -> IR.PrimType IR.PrimFloat64
    Type'text       -> IR.PrimType IR.PrimText
    Type'data_      -> IR.PrimType IR.PrimData
    Type'list elt   -> IR.ListOf (formatType thisModule nodeMap elt)
    -- TODO: use 'brand' to generate type parameters.
    Type'enum{..}   -> IR.EnumType (typeName typeId)
    Type'struct{..} -> IR.StructType (typeName typeId) []
    -- TODO: interfaces are not structs; handle them separately.
    Type'interface{..} -> IR.StructType (typeName typeId) []
    Type'anyPointer anyPtr -> IR.Untyped $
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
    _ -> IR.PrimType IR.PrimVoid -- TODO: constrained anyPointers
  where
    typeName typeId =
        identifierFromMetaData thisModule (nodeMap M.! typeId)

generateImport :: CodeGeneratorRequest'RequestedFile'Import -> IR.Import
generateImport CodeGeneratorRequest'RequestedFile'Import{..} =
    IR.Import (IR.ByCapnpId id)

cgrToIR :: CodeGeneratorRequest -> [IR.Module]
cgrToIR cgr@CodeGeneratorRequest{requestedFiles} =
    map (generateModule $ makeNodeMap cgr) $ V.toList requestedFiles
