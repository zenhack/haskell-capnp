{-| This is the capnp compiler plugin.
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import Data.Capnp.Core.Schema

import Data.Word

import Codec.Capnp               (Decerialize(..))
import Data.Capnp.TraversalLimit (evalWithLimit)
import Data.Capnp.Untyped        (rootPtr)
import Data.Capnp.Untyped.Pure   (readStruct)

import qualified Data.Capnp.Message as Message
import qualified HsAst

import HsAst (HsFmt(..), mintercalate)

import Data.Function ((&))
import Data.Monoid   ((<>))
import Text.Printf   (printf)

import System.Directory (createDirectoryIfMissing)
import System.FilePath  (takeDirectory)

import qualified Data.ByteString        as BS
import qualified Data.Map.Strict        as M
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO      as TIO
import qualified Data.Vector            as V

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
identifierFromMetaData :: Id -> NodeMetaData -> T.Text
identifierFromMetaData thisModule NodeMetaData{..} =
    (if moduleId /= thisModule
        then moduleNameFromId moduleId <> "."
        else "")
    <> mintercalate "'" (reverse namespace)

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

main :: IO ()
main = do
    msg <- Message.decode =<< BS.getContents
    -- Traversal limit is 64 MiB. Somewhat aribtrary.
    cgr@CodeGeneratorRequest{..} <- evalWithLimit (64 * 1024 * 1024) (rootPtr msg >>= readStruct >>= decerialize)
    mapM_ saveResult (handleCGR cgr)
  where
    saveResult (filename, contents) = do
        createDirectoryIfMissing True (takeDirectory filename)
        TIO.writeFile filename $ TB.toLazyText contents

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

-- | Translate a capnproto id to a fully-qualified haskell module name.
-- We name our modules Data.Capnp.ById.X<schema-id>, because this
-- makes it easy for one generated module to import another without
-- the troubles with finding the correct namespace that crop up with
-- other implmentations -- no special annotations, no worrying about
-- what the namespaces of generated modules are.
--
-- We will also want to probably output a module with a more human-friendly
-- name, which re-exports everything from this module, but this is still
-- TODO.
moduleNameFromId :: Id -> T.Text
moduleNameFromId = T.pack . printf "Data.Capnp.ById.X%x.Pure"

-- | @'untypedName' name@ is the fully qualified name for @name@ defined
-- within the pure-untyped module.
untypedName :: T.Text -> HsAst.Name
untypedName name = HsAst.Name ["Data.Capnp.Untyped.Pure." <> name]

-- | Generate the source code for a module based on a RequestedFile.
generateFile :: NodeMap -> CodeGeneratorRequest'RequestedFile -> TB.Builder
generateFile nodeMap CodeGeneratorRequest'RequestedFile{..} = mintercalate "\n"
    [ "{-# LANGUAGE DuplicateRecordFields #-}"
    , "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    , "module " <> TB.fromText (moduleNameFromId id) <> " where"
    , ""
    , "-- generated from " <> TB.fromText filename
    , ""
    , "import Data.Int"
    , "import Data.Word"
    , ""
    , "import Data.Capnp.Untyped.Pure (List)"
    , "import Data.Capnp.BuiltinTypes.Pure (Data, Text)"
    , ""
    , "import qualified Data.Capnp.Untyped.Pure"
    , "import qualified Codec.Capnp"
    , ""
    , mintercalate "\n" $ map generateImport $ V.toList imports
    , ""
    , mconcat $ map (mconcat . map hsFmt . generateTypes id nodeMap)
        $ filter (\NodeMetaData{..} -> moduleId == id)
        $ map snd
        $ M.toList nodeMap
    ]


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

generateTypes :: Id -> NodeMap -> NodeMetaData -> [HsAst.DataDef]
generateTypes thisModule nodeMap meta@NodeMetaData{..} =
    let Node'{..} = node
        name = identifierFromMetaData moduleId meta
    in case union' of
        Node'struct{..} | neededByParent nodeMap node ->
            let allFields = V.toList fields
                unionFields = filter isUnionField allFields
                commonFields = filter (not . isUnionField) allFields
                typeName = HsAst.Name [name]
                -- variants to generate that go inside the union:
                unionVariants =
                    map (generateVariant thisModule nodeMap name) unionFields
                    ++
                    -- Every union gets an extra "unknown" varaint, which is used
                    -- whenever what's on the wire has a discriminant that's not
                    -- in our schema.
                    [ HsAst.NormalVariant
                        (HsAst.Name [name, "unknown'"])
                        (Just $ HsAst.Type "Word16" [])
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
                    [ HsAst.DataDef
                        { dataName = typeName
                        , dataVariants = [formatStructBody thisModule nodeMap typeName allFields]
                        , dataTagLoc = Nothing
                        }
                    ]
                (_:_, []) ->
                    -- The struct is just one big anonymous union; expand the variants
                    -- in-line, rather than making a wrapper.
                    [ HsAst.DataDef
                        { dataName = typeName
                        , dataVariants = unionVariants
                        , dataTagLoc = Just (dataLoc discriminantOffset Type'uint16)
                        }
                    ]
                (_:_, _:_) ->
                    -- There are both common fields and an anonymous union. Generate
                    -- an auxiliary type for the union.
                    let unionName = HsAst.Name [name, ""]
                    in  [ HsAst.DataDef
                            { dataName = typeName
                            , dataVariants = [formatStructBody thisModule nodeMap unionName allFields]
                            , dataTagLoc = Nothing
                            }
                        , HsAst.DataDef
                            { dataName = unionName
                            , dataVariants = unionVariants
                            , dataTagLoc = Just (dataLoc discriminantOffset Type'uint16)
                            }
                        ]
        Node'enum{..} ->
            [ HsAst.DataDef
                { dataName = HsAst.Name [name]
                , dataVariants =
                    map (generateEnum thisModule nodeMap name) (V.toList enumerants)
                    <> [ HsAst.NormalVariant
                            { HsAst.variantName = HsAst.Name [name, "unknown'"]
                            , HsAst.variantType = Just $ HsAst.Type "Word16" []
                            }
                       ]
                , dataTagLoc = Nothing
                }
            ]
        _ -> [] -- TODO

-- | Given the offset field from the capnp schema and a type, return a DataLoc
-- describing the location of a field.
dataLoc :: Word32 -> Type -> HsAst.DataLoc
dataLoc offset ty =
    let bitsOffset = fromIntegral offset * typeSize ty
    in HsAst.DataLoc
        { dataIdx = bitsOffset `div` 64
        , dataOff = bitsOffset `mod` 64
        }

generateEnum :: Id -> NodeMap -> T.Text -> Enumerant -> HsAst.Variant
generateEnum thisModule nodeMap parentName Enumerant{..} =
    HsAst.NormalVariant
        { HsAst.variantName = HsAst.Name [parentName, name]
        , HsAst.variantType = Nothing
        }

-- | Return whether the field is part of a union within its struct.
isUnionField :: Field -> Bool
isUnionField Field'{..} = discriminantValue /= field'noDiscriminant

formatStructBody :: Id -> NodeMap -> HsAst.Name -> [Field] -> HsAst.Variant
formatStructBody thisModule nodeMap parentName fields = HsAst.Record
    parentName
    $ map (generateField thisModule nodeMap) (filter (not . isUnionField) fields)
    <> case filter isUnionField fields of
        [] -> [] -- no union
        _  ->
            [ HsAst.Field
                { fieldName = "union'"
                , fieldType = HsAst.Type parentName []
                , fieldLoc = HsAst.HereField
                }
            ]

-- | Generate a variant of a type corresponding to an anonymous union in a
-- struct.
generateVariant :: Id -> NodeMap -> T.Text -> Field -> HsAst.Variant
generateVariant thisModule nodeMap parentName Field'{..} = case union' of
    Field'slot{..} -> HsAst.NormalVariant variantName $
        case type_ of
            Type'void -> Nothing
            _         -> Just $ formatType thisModule nodeMap type_
    Field'group{..} ->
        let NodeMetaData{node=node@Node'{..},..} = nodeMap M.! typeId
        in case union' of
            Node'struct{..} ->
                formatStructBody thisModule nodeMap variantName $ V.toList fields
            _               ->
                error "A group field referenced a non-struct node."
    Field'unknown' _ ->
        -- Some sort of field we don't know about (newer version of capnp probably).
        -- Generate the variant, but we don't know what the argument type should be,
        -- so leave it out.
        HsAst.NormalVariant variantName Nothing
  where
    variantName = HsAst.Name [parentName, makeLegalName name]


generateField :: Id -> NodeMap -> Field -> HsAst.Field
generateField thisModule nodeMap Field'{..} =
    HsAst.Field
        { fieldName = makeLegalName name
        , fieldType = case union' of
            Field'slot{..}   -> formatType thisModule nodeMap type_
            Field'group{..} ->
                HsAst.Type (HsAst.Name [identifierFromMetaData thisModule (nodeMap M.! typeId)]) []
            Field'unknown' _ ->
                -- Don't know how to interpret this; we'll have to leave the argument
                -- opaque.
                HsAst.Unit
        , fieldLoc = case union' of
            Field'group{} ->
                HsAst.HereField
            Field'slot{offset,type_} ->
                case typeSection type_ of
                    VoidSec ->
                        HsAst.VoidField
                    PtrSec ->
                        HsAst.PtrField (fromIntegral offset)
                    DataSec ->
                        HsAst.DataField (dataLoc offset type_)
            Field'unknown' _ ->
                -- Some field tpe we don't know about; we can't
                -- give a location for it, so call it void
                HsAst.VoidField
        }

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

typeSection :: Type -> Section
typeSection ty = case (typeSize ty, ty) of
    (0, _)         -> VoidSec
    (_, Type'bool) -> DataSec
    (1, _)         -> PtrSec
    _              -> DataSec

data Section = DataSec | PtrSec | VoidSec

formatType :: Id -> NodeMap -> Type -> HsAst.Type
formatType thisModule nodeMap ty = case ty of
    Type'void       -> HsAst.Unit
    Type'bool       -> HsAst.Type "Bool" []
    Type'int8       -> HsAst.Type "Int8" []
    Type'int16      -> HsAst.Type "Int16" []
    Type'int32      -> HsAst.Type "Int32" []
    Type'int64      -> HsAst.Type "Int64" []
    Type'uint8      -> HsAst.Type "Word8" []
    Type'uint16     -> HsAst.Type "Word16" []
    Type'uint32     -> HsAst.Type "Word32" []
    Type'uint64     -> HsAst.Type "Word64" []
    Type'float32    -> HsAst.Type "Float" []
    Type'float64    -> HsAst.Type "Double" []
    Type'text       -> HsAst.Type "Text" []
    Type'data_      -> HsAst.Type "Data" []
    Type'list elt   -> HsAst.Type "List" [formatType thisModule nodeMap elt]
    Type'enum{..} -> namedType typeId brand
    Type'struct{..} -> namedType typeId brand
    Type'interface{..} -> namedType typeId brand
    Type'anyPointer anyPtr ->
        HsAst.Type "Maybe"
            [ HsAst.Type
                (case anyPtr of
                    Type'anyPointer'unconstrained Type'anyPointer'unconstrained'anyKind ->
                        untypedName "PtrType"
                    Type'anyPointer'unconstrained Type'anyPointer'unconstrained'struct ->
                        untypedName "Struct"
                    Type'anyPointer'unconstrained Type'anyPointer'unconstrained'list ->
                        untypedName "List'" -- Note the '; this is an untyped List.
                    Type'anyPointer'unconstrained Type'anyPointer'unconstrained'capability ->
                        untypedName "Cap"
                    _ ->
                        -- Something we don't know about; assume it could be anything.
                        untypedName "PtrType")
                []
            ]
    _ -> HsAst.Type "() {- TODO: constrained anyPointers -}" []
  where
    namedType typeId brand = HsAst.Type
        (HsAst.Name [identifierFromMetaData thisModule (nodeMap M.! typeId)])
        -- TODO: use brand.
        []

generateImport :: CodeGeneratorRequest'RequestedFile'Import -> TB.Builder
generateImport CodeGeneratorRequest'RequestedFile'Import{..} =
    "import qualified " <> TB.fromText (moduleNameFromId id)

handleCGR :: CodeGeneratorRequest -> [(FilePath, TB.Builder)]
handleCGR cgr@CodeGeneratorRequest{..} = V.toList $
    let nodeMap = makeNodeMap cgr
    in fmap
        (\reqFile@CodeGeneratorRequest'RequestedFile{..} ->
            ( printf "Data/Capnp/ById/X%x/Pure.hs" id
            , generateFile nodeMap reqFile)
            )
        requestedFiles
