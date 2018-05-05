{-| This is the capnp compiler plugin.
-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.CapNProto.Core.Schema

import Data.CapNProto.TraversalLimit (evalWithLimit)
import Data.CapNProto.Untyped        (rootPtr)
import Data.CapNProto.Untyped.ADT    (List(..), Text(..), readStruct)

import qualified Data.CapNProto.Message as Message

import Data.Function ((&))
import Data.List     (intercalate)
import Text.Printf   (printf)

import qualified Data.ByteString    as BS
import qualified Data.Map.Strict    as M
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector        as V

-- General TODO: we should avoid using String in favor of Text.Builder, and
-- also avoid asymptotically inefficient uses of lists.

mustDecodeUtf8 :: Text -> String
mustDecodeUtf8 = T.unpack . T.decodeUtf8 . toBytes

type NodeMap = M.Map Id NodeMetaData

-- | Some useful metadata about a node.
data NodeMetaData = NodeMetaData
    { moduleId  :: Id
    -- ^ The id of the module that this node belongs to.
    , namespace :: [String]
    -- ^ The namespace within a file that the node is defined.
    , node      :: Node
    -- ^ The node itself.
    }
    deriving(Show, Read, Eq)

-- | @'identifierFromMetaData' thisModule meta@ return a haskell identifier
-- for a node based on the metadata @meta@, and @thisModule@, the id for
-- the module in which the name will be used.
identifierFromMetaData :: Id -> NodeMetaData -> String
identifierFromMetaData thisModule NodeMetaData{..} =
    (if moduleId /= thisModule
        then moduleNameFromId moduleId ++ "."
        else "")
    ++ intercalate "'" (reverse namespace)

-- Helper for makeNodeMap. TODO: document in more detial.
collectMetaData :: M.Map Id Node -> NodeMetaData -> [(Id, NodeMetaData)]
collectMetaData nodeMap meta@NodeMetaData{..} =
    (nodeId node, meta) : concatMap kid (V.toList $ toVector $ nestedNodes node)
  where
    nodeId Node{..} = id
    kid nn@Node'NestedNode{..} = case M.lookup id nodeMap of
        Just node -> collectMetaData
                        nodeMap
                        meta
                        { node = node
                        , namespace = makeLegalName (mustDecodeUtf8 name) : namespace
                        }
        Nothing ->
            -- Imperically, this can happen if e.g. the node isn't in a subtree
            -- of a RequestedFile, and not actually used anywhere else. This
            -- crops up with c++.capnp:name when processing schema.capnp, for
            -- example. In this case, just leave it out of the map:
            []

-- | Convert the argument into a valid haskell identifier. This doesn't handle
-- every possible violation, just ones that might occur in legal schema.
makeLegalName :: String -> String
makeLegalName str
    | str `elem` keywords = str ++ "_"
    | otherwise = str
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
    mapM_ printResult (handleCGR cgr)
  where
    printResult (filename, contents) = do
        putStrLn $ "-- " ++ filename
        putStrLn contents

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
    rootNodes = V.filter (\Node{..} -> scopeId == 0) $ toVector nodes
    baseMap =
        toVector nodes
        & V.toList
        & map (\node@Node{..} -> (id, node))
        & M.fromList

-- | Translate a capnproto id to a fully-qualified haskell module name.
-- We name our modules Data.CapNProto.ById.X<schema-id>, because this
-- makes it easy for one generated module to import another without
-- the troubles with finding the correct namespace that crop up with
-- other implmentations -- no special annotations, no worrying about
-- what the namespaces of generated modules are.
--
-- We will also want to probably output a module with a more human-friendly
-- name, which re-exports everything from this module, but this is still
-- TODO.
moduleNameFromId :: Id -> String
moduleNameFromId = printf "Data.CapNProto.ById.X%x"

-- | Generate the source code for a module based on a RequestedFile.
generateFile :: NodeMap -> CodeGeneratorRequest'RequestedFile -> String
generateFile nodeMap CodeGeneratorRequest'RequestedFile{..} = intercalate "\n"
    [ "{-# LANGUAGE DuplicateRecordFields #-}"
    , "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    , "module " ++ moduleNameFromId id ++ " where"
    , ""
    , "import Data.Int"
    , "import Data.Word"
    , ""
    , "import Data.CapNProto.Untyped.ADT (Text, Data, List)"
    , ""
    , intercalate "\n" $ map generateImport $ V.toList $ toVector imports
    , ""
    , concatMap (generateTypes id nodeMap)
        $ filter (\NodeMetaData{..} -> moduleId == id)
        $ map snd
        $ M.toList nodeMap
    ]


generateTypes :: Id -> NodeMap -> NodeMetaData -> String
generateTypes thisModule nodeMap meta@NodeMetaData{..} =
    let Node{..} = node
    in case union' of
        Node'Struct{..} ->
            let name = identifierFromMetaData moduleId meta
                allFields = V.toList $ toVector fields
            in concat
                [ "data ", name, " = ", name
                , formatStructBody thisModule nodeMap name allFields
                , " deriving(Show, Read, Eq)\n\n"
                , case filter isUnionField allFields of
                    [] -> "" -- No union.
                    unionFields -> concat
                        [ "data ", name, "'\n    = "
                        , intercalate "\n    | " $ map (generateVariant thisModule nodeMap name) unionFields
                        , "\n    deriving(Show, Eq, Ord)\n\n"
                        ]
                ]
        _ -> "" -- TODO

-- | Return whether the field is part of a union within its struct.
isUnionField :: Field -> Bool
isUnionField Field{..} = discriminantValue /= field'noDiscriminant

formatStructBody :: Id -> NodeMap -> String -> [Field] -> String
formatStructBody thisModule nodeMap parentName fields =
    concat
        [ "\n    { "
        , intercalate "\n    , " $
            map (generateField thisModule nodeMap) (filter (not . isUnionField) fields)
            ++ case filter isUnionField fields of
                        [] -> [] -- No union.
                        _  -> ["union' :: " ++ parentName ++ "'"]
        , "\n    }"
        ]

-- | Generate a variant type corresponding to an anonymous union in a struct.
--
-- If the generated type for the struct is Foo, then the type for the union
-- will be Foo'.
generateVariant :: Id -> NodeMap -> String -> Field -> String
generateVariant thisModule nodeMap parentName Field{..} =
    let variantName = parentName ++ "'" ++ makeLegalName (mustDecodeUtf8 name)
    in variantName
        ++ case union' of
            Field'Slot Field'Slot'{..} ->
                case type' of
                    Type Type'Void -> ""
                    _              -> " " ++ formatType thisModule nodeMap type'
            Field'Group Field'Group'{..} ->
                let NodeMetaData{..} = nodeMap M.! typeId
                                        -- FIXME: for some reason this is coming up
                                        -- missing for the Field.slot id. I've
                                        -- inspected the cgr manually and the
                                        -- corresponding node id is there; need to
                                        -- investigate further.
                    Node{..} = node
                in case union' of
                    Node'Struct{..} ->
                        formatStructBody thisModule nodeMap variantName (V.toList $ toVector fields)
                    _               ->
                        error "A group field referenced a non-struct node."


generateField :: Id -> NodeMap -> Field -> String
generateField thisModule nodeMap Field{..} =
    makeLegalName (T.unpack (T.decodeUtf8 $ toBytes name))
        ++ " :: "
        ++ case union' of
            Field'Slot Field'Slot'{..}   -> formatType thisModule nodeMap type'
            Field'Group Field'Group'{..} -> "() {- TODO: group -}"

formatType :: Id -> NodeMap -> Type -> String
formatType thisModule nodeMap (Type ty) = case ty of
    Type'Void       -> "()"
    Type'Bool       -> "Bool"
    Type'Int8       -> "Int8"
    Type'Int16      -> "Int16"
    Type'Int32      -> "Int32"
    Type'Int64      -> "Int64"
    Type'Uint8      -> "Word8"
    Type'Uint16     -> "Word16"
    Type'Uint32     -> "Word32"
    Type'Uint64     -> "Word64"
    Type'Float32    -> "Float"
    Type'Float64    -> "Double"
    Type'Text       -> "Text"
    Type'Data       -> "Data"
    Type'List elt   -> "List (" ++ formatType thisModule nodeMap elt ++ ")"
    Type'Enum{..} -> namedType typeId brand
    Type'Struct{..} -> namedType typeId brand
    Type'Interface{..} -> namedType typeId brand
    _               -> "() {- TODO: anyPointer-}"
  where
    namedType typeId brand =
        -- TODO: use brand.
        identifierFromMetaData thisModule (nodeMap M.! typeId)

generateImport :: CodeGeneratorRequest'RequestedFile'Import -> String
generateImport CodeGeneratorRequest'RequestedFile'Import{..} =
    "import qualified " ++ moduleNameFromId id

handleCGR :: CodeGeneratorRequest -> [(FilePath, String)]
handleCGR cgr@CodeGeneratorRequest{..} = V.toList $
    let nodeMap = makeNodeMap cgr
    in fmap
        (\reqFile@CodeGeneratorRequest'RequestedFile{..} ->
            (mustDecodeUtf8 filename, generateFile nodeMap reqFile))
        (toVector requestedFiles)
