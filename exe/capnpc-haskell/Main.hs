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

-- Helper for makeNodeMap. TODO: document in more detial.
collectMetaData :: M.Map Id Node -> NodeMetaData -> [(Id, NodeMetaData)]
collectMetaData nodeMap meta@NodeMetaData{..} =
    (nodeId node, meta) : concatMap kid (V.toList $ toVector $ nestedNodes node)
  where
    nodeId Node{..} = id
    kid nn@Node'NestedNode{..} =
        collectMetaData
            nodeMap
            meta
                { node = nodeMap M.! id
                , namespace = mustDecodeUtf8 name : namespace
                }

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
    nodes' = toVector nodes
    rootNodes = V.filter (\Node{..} -> scopeId == 0) $ toVector nodes
    baseMap =
        toVector nodes
        & V.toList
        & map (\node@Node{..} -> (id, node))
        & M.fromList

moduleNameFromId :: Id -> String
moduleNameFromId = printf "Data.CapNProto.ById.X%x"

-- | Generate the source code for a module based on a RequstedFile.
generateFile :: NodeMap -> CodeGeneratorRequest'RequestedFile -> String
generateFile nodeMap CodeGeneratorRequest'RequestedFile{..} = intercalate "\n"
    [ "{-# LANGUAGE DuplicateRecordFields #-}"
    , "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    , "module " ++ moduleNameFromId id ++ " where"
    , ""
    , intercalate "\n" $ map generateImport $ V.toList $ toVector imports
    , ""
    -- , generateTypes nodeMap id []
    ]

{-

generateTypes :: NodeMap -> Id -> [String] -> String
generateTypes nodeMap id namespace =
    let Node{..} = nodeMap M.! id
    in case union' of
        Node'Struct{..} ->
            let name = intercalate "'" namespace
            in concat
                [ "data ", name, " = ", name
                , "{", intercalate ", " (map (generateField nodeMap) $ V.toList $ toVector fields)
                , "}"
                , "deriving(Show, Eq, Ord)"
                ]
        _ -> "" -- TODO

generateField :: NodeMap -> Field -> String
generateField nodeMap Field{..} = T.unpack (T.decodeUtf8 $ toBytes name) ++ " :: () {- TODO -}" -- ++ case union' of
    -- Field'Slot{..} -> formatType nodes type'
-}

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
