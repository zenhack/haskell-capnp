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
import qualified Data.Map           as M
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector        as V

main :: IO ()
main = do
    msg <- Message.decode =<< BS.getContents
    -- Traversal limit is 64 MiB. Somewhat aribtrary.
    cgr <- evalWithLimit (64 * 1024 * 1024) (rootPtr msg >>= readStruct >>= decerialize)
    let (_, moduleText) = handleCGR cgr V.! 0
    putStrLn moduleText

allNodes :: CodeGeneratorRequest -> M.Map Id Node
allNodes CodeGeneratorRequest{..} =
    toVector nodes
    & V.toList
    & map (\node@Node{..} -> (id, node))
    & M.fromList

moduleNameFromId :: Id -> String
moduleNameFromId = printf "Data.CapNProto.ById.X%x"

generateFile :: M.Map Id Node -> CodeGeneratorRequest'RequestedFile -> String
generateFile nodeMap CodeGeneratorRequest'RequestedFile{..} = intercalate "\n"
    [ "{-# LANGUAGE DuplicateRecordFields #-}"
    , "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    , "module " ++ moduleNameFromId id ++ " where"
    , ""
    , intercalate "\n" $ map generateImport $ V.toList $ toVector imports
    , ""
    , generateTypes nodeMap id []
    ]

generateTypes :: M.Map Id Node -> Id -> [String] -> String
generateTypes nodeMap id namespace = case M.lookup id nodeMap of
    Nothing -> error $ printf "Referenced node with id 0x%x does not exist!" id
    Just Node{..} -> case union' of
        Node'Struct{..} ->
            let name = intercalate "'" namespace
            in concat
                [ "data ", name, " = ", name
                , "{", intercalate ", " (map (generateField nodeMap) $ V.toList $ toVector fields)
                , "}"
                , "deriving(Show, Eq, Ord)"
                ]
        _ -> "" -- TODO

generateField :: M.Map Id Node -> Field -> String
generateField nodeMap Field{..} = T.unpack (T.decodeUtf8 $ toBytes name) ++ " :: () {- TODO -}" -- ++ case union' of
    -- Field'Slot{..} -> formatType nodes type'

generateImport :: CodeGeneratorRequest'RequestedFile'Import -> String
generateImport CodeGeneratorRequest'RequestedFile'Import{..} =
    "import qualified " ++ moduleNameFromId id

handleCGR :: CodeGeneratorRequest -> V.Vector (Text, String)
handleCGR cgr@CodeGeneratorRequest{..} =
    let nodeMap = allNodes cgr
    in fmap
        (\reqFile@CodeGeneratorRequest'RequestedFile{..} ->
            (filename, generateFile nodeMap reqFile))
        (toVector requestedFiles)
