{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Generator

import Control.Monad.Catch           (MonadThrow)
import Control.Monad.Reader          (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class     (lift)
import Data.ByteString.UTF8          (toString)
import Data.CapNProto.Errors         (ThrowError)
import Data.CapNProto.Message        (Message, decode)
import Data.CapNProto.TraversalLimit (Limit, evalWithLimit)
import Namespace
import System.Directory              (createDirectoryIfMissing)
import System.FilePath               (takeDirectory)

import qualified Data.ByteString                                                   as BS
import qualified Data.CapNProto.BasicTypes                                         as BT
import qualified Data.CapNProto.List                                               as List
import qualified Data.Map.Strict                                                   as M
import qualified Language.CapNProto.TH                                             as CTH
import qualified Language.Haskell.TH                                               as TH
import qualified Schema.CapNProto.Reader.Schema                                    as Schema
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest               as CGR
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest.RequestedFile as ReqFile
import qualified Schema.CapNProto.Reader.Schema.Node                               as Node
import qualified Schema.CapNProto.Reader.Schema.Node.NestedNode                    as NN
import qualified Schema.CapNProto.Reader.Schema.Node.Union_.Struct                 as Node'Struct

-- TODO: right now in a number of places we're doing:
--
-- Just foo <- ...
--
-- In a do block. We should change this to something that will facilitate good
-- error messages.

type BS = BS.ByteString
type Generator m a = ReaderT NodeMap (GenT m) a
type NodeMap = M.Map Node.Id (Schema.Node BS)

buildNodeMap :: (Monad m, ThrowError m, Limit m) => List.ListOf BS (Schema.Node BS) -> m NodeMap
buildNodeMap = List.foldl addNode M.empty
  where
    addNode m node = do
        nodeId <- Node.id node
        return $ M.insert nodeId node m

genModule :: (Monad m, ThrowError m, Limit m) => CGR.RequestedFile BS -> Generator m ()
genModule file = do
    id <- ReqFile.id file
    Just node <- M.lookup id <$> ask

    -- First, verify that the node in question is actually a file:
    Node.File <- Node.union_ node

    nn <- Node.nestedNodes node
    -- We put everything in a module named Schema.CapNProto.Reader.X<fileId>.
    -- this makes module names deterministic, without annotations, regardless
    -- of where files are actually loaded from. We can in the future generate
    -- aliases that re-export these modules from friendlier names, but doing it
    -- this way means we don't have to worry about a lot of the awkwardness re:
    -- locating packages that crops up in other languages.
    List.mapM_ (genNestedNode (fromId id)) nn

genNestedNode :: (Monad m, ThrowError m, Limit m) => NS -> Node.NestedNode BS -> Generator m ()
genNestedNode ns nestedNode = do
    name <- NN.name nestedNode
    id <- NN.id nestedNode
    Just node <- M.lookup id <$> ask
    genNode ns node name
    nn <- Node.nestedNodes node
    List.mapM_ (genNestedNode (subNS ns name)) nn

genNode :: (Monad m, ThrowError m, Limit m)
        => NS -> Schema.Node BS -> (BT.Text BS) -> Generator m ()
genNode ns node (BT.Text name) = do
    union_ <- Node.union_ node
    case union_ of
        Node.Struct struct -> do
            fields <- Node'Struct.fields struct
            lift $ emit ns (CTH.mkStructWrappers [toString name])
            List.mapM_ (genField (subNS ns (BT.Text name))) fields
        _ -> return ()

genField :: (Monad m, ThrowError m, Limit m) => NS -> Schema.Field BS -> Generator m ()
genField ns field = return ()

generate :: (Monad m, ThrowError m, Limit m) => Message BS -> GenT m ()
generate msg = do
    cgr <- CGR.root_ msg
    nodes <- CGR.nodes cgr
    nodeMap <- lift $ buildNodeMap nodes
    runReaderT (genReq cgr) nodeMap

genReq :: (Monad m, ThrowError m, Limit m)
       => Schema.CodeGeneratorRequest BS -> Generator m ()
genReq cgr = do
    reqFiles <- CGR.requestedFiles cgr
    List.mapM_ genModule reqFiles

mkSrcs :: M.Map NS TH.DecsQ -> IO [(FilePath, String)]
mkSrcs = mapM mkSrc . M.toList where
    mkSrc :: (NS, TH.DecsQ) -> IO (FilePath, String)
    mkSrc (ns, q) = do
        decs <- TH.runQ q
        let BT.Text modname = moduleName ns
            filename = moduleFile ns
            contents = unlines $ map TH.pprint $ decs
        return ( filename
               , "module " ++ toString modname ++ " where\n" ++
                 "\n" ++
                 "import qualified Data.CapNProto.Untyped\n\n" ++
                 contents
               )

main :: IO ()
main = do
    msg <- BS.getContents >>= decode
    let quota = 1024 * 1024
    decls <- evalWithLimit quota $ evalGenT $ generate msg
    mkSrcs decls >>= mapM_ writeOut
  where
    writeOut (path, contents) = do
        createDirectoryIfMissing True (takeDirectory path)
        writeFile path contents
