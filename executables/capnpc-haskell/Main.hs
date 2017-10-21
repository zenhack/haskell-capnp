{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Main (main) where

import Generator

import Control.Monad.Trans.Class (lift)
import Control.Monad.Catch.Pure  (CatchT(..))
import Control.Monad.Reader      (ReaderT, runReaderT, ask)
import Data.ByteString.UTF8      (toString)
import Data.CapNProto.Message    (Message, decode)
import Data.Functor.Identity     (Identity(..))
import Namespace
import System.Directory          (createDirectoryIfMissing)
import System.FilePath           (takeDirectory)

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

type BS = BS.ByteString

type NodeMap = M.Map Node.Id (Schema.Node BS)

type Generator a = ReaderT NodeMap (GenT (CatchT Identity)) a

buildNodeMap :: List.ListOf BS (Schema.Node BS) -> (CatchT Identity) NodeMap
buildNodeMap = List.foldl addNode M.empty
  where
    addNode m node = do
        nodeId <- Node.id node
        return $ M.insert nodeId node m

genModule :: CGR.RequestedFile BS -> Generator ()
genModule (ReqFile.id -> Right id) = do
    Just node <- M.lookup id <$> ask

    -- First, verify that the node in question is actually a file:
    Node.File <- Node.union_ node

    Just nn <- Node.nestedNodes node
    -- We put everything in a module named Schema.CapNProto.Reader.X<fileId>.
    -- this makes module names deterministic, without annotations, regardless
    -- of where files are actually loaded from. We can in the future generate
    -- aliases that re-export these modules from friendlier names, but doing it
    -- this way means we don't have to worry about a lot of the awkwardness re:
    -- locating packages that crops up in other languages.
    List.mapM_ (genNestedNode (fromId id)) nn

genNestedNode :: NS -> Node.NestedNode BS -> Generator ()
genNestedNode ns nestedNode = do
    Just name <- NN.name nestedNode
    id <- NN.id nestedNode
    Just node <- M.lookup id <$> ask
    genNode ns node name
    Just nn <- Node.nestedNodes node
    List.mapM_ (genNestedNode (subNS ns name)) nn

genNode :: NS -> Schema.Node BS -> (BT.Text BS) -> Generator ()
genNode ns node (BT.Text name) = case Node.union_ node of
    Right (Node.Struct struct) -> do
        lift $ emit ns (CTH.mkStructWrappers [toString name])
    _ -> return ()

generate :: Message BS -> GenT (CatchT Identity) ()
generate (CGR.root_ -> Right cgr@(CGR.nodes -> Right (Just nodes))) = do
    nodeMap <- lift $ buildNodeMap nodes
    runReaderT (genReq cgr) nodeMap

genReq :: Schema.CodeGeneratorRequest BS -> Generator ()
genReq (CGR.requestedFiles -> Right (Just reqFiles)) =
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
    case runIdentity $ runCatchT $ evalGenT $ generate msg of
        (Right decls) -> mkSrcs decls >>= mapM_ writeOut
  where
    writeOut (path, contents) = do
        createDirectoryIfMissing True (takeDirectory path)
        writeFile path contents
