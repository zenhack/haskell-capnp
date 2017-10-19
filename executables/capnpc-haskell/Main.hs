{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}
module Main (main) where

import Control.Monad          ((>=>), void)
import Control.Monad.Quota    (Quota(..), evalQuotaT)
import Control.Monad.Writer   (MonadWriter, Writer, runWriterT, tell)
import Control.Monad.Catch.Pure (CatchT(..))
import Data.CapNProto.Message (Message, decode)

import qualified Language.Haskell.TH as TH
import qualified Language.CapNProto.TH as CTH
import qualified Data.ByteString                                                   as BS
import qualified Data.CapNProto.BasicTypes                                         as BT
import qualified Data.CapNProto.List                                               as List
import qualified Data.CapNProto.Untyped                                            as U
import qualified Data.Map.Strict                                                   as M
import qualified Schema.CapNProto.Reader.Schema                                    as Schema
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest               as CGR
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest.RequestedFile as ReqFile
import qualified Schema.CapNProto.Reader.Schema.Node                               as Node
import qualified Schema.CapNProto.Reader.Schema.Node.NestedNode                    as NN

type BS = BS.ByteString
type NS = [BT.Text BS]

type NodeMap = M.Map Node.Id (Schema.Node BS)

type Generator a = CatchT (Writer [(NS, TH.DecsQ)]) a

buildNodeMap :: List.ListOf BS (Schema.Node BS) -> Generator NodeMap
buildNodeMap = List.foldl addNode M.empty
  where
    addNode m node = do
        nodeId <- Node.id node
        return $ M.insert nodeId node m

genModule :: NodeMap -> CGR.RequestedFile BS -> Generator ()
genModule nodeMap file = do
    id <- ReqFile.id file
    let Just node = M.lookup id nodeMap

    -- First, verify that the node in question is actually a file:
    Node.File <- Node.union_ node

    Just nn <- Node.nestedNodes node
    List.mapM_ (genNestedNode nodeMap []) nn

genNestedNode :: NodeMap -> NS -> Node.NestedNode BS -> Generator ()
genNestedNode nodeMap ns nestedNode = do
    Just name <- NN.name nestedNode
    id <- NN.id nestedNode
    let Just node = M.lookup id nodeMap
    genNode nodeMap ns node name
    Just nn <- Node.nestedNodes node
    List.mapM_ (genNestedNode nodeMap (name:ns)) nn

genNode :: NodeMap -> NS -> Schema.Node BS -> (BT.Text BS) -> Generator ()
genNode nodeMap ns node name = case Node.union_ node of
    Just (Node.Struct struct) -> do
        undefined -- tell [(ns, mkStructWrapper name)]

genReq :: Message BS.ByteString -> Generator ()
genReq (CGR.root_ -> Right cgr) = do
    Just nodes <- CGR.nodes cgr
    nodeMap <- buildNodeMap nodes
    Just reqFiles <- CGR.requestedFiles cgr
    List.mapM_ (genModule nodeMap) reqFiles

main :: IO ()
main = do
    msg <- BS.getContents >>= decode
    void $ return $ runWriterT $ runCatchT $ genReq msg
