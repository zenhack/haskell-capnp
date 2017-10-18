{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}
module Main (main) where

import Control.Monad          ((>=>), join)
import Control.Monad.Quota    (Quota(..), evalQuotaT)
import Control.Monad.Writer   (MonadWriter, Writer, runWriterT, tell)
import Control.Monad.Maybe (MaybeT(..))
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

type Generator a = MaybeT (Writer [(NS, TH.DecsQ)] a)

buildNodeMap :: List.ListOf BS (Schema.Node BS) -> Maybe NodeMap
buildNodeMap = List.foldl addNode M.empty
  where
    addNode m node = do
        nodeId <- Node.id node
        return $ M.insert nodeId node m

genModule :: NodeMap -> CGR.RequestedFile BS -> Generator ()
genModule nodeMap file = do
    id <- ReqFile.id file
    node <- M.lookup id nodeMap

    -- First, verify that the node in question is actually a file:
    Node.File <- Node.union_ node

    join (Node.nestedNodes node)
        >>= List.mapM_ (genNestedNode nodeMap [])

genNestedNode :: NodeMap -> NS -> Node.NestedNode BS -> Generator ()
genNestedNode nodeMap ns nestedNode = do
    Just name <- NN.name nestedNode
    id <- NN.id nestedNode
    node <- M.lookup id nodeMap
    genNode nodeMap ns node name
    join (Node.nestedNodes node) >>=
        List.mapM_ (genNestedNode nodeMap (name:ns))

genNode :: NodeMap -> NS -> Schema.Node BS -> (BT.Text BS) -> Generator ()
genNode nodeMap ns node name = case Node.union_ node of
    Just (Node.Struct struct) -> do
        undefined -- tell [(ns, mkStructWrapper name)]

genReq :: Message BS.ByteString -> Maybe ()
genReq (CGR.root_ -> Just cgr) = do
    Just nodes <- CGR.nodes cgr
    nodeMap <- buildNodeMap nodes
    join (CGR.requestedFiles cgr)
        >>= List.mapM_ (genModule nodeMap)

main :: IO ()
main = BS.getContents >>= decode >>= print . genReq
