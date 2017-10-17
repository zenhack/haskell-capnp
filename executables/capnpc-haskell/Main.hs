{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}
module Main (main) where

import Control.Monad          (join, (>=>))
import Control.Monad.Quota    (Quota(..), evalQuotaT)
import Control.Monad.Writer   (MonadWriter, runWriterT, tell)
import Data.CapNProto.Message (Message, decode)

import qualified Data.ByteString                                                   as BS
import qualified Data.CapNProto.BasicTypes                                         as BT
import qualified Data.CapNProto.List                                               as List
import qualified Data.CapNProto.Untyped                                            as U
import qualified Data.Map.Strict                                                   as M
import qualified Schema.CapNProto.Reader.Schema                                    as Schema
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest               as CGR
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest.RequestedFile as ReqFile
import qualified Schema.CapNProto.Reader.Schema.Node                               as Node

buildNodeMap :: U.ReadCtx m b
             => List.ListOf b (Schema.Node b)
             -> m (M.Map Node.Id (Schema.Node b))
buildNodeMap = List.foldl addNode M.empty
  where
    addNode m node = do
        nodeId <- Node.id node
        return $ M.insert nodeId node m

codegen :: Message BS.ByteString -> Maybe (Int, [Node.Id])
codegen (CGR.root_ -> Just cgr) = do
    Just nodes <- CGR.nodes cgr
    nodeMap <- buildNodeMap nodes
    Just reqFiles <- CGR.requestedFiles cgr
    reqFiles' <- List.toList reqFiles
    return (length reqFiles', M.keys nodeMap)

main :: IO ()
main = BS.getContents >>= decode >>= print . codegen
