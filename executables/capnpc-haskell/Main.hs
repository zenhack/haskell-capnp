{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Main (main) where

import Control.Monad            (void, (>=>))
import Control.Monad.Catch.Pure (CatchT(..))
import Control.Monad.Quota      (Quota(..), evalQuotaT)
import Control.Monad.Writer     (MonadWriter, Writer, runWriterT, tell)
import Data.ByteString.UTF8     (toString)
import Data.CapNProto.Message   (Message, decode)
import Data.Functor.Identity    (Identity(..))
import Data.List                (intersperse)
import Data.Monoid              (mconcat, (<>))

import qualified Data.ByteString                                                   as BS
import qualified Data.CapNProto.BasicTypes                                         as BT
import qualified Data.CapNProto.List                                               as List
import qualified Data.CapNProto.Untyped                                            as U
import qualified Data.Map.Strict                                                   as M
import qualified Language.CapNProto.TH                                             as CTH
import qualified Language.Haskell.TH                                               as TH
import qualified Schema.CapNProto.Reader.Schema                                    as Schema
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest               as CGR
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest.RequestedFile as ReqFile
import qualified Schema.CapNProto.Reader.Schema.Node                               as Node
import qualified Schema.CapNProto.Reader.Schema.Node.NestedNode                    as NN

type BS = BS.ByteString
type NS = [BT.Text BS]

type NodeMap = M.Map Node.Id (Schema.Node BS)

type Generator a = CatchT (Writer [(NS, TH.DecsQ)]) a

modulePath :: NS -> BT.Text BS
modulePath ns = mconcat $
    "Schema.CapNProto.Reader." :
    reverse (intersperse "." ns)

moduleFile :: NS -> FilePath
moduleFile ns = asStr $ mconcat ( "Schema/CapNproto/Reader/"
                                : reverse (intersperse "/" ns)
                                ) <> ".hs"
  where
    asStr (BT.Text path) = toString path

buildNodeMap :: List.ListOf BS (Schema.Node BS) -> Generator NodeMap
buildNodeMap = List.foldl addNode M.empty
  where
    addNode m node = do
        nodeId <- Node.id node
        return $ M.insert nodeId node m

genModule :: NodeMap -> CGR.RequestedFile BS -> Generator ()
genModule nodeMap (ReqFile.id -> Right id) = do
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
genNode nodeMap ns node (BT.Text name) = case Node.union_ node of
    Right (Node.Struct struct) -> do
        tell [ (ns, CTH.mkStructWrappers [toString name])]

genReq :: Message BS.ByteString -> Generator ()
genReq (CGR.root_ -> Right (split CGR.nodes CGR.requestedFiles ->
                            (Right (Just nodes), Right (Just reqFiles)))) = do
    nodeMap <- buildNodeMap nodes
    List.mapM_ (genModule nodeMap) reqFiles
split f g x = (f x, g x)


main :: IO ()
main = do
    msg <- BS.getContents >>= decode
    case runWriterT $ runCatchT $ genReq msg of
        Identity (Right (), decls) -> undefined
