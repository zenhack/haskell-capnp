module Main (main) where

import Control.Monad.Quota        (Quota(..), evalQuotaT)
import Control.Monad.State.Strict (modify, runStateT)
import Data.CapNProto.Message     (Message, decode)

import qualified Data.ByteString                                     as BS
import qualified Data.CapNProto.List                                 as List
import qualified Data.CapNProto.Untyped                              as U
import qualified Data.Map.Strict                                     as M
import qualified Schema.CapNProto.Reader.Schema                      as Schema
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest as CGR
import qualified Schema.CapNProto.Reader.Schema.Node                 as Node

buildNodeMap :: U.ReadCtx m b
             => List.ListOf b (Schema.Node b)
             -> m (M.Map Node.Id (Schema.Node b))
buildNodeMap = List.foldl
    (\m node -> do node' <- node
                   M.insert <$> Node.id node'
                            <*> return node'
                            <*> m)
    (return M.empty)

codegen :: U.ReadCtx m b => Message b -> m [Node.Id]
codegen msg = do
    cgr <- CGR.root_ msg
    -- FIXME: handle 'Nothing' properly.
    Just nodes <- CGR.nodes cgr
    nodeMap <- buildNodeMap nodes
    return (M.keys nodeMap)

main :: IO ()
main = do
    contents <- BS.getContents
    msg <- decode contents
    result <- evalQuotaT (codegen msg) (Quota $ BS.length contents * 10)
    print result
