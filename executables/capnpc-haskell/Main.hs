module Main (main) where

import Control.Monad.Quota (evalQuotaT, Quota(..))

import qualified Data.ByteString        as BS
import qualified Data.CapNProto.Message as M
import qualified Data.CapNProto.Untyped as U
import qualified Schema.CapNProto.Reader.Schema.CodeGeneratorRequest as CGReq


codegen :: U.ReadCtx m b => M.Message b -> m ()
codegen msg = do
    root <- CGReq.root_ msg
    return ()

main :: IO ()
main = do
    contents <- BS.getContents
    msg <- M.decode contents
    result <- evalQuotaT (codegen msg) (Quota $ BS.length contents * 10)
    print result
