module Main where

import Control.Monad.Quota
import Control.Monad.State
import Control.Monad.Catch.Pure (CatchT(..))
-- import Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import Data.CapNProto.Stream
import Data.CapNProto.Untyped (Message)
import Data.Bytes.Get
import Data.Serialize.Get
import Data.Int (Int64)

quota :: Quota Int64
quota = Quota 30 (64 * 1024 * 1024)

type Mon a = QuotaLimitT (CatchT (StateT (Quota Int64) Get))

main :: IO ()
main = do
    contents <- B.getContents
    let msg = runStateT (runCatchT (runQuotaLimitT (runGetLazy getMessage contents))) quota
    print msg

{-
    let foo = runGetL
    print =<< (runQuotaLimitT $ QuotaLimitT $ StateT $ get quota)
 where
    get :: IO (Message, Quota Int64)
    get = return . fst $ getMessage

--        getRootView msg defaultMaxPointerDepth
-}
