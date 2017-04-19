module TmpUtil where

-- This stuff really ought to be pulled from some library, but I haven't yet
-- found the time to choose something appropriate.

import Control.Monad.Catch(MonadThrow, throwM, Exception, SomeException)
import Control.Monad.Catch.Pure(CatchT(..))
import Control.Monad.State(StateT(StateT, runStateT), MonadState, put, get)
import Control.Monad.Quota
import qualified Data.CapNProto.Message as M
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import Data.Bits
import Data.Binary.Get (Get, runGet, getWord64le, getWord32le)
import Control.Monad.Trans.Class (lift)
import Data.Word

data ShortRead = ShortRead deriving(Show)
instance Exception ShortRead

type Message = B.Vector (U.Vector Word64)

getMessage :: L.ByteString -> Int -> Either SomeException (Message, Quota)
getMessage contents quota = do
    runGet (runCatchT $ runQuotaT getMessage' (Quota quota)) contents
  where
    getMessage' :: QuotaT (CatchT Get) Message
    getMessage' = M.readMessage
        (lift $ lift getWord64le)
        (lift $ lift getWord32le)
