module TmpUtil where

-- This stuff really ought to be pulled from some library, but I haven't yet
-- found the time to choose something appropriate.

import Control.Monad.Catch(MonadThrow, throwM, Exception)
import Control.Monad.State(StateT(StateT, runStateT), MonadState, put, get)
import Control.Monad.Quota
import qualified Data.CapNProto.Message as M
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import Data.Bits
import Data.Word

data ShortRead = ShortRead deriving(Show)
instance Exception ShortRead

type Message = B.Vector (U.Vector Word64)

getMessage :: MonadThrow m => L.ByteString -> Int -> m Message
getMessage contents quota = do
    ((msg, _), _) <- runStateT (runQuotaT monadStack (Quota 8192))
                               (L.unpack contents)
    return msg
  where
    monadStack :: MonadThrow m => QuotaT (StateT [Word8] m) Message
    monadStack = do
           QuotaT $ \q ->
                return ( StateT $ \s -> return ((), s) :: IO ((), [Word8])
                       , q
                       )
           M.readMessage read64 read32

read32 :: (MonadState [Word8] m, MonadThrow m) => m Word32
read32 = do
    bytes <- get
    -- During tests, we seem to get as far as evaluating bytes in
    -- the condition of the case, but no further, as determined by
    -- placing calls to error. This is very strange, and still being
    -- debugged.
    case bytes of
        (a:b:c:d:bytes') -> do
            put bytes'
            return $ foldl (.|.) 0
                        [ fromIntegral a
                        , fromIntegral b `shiftL` 8
                        , fromIntegral c `shiftL` 16
                        , fromIntegral d `shiftL` 24
                        ]
        _ -> throwM $ ShortRead

read64 :: (MonadState [Word8] m, MonadThrow m) => m Word64
read64 = do
    bytes <- get
    case bytes of
        (a:b:c:d:e:f:g:h:bytes') -> do
            put bytes'
            return $ foldl (.|.) 0
                        [ fromIntegral a
                        , fromIntegral b `shiftL` 8
                        , fromIntegral c `shiftL` 16
                        , fromIntegral d `shiftL` 24
                        , fromIntegral e `shiftL` 32
                        , fromIntegral f `shiftL` 40
                        , fromIntegral g `shiftL` 48
                        , fromIntegral h `shiftL` 56
                        ]
        _ -> throwM $ ShortRead
