module Main where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.Quota (QuotaT(..), Quota(..))
import Control.Monad.State (MonadState, put, get, StateT(..))
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString.Lazy as L
import qualified Data.CapNProto.Message.Vector as V
import Data.Word (Word8, Word32, Word64)


main :: IO ()
main = do
    contents <- L.unpack <$> L.getContents
    msg <- runStateT (runQuotaT monadStack (Quota 8192)) contents
    print msg
  where
    monadStack :: QuotaT (StateT [Word8] IO) V.Message
    monadStack = do
           QuotaT $ \q ->
                return ( StateT $ \s -> return ((), s) :: IO ((), [Word8])
                       , q
                       )
           V.readMessage read64 read32

-- The rest of this ought to go somewhere else (or, better yet, we should just
-- use something from some library I haven't found yet):

data ShortRead = ShortRead deriving(Show)
instance Exception ShortRead

read32 :: (MonadState [Word8] m, MonadThrow m) => m Word32
read32 = do
    bytes <- get
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
