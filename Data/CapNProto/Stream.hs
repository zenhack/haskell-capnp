{-|
Module : Data.CapNProto.Stream
Description : Serialize CapNProto messages over streams.

This module implements the encoding described at:

https://capnproto.org/encoding.html#serialization-over-a-stream

-}
module Data.CapNProto.Stream where
{-
    ( Segment
    , Message
    , getMessage
    , putMessage
    , StreamReader(..)
    , StreamWriter(..)
    , QuotaStreamReader(..)
    ) where

import Prelude hiding (length, lookup)
import Data.CapNProto.List
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as B
import Data.Word (Word32, Word64)
import Control.Monad (when, void)
import Control.Monad.Catch (MonadThrow, throwM, Exception)
import Control.Monad.Quota (MonadQuota(..))
import Control.Monad.Trans (MonadTrans(..))
import qualified Data.Vector as BV

type Message = BV.Vector Segment
type Segment = B.ByteString

class (Monad m) => StreamReader m where
    getWord32le   :: m Word32
    getByteString :: Int -> m B.ByteString
    getWord32le = do
        bytes <- getByteString 4
        let [b0, b1, b2, b3] = (map fromIntegral (B.unpack bytes)) :: [Word32]
        return $
            (b0 `shiftL`  0) .|.
            (b1 `shiftL`  8) .|.
            (b2 `shiftL` 16) .|.
            (b3 `shiftL` 24)

class StreamWriter m where
    putWord32le :: Word32 -> m ()
    putByteString :: B.ByteString -> m ()

newtype QuotaStreamReader m a = QuotaStreamReader { runQuotaStreamReader :: m a }

instance Monad m => Monad (QuotaStreamReader m) where
    return = QuotaStreamReader . return
    (QuotaStreamReader m) >>= f = QuotaStreamReader $ do
        x <- m
        runQuotaStreamReader (f x)
instance (Monad m) => Applicative (QuotaStreamReader m) where
    pure = return
    f <*> x = do
        x' <- x
        f' <- f
        return (f' x')
instance (Monad m) => Functor (QuotaStreamReader m) where
    fmap f x = pure f <*> x


instance MonadTrans QuotaStreamReader where
    lift = QuotaStreamReader


instance (Monad m, MonadQuota m) => MonadQuota (QuotaStreamReader m) where
    invoice = lift . invoice
    recurse = lift . recurse . runQuotaStreamReader


instance (StreamReader m, MonadQuota m) => StreamReader (QuotaStreamReader m) where
    getWord32le     = invoice 4 >> (QuotaStreamReader getWord32le)
    getByteString n = invoice n >> (QuotaStreamReader (getByteString n))

-- | Read in a Cap'n Proto message.
--
-- The size of the message will be invoiced to the 'MonadQuota' instance,
-- so unmarshalling may stop if if the quota is exceeded.
getMessage :: (StreamReader m, MonadThrow m) => m Message
getMessage = do
    numSegments <- getWord32le >>= fromWord
    segmentSizes <- BV.replicateM numSegments $ do w <- getWord32le
                                                   fromWord (8 * w)
    -- pad up to the next word boundary:
    when (numSegments `mod` 2 == 0) $ void getWord32le
    BV.mapM getByteString segmentSizes
  where
    -- Make sure that this doesn't become negative when cast to a signed
    -- integer.
    fromWord :: MonadThrow m => Word32 -> m Int
    fromWord n
        | (fromIntegral n :: Int) < 0 =
            throwM $ BoundsError (fromIntegral n) (maxBound :: Int)
        | otherwise = return $ fromIntegral n

-- | Write out a Cap'n Proto message.
putMessage :: (Monad m, StreamWriter m) => Message -> m ()
putMessage msg = do
    putWord32le (fromIntegral $ BV.length msg)
    BV.mapM_ (putWord32le . fromIntegral . (`div` 8) . B.length) msg
    when (BV.length msg `mod` 2 == 0) (putWord32le 0)
    BV.mapM_ putByteString msg


listMessage :: (Monad m, MonadQuota m, MonadThrow m)
    => Message -> List m (List m Word64)
listMessage msg = List
    { length = return $ BV.length msg
    , lookup = \i -> do checkBounds i (BV.length msg)
                        listSegment <$> BV.indexM msg i
    }

listSegment :: (Monad m, MonadQuota m, MonadThrow m)
    => B.ByteString -> List m Word64
listSegment bs = List
    { length = return $ B.length bs `div` 8
    , lookup = \i -> do invoice 8
                        checkBounds i (B.length bs `div` 8)
                        let i' = i * 8
                        let indicies = fromIntegral <$> (+i') <$> take 8 [0,1..]
                        let [b0,b1,b2,b3,b4,b5,b6,b7] = fromIntegral <$> B.index bs <$> indicies
                        return $
                            (b0 `shiftL`  0) .|.
                            (b1 `shiftL`  8) .|.
                            (b2 `shiftL` 16) .|.
                            (b3 `shiftL` 24) .|.
                            (b4 `shiftL` 32) .|.
                            (b5 `shiftL` 40) .|.
                            (b6 `shiftL` 48) .|.
                            (b7 `shiftL` 56)
    }
-}
