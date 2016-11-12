{-|
Module : Data.CapNProto.Stream
Description : Serialize CapNProto messages over streams.

This module implements the encoding described at:

https://capnproto.org/encoding.html#serialization-over-a-stream

-}
module Data.CapNProto.Stream
    ( getMessage
    , putMessage
    ) where

import Data.CapNProto.Untyped (Segment, Message)
import Data.Bytes.Get (MonadGet, getWord32le, getWord64le)
import Data.Bytes.Put (MonadPut, putWord32le, putWord64le)
import Data.Int (Int64)
import Control.Monad (when, void)
import Control.Monad.Catch (MonadThrow, throwM, Exception)
import Control.Monad.Quota (MonadQuota(invoice))
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as BV

data BoundsError = BoundsError deriving(Show)
instance Exception BoundsError

-- | Read in a Cap'n Proto message.
--
-- The size of the message will be invoiced to the 'MonadQuota' instance,
-- so unmarshalling may stop if if the cursor's quota is exceeded.
getMessage :: (MonadGet m, MonadThrow m, MonadQuota Int64 m) => m Message
getMessage = do
    numSegments <- get32 >>= fromWord
    segmentSizes <- BV.replicateM numSegments $ get32 >>= fromWord
    let _ = segmentSizes :: BV.Vector Int
    -- pad up to the next word boundary:
    when ((1 + BV.length segmentSizes) `mod` 2 /= 0) $ void get32
    BV.mapM getSegment segmentSizes
  where
    fromWord n = do
        let n' = fromIntegral n
        when (n < 0) $ throwM BoundsError
        return n'
    getLimited size getter = invoice size >> getter
    get32 = getLimited 4 getWord32le
    get64 = getLimited 8 getWord64le
    getSegment size = UV.replicateM size $ get64 >>= fromWord

-- | Write out a Cap'n Proto message.
putMessage :: (MonadPut m) => Message -> m ()
putMessage msg = do
    putWord32le (fromIntegral $ BV.length msg)
    BV.mapM_ (putWord32le . fromIntegral . UV.length) msg
    when ((1 + BV.length msg) `mod` 2 /= 0) (putWord32le 0)
    BV.mapM_ putSegment msg
  where
    putSegment seg = UV.mapM_ putWord64le seg
