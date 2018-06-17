{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-|
Module: Data.Capnp.Message
Description: Read-only capnpoto messages.

-}
module Data.Capnp.Message
    ( Message(..)
    , Segment(..)
    , decode
    , readMessage
    , writeMessage
    )
  where

import Control.Monad             (void, when)
import Control.Monad.Catch       (MonadThrow(..))
import Control.Monad.State       (evalStateT, get, put)
import Data.ByteString.Internal  (ByteString(..))
import Data.Capnp.Bits           (WordCount(..), hi, lo)
import Data.Capnp.Internal.Util  (checkIndex)
import Data.Capnp.TraversalLimit (MonadLimit(invoice), evalLimitT)
import Data.Word                 (Word32, Word64)
import System.Endian             (fromLE64)

import qualified Data.Capnp.Message.Generic as GM
import qualified Data.Vector                as V
import qualified Data.Vector.Storable       as SV

-- | A read-only capnproto message.
newtype Message = Message (V.Vector Segment)

-- | A read-only segment in a 'Message'.
--
-- 'Segment' is an instance of the generic 'GM.Segment' type class. its
-- implementations of 'GM.toByteString' and 'GM.fromByteString' are O(1);
-- the underlying bytes are not copied.
newtype Segment = Segment (SV.Vector Word64)

instance MonadThrow m => GM.Segment m Segment where
    numWords (Segment vec) = pure $ SV.length vec
    slice start len (Segment vec) = pure $ Segment (SV.slice start len vec)
    read (Segment vec) i = fromLE64 <$> vec `SV.indexM` i

    -- FIXME: Verify that the pointer is actually 64-bit aligned before casting.
    fromByteString (PS fptr offset len) =
        pure $ Segment (SV.unsafeCast $ SV.unsafeFromForeignPtr fptr offset len)
    toByteString (Segment vec) = pure $ PS fptr offset len where
        (fptr, offset, len) = SV.unsafeToForeignPtr (SV.unsafeCast vec)

instance MonadThrow m => GM.Message m Message Segment where
    numSegs (Message vec) = pure $ V.length vec
    internalGetSeg (Message vec) i = do
        checkIndex i (V.length vec)
        vec `V.indexM` i

-- | 'decode' decodes a message from a bytestring.
--
-- The segments will not be copied; the resulting message will be a view into
-- the original bytestring. Runs in O(number of segments in the message).
decode :: MonadThrow m => ByteString -> m Message
decode bytes = GM.fromByteString bytes >>= decodeSeg

-- | 'decodeSeg' decodes a message from a segment, treating the segment as if
-- it were raw bytes.
--
-- this is mostly here as a helper for 'decode'.
decodeSeg :: MonadThrow m => Segment -> m Message
decodeSeg seg = do
    len <- GM.numWords seg
    flip evalStateT (Nothing, 0) $ evalLimitT len $
        -- Note: we use the quota to avoid needing to do bounds checking here;
        -- since readMessage invoices the quota before reading, we can rely on it
        -- not to read past the end of the blob.
        readMessage read32 readSegment
  where
    read32 = do
        (cur, idx) <- get
        case cur of
            Just n -> do
                put (Nothing, idx)
                return n
            Nothing -> do
                word <- GM.read seg idx
                put (Just $ hi word, idx + 1)
                return (lo word)
    readSegment (WordCount len) = do
        (cur, idx) <- get
        put (cur, idx + len)
        GM.slice idx len seg

-- | @readMessage read32 readSegment@ reads in a message using the
-- monadic context, which should manage the current read position,
-- into a message. read32 should read a 32-bit little-endian integer,
-- and @readSegment n@ should read a blob of @n@ 64-bit words.
-- The size of the message (in 64-bit words) is deducted from the quota,
-- which can be used to set the maximum message size.
readMessage :: (MonadLimit m) => m Word32 -> (WordCount -> m Segment) -> m Message
readMessage read32 readSegment = do
    invoice 1
    numSegs' <- read32
    let numSegs = numSegs' + 1
    invoice (fromIntegral numSegs `div` 2)
    segSizes <- V.replicateM (fromIntegral numSegs) read32
    when (numSegs `mod` 2 == 0) $ void read32
    V.mapM_ (invoice . fromIntegral) segSizes
    Message <$> V.mapM (readSegment . fromIntegral) segSizes

-- | @writeMesage write32 writeSegment@ writes out the message. @write32@
-- should write a 32-bit word in little-endian format to the output stream.
-- @writeSegment@ should write a blob.
writeMessage :: MonadThrow m => Message -> (Word32 -> m ()) -> (Segment -> m ()) -> m ()
writeMessage (Message segs) write32 writeSegment = do
    let numSegs = V.length segs
    write32 (fromIntegral numSegs - 1)
    V.forM_ segs $ \seg -> write32 =<< fromIntegral <$> GM.numWords seg
    when (numSegs `mod` 2 == 0) $ write32 0
    V.forM_ segs writeSegment
