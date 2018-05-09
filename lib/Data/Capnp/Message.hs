{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-|
Module: Data.Capnp.Message
Description: Tools for working with messages.

-}
module Data.Capnp.Message
    ( Message
    , getSegment
    , getWord
    , decode
    , readMessage
    , writeMessage
    )
  where

import Control.Monad                 (void, when)
import Control.Monad.State           (evalStateT, get, put)
import Control.Monad.Trans.Class     (MonadTrans(..))
import Data.Capnp.Address        (WordAddr(..))
import Data.Capnp.Bits           (WordCount(..), hi, lo, wordsToBytes)
import Data.Capnp.Errors         (Error(..), ThrowError(..))
import Data.Capnp.TraversalLimit (Limit(invoice), evalWithLimit)
import Data.Word                     (Word32, Word64)

import qualified Data.Capnp.Blob as B
import qualified Data.Vector         as V

newtype Message a = Message (V.Vector a) deriving(Show)

-- | @getSegment msg i@ gets the ith segment of a message. Throws a
-- 'BoundsError' if @i@ is out of bounds.
getSegment :: (Monad m, ThrowError m) => Message a -> Int -> m a
getSegment (Message segs) i = do
    when (i < 0 || i >= V.length segs) $
        throwError BoundsError { index = i, maxIndex = V.length segs }
    segs `V.indexM` i

-- | @getWord msg addr@ returns the word at @addr@ within @msg@. It throws a
-- @BoundsError@ if the address is out of bounds.
getWord :: (B.Blob m seg, Monad m, ThrowError m)
    => Message seg -> WordAddr -> m Word64
getWord (Message segs) WordAt{..} = do
    seg <- segs `V.indexM` segIndex
    seg `B.indexWord` wordIndex

-- | @decode blob@ decodes a message from the blob.
--
-- The segments will not be copied; the resulting message will be a view into
-- the original blob.
decode :: (B.Slice m b, B.Blob m b, Monad m, ThrowError m) => b -> m (Message b)
decode blob = do
    -- Note: we use the quota to avoid needing to do bounds checking here;
    -- since readMessage invoices the quota before reading, we can rely on it
    -- not to read past the end of the blob.
    WordCount blobLen <- B.lengthInWords blob
    flip evalStateT (Nothing, 0) $ evalWithLimit blobLen $
        readMessage read32 readSegment
  where
    bIndex b i = lift $ lift $ B.indexWord b i
    read32 = do
        (cur, idx) <- get
        case cur of
            Just n -> do
                put (Nothing, idx)
                return n
            Nothing -> do
                word <- bIndex blob idx
                put (Just $ hi word, idx + 1)
                return (lo word)
    readSegment len = do
        (cur, idx) <- get
        put (cur, idx + len)
        lift $ lift $ B.slice blob (wordsToBytes idx) (wordsToBytes len)

-- | @readMessage read32 readSegment@ reads in a message using the
-- monadic context, which should manage the current read position,
-- into a message. read32 should read a 32-bit little-endian integer,
-- and @readSegment n@ should read a blob of @n@ 64-bit words.
-- The size of the message (in 64-bit words) is deducted from the quota,
-- which can be used to set the maximum message size.
readMessage :: (Monad m, ThrowError m, Limit m)
    => m Word32 -> (WordCount -> m b) -> m (Message b)
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
writeMessage :: (Monad m, B.Blob m b)
    => Message b -> (Word32 -> m ()) -> (b -> m ()) -> m ()
writeMessage (Message msg) write32 writeSegment = do
    let numSegs = V.length msg
    write32 (fromIntegral numSegs - 1)
    V.forM_ msg $ \seg -> write32 =<< fromIntegral <$> B.length seg
    when (numSegs `mod` 2 == 0) $ write32 0
    V.forM_ msg writeSegment
