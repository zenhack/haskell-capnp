{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}
{-|
Module: Data.Capnp.Message
Description: Cap'N Proto messages

-}
module Data.Capnp.Message
    ( Message(..)
    , ConstMsg(..)
    , MutMsg(..)
    , WriteCtx(..)
    , Mutable(..)
    , getSegment
    , getWord
    , setSegment
    , setWord
    , encode
    , decode
    , readMessage
    , writeMessage
    )
  where

import Prelude hiding (read)

import Control.Monad             (void, when, (>=>))
import Control.Monad.Catch       (MonadThrow(..))
import Control.Monad.Primitive   (PrimMonad, PrimState)
import Control.Monad.State       (evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer      (execWriterT, tell)
import Data.ByteString.Internal  (ByteString(..))
import Data.Capnp.Address        (WordAddr(..))
import Data.Capnp.Bits           (WordCount(..), hi, lo)
import Data.Capnp.TraversalLimit (MonadLimit(invoice), evalLimitT)
import Data.Word                 (Word32, Word64)
import Internal.Util             (checkIndex)
import System.Endian             (fromLE64, toLE64)

import qualified Data.ByteString.Builder      as BB
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as MV
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as SMV

-- | A 'Message' is a (possibly read-only) capnproto message. It is
-- parameterized over a monad in which operations are performed.
class Monad m => Message m msg where
    -- | The type of segments in the message.
    data Segment m msg

    -- | 'numSegs' gets the number of segments in a message.
    numSegs :: msg -> m Int
    -- | @'internalGetSeg' message index@ gets the segment at index 'index'
    -- in 'message'. Most callers should use the 'getSegment' wrapper, instead
    -- of calling this directly.
    internalGetSeg :: msg -> Int -> m (Segment m msg)
    -- | Get the length of the segment, in units of 64-bit words.
    numWords :: Segment m msg -> m Int
    -- | @'slice' start length segment@ extracts a sub-section of the segment,
    -- starting at index @start@, of length @length@.
    slice   :: Int -> Int -> Segment m msg -> m (Segment m msg)
    -- | @'read' segment index@ reads a 64-bit word from the segement at the
    -- given index. Consider using 'getWord' on the message, instead of
    -- calling this directly.
    read    :: Segment m msg -> Int -> m Word64
    -- | Convert a ByteString to a segment.
    fromByteString :: ByteString -> m (Segment m msg)
    -- | Convert a segment to a byte string.
    toByteString :: Segment m msg -> m ByteString

-- | The 'Mutable' type class relates mutable and immutable versions of a type.
class PrimMonad m => Mutable m mut const where
    -- | Convert an immutable value to a mutable one.
    thaw :: const -> m mut
    -- | Convert a mutable value to an immutable one.
    freeze :: mut -> m const

-- | @'getSegment' message index@ fetches the given segment in the message.
-- It throws a @BoundsError@ if the address is out of bounds.
getSegment :: (MonadThrow m, Message m msg) => msg -> Int -> m (Segment m msg)
getSegment msg i = do
    checkIndex i =<< numSegs msg
    internalGetSeg msg i

-- | @'getWord' msg addr@ returns the word at @addr@ within @msg@. It throws a
-- @BoundsError@ if the address is out of bounds.
getWord :: (MonadThrow m, Message m msg) => msg -> WordAddr -> m Word64
getWord msg WordAt{wordIndex=wordIndex@(WordCount i), segIndex} = do
    seg <- getSegment msg segIndex
    checkIndex i =<< numWords seg
    seg `read` i

-- | @'setSegment' message index segment@ sets the segment at the given index
-- in the message. It throws a @BoundsError@ if the address is out of bounds.
setSegment :: (WriteCtx m s, MonadThrow m) => MutMsg s -> Int -> Segment m (MutMsg s) -> m ()
setSegment msg i seg = do
    checkIndex i =<< numSegs msg
    internalSetSeg msg i seg

-- | @'setWord' message address value@ sets the word at @address@ in the
-- message to @value@. If the address is not valid in the message, a
-- @BoundsError@ will be thrown.
setWord :: (WriteCtx m s, MonadThrow m) => MutMsg s -> WordAddr -> Word64 -> m ()
setWord msg WordAt{wordIndex=WordCount i, segIndex} val = do
    seg <- getSegment msg segIndex
    checkIndex i =<< numWords seg
    write seg i val

-- | A read-only capnproto message.
--
-- 'ConstMsg' is an instance of the generic 'Message' type class. its
-- implementations of 'toByteString' and 'fromByteString' are O(1);
-- the underlying bytes are not copied.
newtype ConstMsg = ConstMsg (V.Vector (SV.Vector Word64))

instance MonadThrow m => Message m ConstMsg where
    newtype Segment m ConstMsg = ConstSegment { constSegToVec :: SV.Vector Word64 }

    numSegs (ConstMsg vec) = pure $ V.length vec
    internalGetSeg (ConstMsg vec) i = do
        checkIndex i (V.length vec)
        ConstSegment <$> vec `V.indexM` i

    numWords (ConstSegment vec) = pure $ SV.length vec
    slice start len (ConstSegment vec) = pure $ ConstSegment (SV.slice start len vec)
    read (ConstSegment vec) i = fromLE64 <$> vec `SV.indexM` i

    -- FIXME: Verify that the pointer is actually 64-bit aligned before casting.
    fromByteString (PS fptr offset len) =
        pure $ ConstSegment (SV.unsafeCast $ SV.unsafeFromForeignPtr fptr offset len)
    toByteString (ConstSegment vec) = pure $ PS fptr offset len where
        (fptr, offset, len) = SV.unsafeToForeignPtr (SV.unsafeCast vec)

-- | 'decode' decodes a message from a bytestring.
--
-- The segments will not be copied; the resulting message will be a view into
-- the original bytestring. Runs in O(number of segments in the message).
decode :: MonadThrow m => ByteString -> m ConstMsg
decode bytes = fromByteString bytes >>= decodeSeg

encode :: MonadThrow m => ConstMsg -> m BB.Builder
encode msg = execWriterT $ writeMessage
    msg
    (tell . BB.word32LE)
    (toByteString >=> tell . BB.byteString)

-- | 'decodeSeg' decodes a message from a segment, treating the segment as if
-- it were raw bytes.
--
-- this is mostly here as a helper for 'decode'.
decodeSeg :: MonadThrow m => Segment m ConstMsg -> m ConstMsg
decodeSeg seg = do
    len <- numWords seg
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
                word <- lift $ lift $ read seg idx
                put (Just $ hi word, idx + 1)
                return (lo word)
    readSegment (WordCount len) = do
        (cur, idx) <- get
        put (cur, idx + len)
        lift $ lift $ slice idx len seg

-- | @readMessage read32 readSegment@ reads in a message using the
-- monadic context, which should manage the current read position,
-- into a message. read32 should read a 32-bit little-endian integer,
-- and @readSegment n@ should read a blob of @n@ 64-bit words.
-- The size of the message (in 64-bit words) is deducted from the quota,
-- which can be used to set the maximum message size.
readMessage read32 readSegment = do
    invoice 1
    numSegs' <- read32
    let numSegs = numSegs' + 1
    invoice (fromIntegral numSegs `div` 2)
    segSizes <- V.replicateM (fromIntegral numSegs) read32
    when (numSegs `mod` 2 == 0) $ void read32
    V.mapM_ (invoice . fromIntegral) segSizes
    ConstMsg <$> V.mapM (fmap constSegToVec . readSegment . fromIntegral) segSizes

-- | @writeMesage write32 writeSegment@ writes out the message. @write32@
-- should write a 32-bit word in little-endian format to the output stream.
-- @writeSegment@ should write a blob.
writeMessage :: MonadThrow m => ConstMsg -> (Word32 -> m ()) -> (Segment m ConstMsg -> m ()) -> m ()
writeMessage (ConstMsg segs) write32 writeSegment = do
    let numSegs = V.length segs
    write32 (fromIntegral numSegs - 1)
    V.forM_ segs $ \seg -> write32 =<< fromIntegral <$> numWords (ConstSegment seg)
    when (numSegs `mod` 2 == 0) $ write32 0
    V.forM_ segs (writeSegment . ConstSegment)

-- | A 'MutMsg' is a mutable capnproto message. The type parameter 's' is the
-- state token for the instance of 'PrimMonad' in which the message may be
-- modified.
--
-- Due to mutabilty, the implementations of 'toByteString' and 'fromByteString'
-- must make full copies, and so are O(n) in the length of the segment.
newtype MutMsg s = MutMsg (MV.MVector s (SMV.MVector s Word64))

-- | 'WriteCtx' is the context needed for most write operations.
type WriteCtx m s = (PrimMonad m, s ~ PrimState m, MonadThrow m)

instance WriteCtx m s => Message m (MutMsg s) where
    newtype Segment m (MutMsg s) = MutSegment { mutSegToVec :: SMV.MVector s Word64 }

    numWords (MutSegment vec) = pure $ SMV.length vec
    slice start len (MutSegment vec) = pure $ MutSegment (SMV.slice start len vec)
    read (MutSegment vec) i = fromLE64 <$> SMV.read vec i
    fromByteString bytes = do
        vec <- constSegToVec <$> fromByteString bytes
        MutSegment <$> SV.thaw vec
    toByteString (MutSegment vec) = do
        seg <- ConstSegment <$> SV.freeze vec
        toByteString seg

    numSegs (MutMsg vec) = pure $ MV.length vec
    internalGetSeg (MutMsg vec) i = MutSegment <$> MV.read vec i


-- | @'internalSetSeg' message index segment@ sets the segment at the given
-- index in the message. Most callers should use the 'setSegment' wrapper,
-- instead of calling this directly.
internalSetSeg :: WriteCtx m s => MutMsg s -> Int -> Segment m (MutMsg s) -> m ()
internalSetSeg (MutMsg msg) i (MutSegment seg) = MV.write msg i seg

-- | @'write' segment index value@ writes a value to the 64-bit word
-- at the provided index. Consider using 'setWord' on the message,
-- instead of calling this directly.
write :: WriteCtx m s => Segment m (MutMsg s) -> Int -> Word64 -> m ()
write (MutSegment vec) i val = SMV.write vec i (toLE64 val)

-- | @'grow' segment amount@ grows the segment by the specified number
-- of 64-bit words. The original segment should not be used afterwards.
grow  :: WriteCtx m s => Segment m (MutMsg s) -> Int -> m (Segment m (MutMsg s))
grow (MutSegment vec) amount = MutSegment <$> SMV.grow vec amount


instance WriteCtx m s => Mutable m (MutMsg s) ConstMsg where
    thaw (ConstMsg vec) =
        MutMsg <$> (V.mapM SV.thaw vec >>= V.thaw)
    freeze (MutMsg mvec) = do
        vec <- V.freeze mvec
        ConstMsg <$> V.mapM SV.freeze vec
