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
    , alloc
    , allocInSeg
    , newMessage
    , newSegment
    , empty
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
import Data.Primitive            (MutVar, newMutVar, readMutVar, writeMutVar)
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
    data Segment msg

    -- | 'numSegs' gets the number of segments in a message.
    numSegs :: msg -> m Int
    -- | @'internalGetSeg' message index@ gets the segment at index 'index'
    -- in 'message'. Most callers should use the 'getSegment' wrapper, instead
    -- of calling this directly.
    internalGetSeg :: msg -> Int -> m (Segment msg)
    -- | Get the length of the segment, in units of 64-bit words.
    numWords :: Segment msg -> m Int
    -- | @'slice' start length segment@ extracts a sub-section of the segment,
    -- starting at index @start@, of length @length@.
    slice   :: Int -> Int -> Segment msg -> m (Segment msg)
    -- | @'read' segment index@ reads a 64-bit word from the segement at the
    -- given index. Consider using 'getWord' on the message, instead of
    -- calling this directly.
    read    :: Segment msg -> Int -> m Word64
    -- | Convert a ByteString to a segment.
    fromByteString :: ByteString -> m (Segment msg)
    -- | Convert a segment to a byte string.
    toByteString :: Segment msg -> m ByteString

-- | The 'Mutable' type class relates mutable and immutable versions of a type.
class Mutable a where
    type Scope a
    type Frozen a

    -- | Convert an immutable value to a mutable one.
    thaw :: (MonadThrow m, PrimMonad m, PrimState m ~ Scope a) => Frozen a -> m a

    -- | Convert a mutable value to an immutable one.
    freeze :: (MonadThrow m, PrimMonad m, PrimState m ~ Scope a) => a -> m (Frozen a)

-- | @'getSegment' message index@ fetches the given segment in the message.
-- It throws a @BoundsError@ if the address is out of bounds.
getSegment :: (MonadThrow m, Message m msg) => msg -> Int -> m (Segment msg)
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
setSegment :: (WriteCtx m s, MonadThrow m) => MutMsg s -> Int -> Segment (MutMsg s) -> m ()
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
newtype ConstMsg = ConstMsg (V.Vector (Segment ConstMsg))

instance MonadThrow m => Message m ConstMsg where
    newtype Segment ConstMsg = ConstSegment { constSegToVec :: SV.Vector Word64 }

    numSegs (ConstMsg vec) = pure $ V.length vec
    internalGetSeg (ConstMsg vec) i = do
        checkIndex i (V.length vec)
        vec `V.indexM` i

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
decodeSeg :: MonadThrow m => Segment ConstMsg -> m ConstMsg
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
    ConstMsg <$> V.mapM (readSegment . fromIntegral) segSizes

-- | @writeMesage write32 writeSegment@ writes out the message. @write32@
-- should write a 32-bit word in little-endian format to the output stream.
-- @writeSegment@ should write a blob.
writeMessage :: MonadThrow m => ConstMsg -> (Word32 -> m ()) -> (Segment ConstMsg -> m ()) -> m ()
writeMessage (ConstMsg segs) write32 writeSegment = do
    let numSegs = V.length segs
    write32 (fromIntegral numSegs - 1)
    V.forM_ segs $ \seg -> write32 =<< fromIntegral <$> numWords seg
    when (numSegs `mod` 2 == 0) $ write32 0
    V.forM_ segs writeSegment

-- | A 'MutMsg' is a mutable capnproto message. The type parameter 's' is the
-- state token for the instance of 'PrimMonad' in which the message may be
-- modified.
--
-- Due to mutabilty, the implementations of 'toByteString' and 'fromByteString'
-- must make full copies, and so are O(n) in the length of the segment.
data MutMsg s = MutMsg
    { mutMsgSegs :: MutVar s (MV.MVector s (Segment (MutMsg s)))
    -- ^ A vector of segments. A suffix of this may be unused; see below.
    , mutMsgLen  :: MutVar s Int
    -- ^ The "true" number of segments in the message. This may be shorter
    -- than @'MV.length' mutMsgSegs@; the remainder is considered
    -- unallocated space, and is used for amortized O(1) appending.
    }

-- | 'WriteCtx' is the context needed for most write operations.
type WriteCtx m s = (PrimMonad m, s ~ PrimState m, MonadThrow m)

instance WriteCtx m s => Message m (MutMsg s) where
    data Segment (MutMsg s) = MutSegment
        { mutSegVec :: !(SMV.MVector s Word64)
        -- ^ The underlying vector of words storing segment's data.
        , mutSegLen :: !Int
        -- ^ The "true" length fo the segment. This may be shorter
        -- than @'SMV.length' mutSegVec@; it is analogous to 'mutMsgLen'
        -- at the message level.
        }

    numWords MutSegment{mutSegLen} = pure mutSegLen
    slice start len MutSegment{mutSegVec,mutSegLen} =
        pure MutSegment
            { mutSegVec = SMV.slice start len mutSegVec
            , mutSegLen = len
            }
    read MutSegment{mutSegVec} i = fromLE64 <$> SMV.read mutSegVec i
    fromByteString bytes = do
        vec <- constSegToVec <$> fromByteString bytes
        mvec <- SV.thaw vec
        pure MutSegment
            { mutSegVec = mvec
            , mutSegLen = SV.length vec
            }
    toByteString mseg = do
        seg <- freeze mseg
        toByteString (seg :: Segment ConstMsg)

    numSegs = readMutVar . mutMsgLen
    internalGetSeg MutMsg{mutMsgSegs} i = do
        segs <- readMutVar mutMsgSegs
        MV.read segs i


-- | @'internalSetSeg' message index segment@ sets the segment at the given
-- index in the message. Most callers should use the 'setSegment' wrapper,
-- instead of calling this directly.
internalSetSeg :: WriteCtx m s => MutMsg s -> Int -> Segment (MutMsg s) -> m ()
internalSetSeg MutMsg{mutMsgSegs} segIndex seg = do
    segs <- readMutVar mutMsgSegs
    MV.write segs segIndex seg

-- | @'write' segment index value@ writes a value to the 64-bit word
-- at the provided index. Consider using 'setWord' on the message,
-- instead of calling this directly.
write :: WriteCtx m s => Segment (MutMsg s) -> Int -> Word64 -> m ()
write MutSegment{mutSegVec} i val =
    SMV.write mutSegVec i (toLE64 val)

-- | @'grow' segment amount@ grows the segment by the specified number
-- of 64-bit words. The original segment should not be used afterwards.
grow  :: WriteCtx m s => Segment (MutMsg s) -> Int -> m (Segment (MutMsg s))
grow MutSegment{mutSegVec} amount = do
    -- TODO: use unallocated space if available, instead of actually resizing.
    newVec <- SMV.grow mutSegVec amount
    pure MutSegment
        { mutSegVec = newVec
        , mutSegLen = SMV.length newVec
        }

-- | @'newSegment' msg sizeHint@ allocates a new, initially empty segment in
-- @msg@ with a capacity of @sizeHint@. It returns the a pair of the segment
-- number and the segment itself. Amortized O(1).
newSegment :: WriteCtx m s => MutMsg s -> Int -> m (Int, Segment (MutMsg s))
newSegment msg@MutMsg{mutMsgSegs,mutMsgLen} sizeHint = do
    newSegVec <- SMV.new sizeHint
    segIndex <- numSegs msg
    segs <- readMutVar mutMsgSegs
    when (MV.length segs == segIndex) $ do
        -- out of space; double the length of the message.
        MV.grow segs segIndex >>= writeMutVar mutMsgSegs
        writeMutVar mutMsgLen (segIndex * 2)
    let newSeg = MutSegment
            { mutSegVec = newSegVec
            , mutSegLen = 0
            }
    setSegment msg segIndex newSeg
    pure (segIndex, newSeg)

allocInSeg :: WriteCtx m s => MutMsg s -> Int -> WordCount -> m WordAddr
allocInSeg msg segIndex (WordCount size) = do
    oldSeg@MutSegment{mutSegLen} <- getSegment msg segIndex
    let ret = WordAt { segIndex, wordIndex = WordCount mutSegLen }
    newSeg <- grow oldSeg size
    setSegment msg segIndex newSeg
    pure ret

-- | @'alloc' size@ allocates 'size' words within a message. it returns the
-- starting address of the allocated memory.
alloc :: WriteCtx m s => MutMsg s -> WordCount -> m WordAddr
alloc msg size = do
    -- TODO: check for and deal with segments that are "too big."
    segIndex <- pred <$> numSegs msg
    allocInSeg msg segIndex size

empty :: ConstMsg
empty = ConstMsg $ V.fromList [ ConstSegment $ SV.fromList [0] ]

newMessage :: WriteCtx m s => m (MutMsg s)
newMessage = thaw empty

instance Mutable (Segment (MutMsg s)) where
    type Scope (Segment (MutMsg s)) = s
    type Frozen (Segment (MutMsg s)) = Segment ConstMsg

    thaw (ConstSegment vec) = do
        mvec <- SV.thaw vec
        pure MutSegment
            { mutSegVec = mvec
            , mutSegLen = SV.length vec
            }
    freeze seg@MutSegment{mutSegLen} = do
        -- Slice before freezing, so we don't waste time copying
        -- the unallocated portion:
        MutSegment{mutSegVec} <- slice 0 mutSegLen seg
        ConstSegment <$> SV.freeze mutSegVec


instance Mutable (MutMsg s) where
    type Scope (MutMsg s) = s
    type Frozen (MutMsg s) = ConstMsg

    thaw (ConstMsg vec) = do
        segments <- V.mapM thaw vec >>= V.thaw
        MutMsg
            <$> newMutVar segments
            <*> newMutVar (MV.length segments)
    freeze msg@MutMsg{mutMsgLen} = do
        len <- readMutVar mutMsgLen
        ConstMsg <$> V.generateM len (getSegment msg >=> freeze)
