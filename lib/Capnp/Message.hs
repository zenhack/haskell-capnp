{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}
{-|
Module: Capnp.Message
Description: Cap'N Proto messages

This module provides support for working directly with Cap'N Proto messages.
-}
module Capnp.Message (
      Message
    , Segment
    , Mutability(..)

    -- * Reading and writing messages
    , hPutMsg
    , hGetMsg
    , putMsg
    , getMsg

    , readMessage
    , writeMessage

    -- * Limits on message size
    , maxSegmentSize
    , maxSegments
    , maxCaps

    -- * Converting between messages and 'ByteString's
    , encode
    , decode
    , toByteString
    , fromByteString

    -- * Immutable messages
    , empty
    , singleSegment

    -- * Reading data from messages
    , MonadReadMessage(..)
    , getSegment
    , getCap
    , getCapTable

    -- * Mutable Messages
    , newMessage

    -- ** Allocating space in messages
    , WordPtr(..)
    , alloc
    , allocInSeg
    , newSegment

    -- ** Modifying messages
    , setSegment
    , write
    , setCap
    , appendCap

    , WriteCtx

    , Client
    , nullClient
    , withCapTable
    ) where

import {-# SOURCE #-} Capnp.Rpc.Untyped (Client, nullClient)

import Prelude hiding (read)

import Data.Bits (shiftL)

import Control.Monad             (void, when, (>=>))
import Control.Monad.Catch       (MonadThrow(..))
import Control.Monad.Primitive   (PrimMonad, PrimState, stToPrim)
import Control.Monad.State       (evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer      (execWriterT, tell)
import Data.ByteString.Internal  (ByteString(..))
import Data.Bytes.Get            (getWord32le, runGetS)
import Data.Maybe                (fromJust)
import Data.Primitive            (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Word                 (Word32, Word64, byteSwap64)
import GHC.ByteOrder             (ByteOrder(..), targetByteOrder)
import System.IO                 (Handle, stdin, stdout)

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Builder      as BB
import qualified Data.Vector                  as V
import qualified Data.Vector.Generic.Mutable  as GMV
import qualified Data.Vector.Mutable          as MV
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as SMV

import Capnp.Address        (WordAddr(..))
import Capnp.Bits           (WordCount(..), hi, lo)
import Capnp.Mutability     (MaybeMutable(..), Mutability(..))
import Capnp.TraversalLimit (LimitT, MonadLimit(invoice), evalLimitT)
import Internal.AppendVec   (AppendVec)

import qualified Capnp.Errors       as E
import qualified Internal.AppendVec as AppendVec

swapIfBE64, fromLE64, toLE64 :: Word64 -> Word64
swapIfBE64 = case targetByteOrder of
    LittleEndian -> id
    BigEndian    -> byteSwap64
fromLE64 = swapIfBE64
toLE64 = swapIfBE64


-- | The maximum size of a segment supported by this libarary, in words.
maxSegmentSize :: WordCount
maxSegmentSize = WordCount $ 1 `shiftL` 28 -- 2 GiB.

-- | The maximum number of segments allowed in a message by this library.
maxSegments :: Int
maxSegments = 1024

-- | The maximum number of capabilities allowed in a message by this library.
maxCaps :: Int
maxCaps = 16 * 1024

-- | A pointer to a location in a message. This encodes the same
-- information as a 'WordAddr', but also includes direct references
-- to the segment and message, which can improve performance in very
-- low-level code.
data WordPtr mut = WordPtr
    -- invariants:
    --
    -- - pAddr's segment index refers to pSegment.
    -- - pSegment is in pMessage.
    { pMessage :: !(Message mut)
    , pSegment :: !(Segment mut)
    , pAddr    :: {-# UNPACK #-} !WordAddr
    }

-- | A Cap'n Proto message, parametrized over its mutability.
data family Message (mut :: Mutability)

newtype instance Message 'Const = MsgConst ConstMsg
    deriving(Eq)
newtype instance Message ('Mut s) = MsgMut (MutMsg s)
    deriving(Eq)

-- | A segment in a Cap'n Proto message.
data family Segment (mut :: Mutability)

newtype instance Segment 'Const = SegConst ConstSegment
    deriving(Eq)
newtype instance Segment ('Mut s) = SegMut (MutSegment s)
    deriving(Eq)

data MutSegment s = MutSegment
    { vec  :: SMV.MVector s Word64
    , used :: MutVar s WordCount
    }

instance Eq (MutSegment s) where
    MutSegment{used=x} == MutSegment{used=y} = x == y

newtype ConstSegment = ConstSegment (SV.Vector Word64)
    deriving(Eq)

-- | A 'Message' is a (possibly read-only) capnproto message. It is
-- parameterized over a monad in which operations are performed.
class Monad m => MonadReadMessage mut m where
    -- | 'numSegs' gets the number of segments in a message.
    numSegs :: Message mut -> m Int
    -- | 'numWords' gets the number of words in a segment.
    numWords :: Segment mut -> m WordCount
    -- | 'numCaps' gets the number of capabilities in a message's capability
    -- table.
    numCaps :: Message mut -> m Int
    -- | @'internalGetSeg' message index@ gets the segment at index 'index'
    -- in 'message'. Most callers should use the 'getSegment' wrapper, instead
    -- of calling this directly.
    internalGetSeg :: Message mut -> Int -> m (Segment mut)
    -- | @'internalGetCap' cap index@ reads a capability from the message's
    -- capability table, returning the client. does not check bounds. Callers
    -- should use getCap instead.
    internalGetCap :: Message mut -> Int -> m Client
    -- | @'slice' start length segment@ extracts a sub-section of the segment,
    -- starting at index @start@, of length @length@.
    slice   :: WordCount -> WordCount -> Segment mut -> m (Segment mut)
    -- | @'read' segment index@ reads a 64-bit word from the segement at the
    -- given index. Consider using 'getWord' on the message, instead of
    -- calling this directly.
    read    :: Segment mut -> WordCount -> m Word64

-- | Convert a ByteString to a segment. O(1)
fromByteString :: ByteString -> Segment 'Const
-- FIXME: Verify that the pointer is actually 64-bit aligned before casting.
fromByteString (PS fptr offset len) =
    SegConst $ ConstSegment (SV.unsafeCast $ SV.unsafeFromForeignPtr fptr offset len)

-- | Convert a segment to a byte string. O(1)
toByteString :: Segment 'Const -> ByteString
toByteString (SegConst (ConstSegment vec)) = PS fptr offset len where
    (fptr, offset, len) = SV.unsafeToForeignPtr (SV.unsafeCast vec)

-- | @'getSegment' message index@ fetches the given segment in the message.
-- It throws a 'E.BoundsError' if the address is out of bounds.
getSegment :: (MonadThrow m, MonadReadMessage mut m) => Message mut -> Int -> m (Segment mut)
getSegment msg i = do
    checkIndex i =<< numSegs msg
    internalGetSeg msg i

-- | @'withCapTable'@ replaces the capability table in the message.
withCapTable :: V.Vector Client -> Message 'Const -> Message 'Const
withCapTable newCaps (MsgConst msg) = MsgConst $ msg { constCaps = newCaps }

-- | 'getCapTable' gets the capability table from a 'ConstMsg'.
getCapTable :: Message 'Const -> V.Vector Client
getCapTable (MsgConst ConstMsg{constCaps}) = constCaps

-- | @'getCap' message index@ gets the capability with the given index from
-- the message. throws 'E.BoundsError' if the index is out
-- of bounds.
getCap :: (MonadThrow m, MonadReadMessage mut m) => Message mut -> Int -> m Client
getCap msg i = do
    ncaps <- numCaps msg
    if i >= ncaps || i < 0
        then pure nullClient
        else msg `internalGetCap` i

-- | @'setSegment' message index segment@ sets the segment at the given index
-- in the message. It throws a 'E.BoundsError' if the address is out of bounds.
setSegment :: WriteCtx m s => Message ('Mut s) -> Int -> Segment ('Mut s) -> m ()
setSegment msg i seg = do
    checkIndex i =<< numSegs msg
    internalSetSeg msg i seg

-- | @'setCap' message index cap@ sets the sets the capability at @index@ in
-- the message's capability table to @cap@. If the index is out of bounds, a
-- 'E.BoundsError' will be thrown.
setCap :: WriteCtx m s => Message ('Mut s) -> Int -> Client -> m ()
setCap msg@(MsgMut MutMsg{mutCaps}) i cap = do
    checkIndex i =<< numCaps msg
    capTable <- AppendVec.getVector <$> readMutVar mutCaps
    MV.write capTable i cap

-- | 'appendCap' appends a new capabilty to the end of a message's capability
-- table, returning its index.
appendCap :: WriteCtx m s => Message ('Mut s) -> Client -> m Int
appendCap msg@(MsgMut MutMsg{mutCaps}) cap = do
    i <- numCaps msg
    capTable <- readMutVar mutCaps
    capTable <- AppendVec.grow capTable 1 maxCaps
    writeMutVar mutCaps capTable
    setCap msg i cap
    pure i

-- | A read-only capnproto message.
--
-- 'ConstMsg' is an instance of the generic 'Message' type class.
data ConstMsg = ConstMsg
    { constSegs :: V.Vector (Segment 'Const)
    , constCaps :: V.Vector Client
    }
    deriving(Eq)

instance Monad m => MonadReadMessage 'Const m where
    numSegs (MsgConst ConstMsg{constSegs}) = pure $ V.length constSegs
    numCaps (MsgConst ConstMsg{constCaps}) = pure $ V.length constCaps
    internalGetSeg (MsgConst ConstMsg{constSegs}) i = constSegs `V.indexM` i
    internalGetCap (MsgConst ConstMsg{constCaps}) i = constCaps `V.indexM` i

    numWords (SegConst (ConstSegment vec)) = pure $ WordCount $ SV.length vec
    slice (WordCount start) (WordCount len) (SegConst (ConstSegment vec)) =
        pure $ SegConst $ ConstSegment (SV.slice start len vec)
    read (SegConst (ConstSegment vec)) i = pure $! fromLE64 $! vec SV.! fromIntegral i


-- | 'decode' decodes a message from a bytestring.
--
-- The segments will not be copied; the resulting message will be a view into
-- the original bytestring. Runs in O(number of segments in the message).
decode :: MonadThrow m => ByteString -> m (Message 'Const)
decode bytes = decodeSeg (fromByteString bytes)

-- | 'encode' encodes a message as a bytestring builder.
encode :: Message 'Const -> BB.Builder
encode msg =
    -- We use Maybe as the MonadThrow instance required by
    -- writeMessage/toByteString, but we know this can't actually fail,
    -- so we ignore errors.
    fromJust $ execWriterT $ writeMessage
        msg
        (tell . BB.word32LE)
        (tell . BB.byteString . toByteString)

-- | 'decodeSeg' decodes a message from a segment, treating the segment as if
-- it were raw bytes.
--
-- this is mostly here as a helper for 'decode'.
decodeSeg :: MonadThrow m => Segment 'Const -> m (Message 'Const)
decodeSeg seg = do
    len <- numWords seg
    flip evalStateT (Nothing, 0) $ evalLimitT len $
        -- Note: we use the traversal limit to avoid needing to do bounds checking
        -- here; since readMessage invoices the limit before reading, we can rely
        -- on it not to read past the end of the blob.
        --
        -- TODO: while this works, it means that we throw 'TraversalLimitError'
        -- on failure, which makes for a confusing API.
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
    readSegment len = do
        (cur, idx) <- get
        put (cur, idx + len)
        lift $ lift $ slice idx len seg

-- | @'readMessage' read32 readSegment@ reads in a message using the
-- monadic context, which should manage the current read position,
-- into a message. read32 should read a 32-bit little-endian integer,
-- and @readSegment n@ should read a blob of @n@ 64-bit words.
-- The size of the message (in 64-bit words) is deducted from the traversal,
-- limit which can be used to set the maximum message size.
readMessage :: (MonadThrow m, MonadLimit m) => m Word32 -> (WordCount -> m (Segment 'Const)) -> m (Message 'Const)
readMessage read32 readSegment = do
    invoice 1
    numSegs' <- read32
    let numSegs = numSegs' + 1
    invoice (fromIntegral numSegs `div` 2)
    segSizes <- V.replicateM (fromIntegral numSegs) read32
    when (even numSegs) $ void read32
    V.mapM_ (invoice . fromIntegral) segSizes
    constSegs <- V.mapM (readSegment . fromIntegral) segSizes
    pure $ MsgConst ConstMsg{constSegs, constCaps = V.empty}

-- | @'writeMesage' write32 writeSegment@ writes out the message. @write32@
-- should write a 32-bit word in little-endian format to the output stream.
-- @writeSegment@ should write a blob.
writeMessage :: MonadThrow m => Message 'Const -> (Word32 -> m ()) -> (Segment 'Const -> m ()) -> m ()
writeMessage (MsgConst ConstMsg{constSegs}) write32 writeSegment = do
    let numSegs = V.length constSegs
    write32 (fromIntegral numSegs - 1)
    V.forM_ constSegs $ \seg -> write32 . fromIntegral =<< numWords seg
    when (even numSegs) $ write32 0
    V.forM_ constSegs writeSegment


-- | @'hPutMsg' handle msg@ writes @msg@ to @handle@. If there is an exception,
-- it will be an 'IOError' raised by the underlying IO libraries.
hPutMsg :: Handle -> Message 'Const -> IO ()
hPutMsg handle msg = BB.hPutBuilder handle (encode msg)

-- | Equivalent to @'hPutMsg' 'stdout'@
putMsg :: Message 'Const -> IO ()
putMsg = hPutMsg stdout

-- | @'hGetMsg' handle limit@ reads a message from @handle@ that is at most
-- @limit@ 64-bit words in length.
hGetMsg :: Handle -> WordCount -> IO (Message 'Const)
hGetMsg handle size =
    evalLimitT size $ readMessage read32 readSegment
  where
    read32 :: LimitT IO Word32
    read32 = lift $ do
        bytes <- BS.hGet handle 4
        case runGetS getWord32le bytes of
            Left _ ->
                -- the only way this can happen is if we get < 4 bytes.
                throwM $ E.InvalidDataError "Unexpected end of input"
            Right result ->
                pure result
    readSegment n =
        lift (fromByteString <$> BS.hGet handle (fromIntegral n * 8))

-- | Equivalent to @'hGetMsg' 'stdin'@
getMsg :: WordCount -> IO (Message 'Const)
getMsg = hGetMsg stdin

-- | A 'MutMsg' is a mutable capnproto message. The type parameter @s@ is the
-- state token for the instance of 'PrimMonad' in which the message may be
-- modified.
data MutMsg s = MutMsg
    { mutSegs :: MutVar s (AppendVec MV.MVector s (Segment ('Mut s)))
    , mutCaps :: MutVar s (AppendVec MV.MVector s Client)
    }
    deriving(Eq)

-- | 'WriteCtx' is the context needed for most write operations.
type WriteCtx m s = (PrimMonad m, s ~ PrimState m, MonadThrow m)

instance (PrimMonad m, s ~ PrimState m) => MonadReadMessage ('Mut s) m where
    numWords (SegMut MutSegment{used}) = stToPrim $ readMutVar used
    slice (WordCount start) (WordCount len) (SegMut MutSegment{vec, used}) = stToPrim $ do
        WordCount end <- readMutVar used
        let len' = min (end - start) len
        used' <- newMutVar $ WordCount len'
        pure $ SegMut MutSegment
            { vec = SMV.slice start len' vec
            , used = used'
            }
    read (SegMut MutSegment{vec}) i = stToPrim $
        fromLE64 <$> SMV.read vec (fromIntegral i)
    numSegs (MsgMut MutMsg{mutSegs}) =
        stToPrim $ GMV.length . AppendVec.getVector <$> readMutVar mutSegs
    numCaps (MsgMut MutMsg{mutCaps}) =
        stToPrim $ GMV.length . AppendVec.getVector <$> readMutVar mutCaps
    internalGetSeg (MsgMut MutMsg{mutSegs}) i = stToPrim $ do
        segs <- AppendVec.getVector <$> readMutVar mutSegs
        MV.read segs i
    internalGetCap (MsgMut MutMsg{mutCaps}) i = stToPrim $ do
        caps <- AppendVec.getVector <$> readMutVar mutCaps
        MV.read caps i


-- | @'internalSetSeg' message index segment@ sets the segment at the given
-- index in the message. Most callers should use the 'setSegment' wrapper,
-- instead of calling this directly.
internalSetSeg :: WriteCtx m s => Message ('Mut s) -> Int -> Segment ('Mut s) -> m ()
internalSetSeg (MsgMut MutMsg{mutSegs}) segIndex seg = do
    segs <- AppendVec.getVector <$> readMutVar mutSegs
    MV.write segs segIndex seg

-- | @'write' segment index value@ writes a value to the 64-bit word
-- at the provided index. Consider using 'setWord' on the message,
-- instead of calling this directly.
write :: WriteCtx m s => Segment ('Mut s) -> WordCount -> Word64 -> m ()
write (SegMut MutSegment{vec}) (WordCount i) val = do
    SMV.write vec i (toLE64 val)

-- | @'newSegment' msg sizeHint@ allocates a new, initially empty segment in
-- @msg@ with a capacity of @sizeHint@ words. It returns the a pair of the
-- segment number and the segment itself. Amortized O(1).
newSegment :: WriteCtx m s => Message ('Mut s) -> WordCount -> m (Int, Segment ('Mut s))
newSegment msg@(MsgMut MutMsg{mutSegs}) sizeHint = do
    when (sizeHint > maxSegmentSize) $ throwM E.SizeError
    -- the next segment number will be equal to the *current* number of
    -- segments:
    segIndex <- numSegs msg

    -- make space for th new segment
    segs <- readMutVar mutSegs
    segs <- AppendVec.grow segs 1 maxSegments
    writeMutVar mutSegs segs

    vec <- SMV.new (fromIntegral sizeHint)
    used <- newMutVar 0
    let newSeg = SegMut MutSegment{vec, used}
    setSegment msg segIndex newSeg
    pure (segIndex, newSeg)

-- | Like 'alloc', but the second argument allows the caller to specify the
-- index of the segment in which to allocate the data. Returns 'Nothing' if there is
-- insufficient space in that segment..
allocInSeg :: WriteCtx m s => Message ('Mut s) -> Int -> WordCount -> m (Maybe (WordPtr ('Mut s)))
{-# INLINE allocInSeg #-}
allocInSeg msg segIndex size = do
    -- GHC's type inference aparently isn't smart enough to figure
    -- out that the pattern irrefutable if we do seg@(SegMut ...) <- ...
    -- but this works:
    seg <- getSegment msg segIndex
    case seg of
        SegMut MutSegment{vec, used} -> do
            nextAlloc <- readMutVar used
            if WordCount (SMV.length vec) - nextAlloc < size
                then pure Nothing
                else (do
                    writeMutVar used $! nextAlloc + size
                    pure $ Just WordPtr
                        { pAddr = WordAt
                            { segIndex
                            , wordIndex = nextAlloc
                            }
                        , pSegment = seg
                        , pMessage = msg
                        })

-- | @'alloc' size@ allocates 'size' words within a message. it returns the
-- starting address of the allocated memory, as well as a direct reference
-- to the segment. The latter is redundant information, but this is used
-- in low-level code where this can improve performance.
alloc :: WriteCtx m s => Message ('Mut s) -> WordCount -> m (WordPtr ('Mut s))
{-# INLINABLE alloc #-}
alloc msg size = do
    when (size > maxSegmentSize) $
        throwM E.SizeError
    segIndex <- pred <$> numSegs msg
    existing <- allocInSeg msg segIndex size
    case existing of
        Just res -> pure res
        Nothing -> do
            -- Not enough space in the current segment; allocate a new one.
            -- the new segment's size should match the total size of existing segments
            -- but `maxSegmentSize` bounds how large it can get.
            totalAllocation <- sum <$>
                traverse (getSegment msg >=> numWords) [0..segIndex]
            ( newSegIndex, _ ) <- newSegment msg (min (max totalAllocation size) maxSegmentSize)
            -- This is guaranteed to succeed, since we just made a segment with
            -- at least size available space:
            fromJust <$> allocInSeg msg newSegIndex size

-- | 'empty' is an empty message, i.e. a minimal message with a null pointer as
-- its root object.
empty :: Message 'Const
empty = MsgConst ConstMsg
    { constSegs = V.fromList [ SegConst $ ConstSegment $ SV.fromList [0] ]
    , constCaps = V.empty
    }

-- | @'newMessage' sizeHint@ allocates a new empty message, with a single segment
-- having capacity @sizeHint@. If @sizeHint@ is 'Nothing', defaults to a sensible
-- value.
newMessage :: WriteCtx m s => Maybe WordCount -> m (Message ('Mut s))
newMessage Nothing = newMessage (Just 32)
    -- The default value above is somewhat arbitrary, and just a guess -- we
    -- should do some profiling to figure out what a good value is here.
newMessage (Just sizeHint) = do
    mutSegs <- MV.new 1 >>= newMutVar . AppendVec.makeEmpty
    mutCaps <- MV.new 0 >>= newMutVar . AppendVec.makeEmpty
    let msg = MsgMut MutMsg{mutSegs,mutCaps}
    -- allocte the first segment, and make space for the root pointer:
    _ <- newSegment msg sizeHint
    _ <- alloc msg 1
    pure msg

-- | Create a message from a single segment.
singleSegment :: Segment 'Const -> Message 'Const
singleSegment seg = MsgConst ConstMsg
    { constSegs = V.singleton seg
    , constCaps = V.empty
    }

instance MaybeMutable Segment where
    thaw         = thawSeg   SV.thaw
    unsafeThaw   = thawSeg   SV.unsafeThaw
    freeze       = freezeSeg SV.freeze
    unsafeFreeze = freezeSeg SV.unsafeFreeze

-- Helpers for @Segment ConstMsg@'s Thaw instance.
thawSeg
    :: (PrimMonad m, s ~ PrimState m)
    => (SV.Vector Word64 -> m (SMV.MVector s Word64))
    -> Segment 'Const
    -> m (Segment ('Mut s))
thawSeg thaw (SegConst (ConstSegment vec)) = do
    mvec <- thaw vec
    used <- newMutVar $ WordCount $ SV.length vec
    pure $ SegMut MutSegment { vec = mvec, used }

freezeSeg
    :: (PrimMonad m, s ~ PrimState m)
    => (SMV.MVector s Word64 -> m (SV.Vector Word64))
    -> Segment ('Mut s)
    -> m (Segment 'Const)
freezeSeg freeze (SegMut MutSegment{vec, used}) = do
    WordCount len <- readMutVar used
    SegConst .ConstSegment <$> freeze (SMV.take len vec)

instance MaybeMutable Message where
    thaw         = thawMsg   thaw         V.thaw
    unsafeThaw   = thawMsg   unsafeThaw   V.unsafeThaw
    freeze       = freezeMsg freeze       V.freeze
    unsafeFreeze = freezeMsg unsafeFreeze V.unsafeFreeze

-- Helpers for ConstMsg's Thaw instance.
thawMsg :: (PrimMonad m, s ~ PrimState m)
    => (Segment 'Const -> m (Segment ('Mut s)))
    -> (V.Vector Client -> m (MV.MVector s Client))
    -> Message 'Const
    -> m (Message ('Mut s))
thawMsg thawSeg thawCaps (MsgConst ConstMsg{constSegs, constCaps}) = do
    mutSegs <- newMutVar . AppendVec.fromVector =<< (V.mapM thawSeg constSegs >>= V.unsafeThaw)
    mutCaps <- newMutVar . AppendVec.fromVector =<< thawCaps constCaps
    pure $ MsgMut MutMsg{mutSegs, mutCaps}
freezeMsg :: (PrimMonad m, s ~ PrimState m)
    => (Segment ('Mut s) -> m (Segment 'Const))
    -> (MV.MVector s Client -> m (V.Vector Client))
    -> Message ('Mut s)
    -> m (Message 'Const)
freezeMsg freezeSeg freezeCaps msg@(MsgMut MutMsg{mutCaps}) = do
    len <- numSegs msg
    constSegs <- V.generateM len (internalGetSeg msg >=> freezeSeg)
    constCaps <- freezeCaps . AppendVec.getVector =<< readMutVar mutCaps
    pure $ MsgConst ConstMsg{constSegs, constCaps}

-- | @'checkIndex' index length@ checkes that 'index' is in the range
-- [0, length), throwing a 'BoundsError' if not.
checkIndex :: (Integral a, MonadThrow m) => a -> a -> m ()
checkIndex i len =
    when (i < 0 || i >= len) $
        throwM E.BoundsError
            { E.index = fromIntegral i
            , E.maxIndex = fromIntegral len
            }
