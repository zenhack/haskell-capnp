{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-|
Module: Control.Monad.CapNProto.MessageBuilder
Description: support for building capnproto messages.
-}
module Control.Monad.CapNProto.MessageBuilder where

import Prelude hiding (length)

import Control.Monad                  (when)
import Control.Monad.Primitive        (PrimMonad, PrimState)
import Control.Monad.Trans.Class      (MonadTrans(lift))
import Control.Monad.Trans.RWS.Strict (RWST(runRWST), ask, get, local, put)
import Data.CapNProto.Bits
    ( Word1(..)
    , WordCount(..)
    , bytesToWordsFloor
    , fromHi
    , replaceBits
    , wordsToBytes
    )
import Data.CapNProto.Blob
import Data.CapNProto.Schema          (Field(..))
import Data.Int
import Data.Primitive.ByteArray       (MutableByteArray, newByteArray)
import Data.Word

import qualified Data.CapNProto.Pointer as P

-- | Internal mutable state of a builder.
data BuilderState s = BuilderState
    { nextAlloc :: !WordCount -- ^ offset into the array for the next allocation
    , array     :: MutableByteArray s -- ^ array storing the message being built
    }

-- | Internal read-only enviornment for the builder.
data BuilderEnv = BuilderEnv
    { parentOff    :: !WordCount -- ^ offset into the array to the start of
                              -- the parent object.
    , parentDataSz :: !Word16 -- ^ The size of the parent's data section. Only
                              -- meaningful if the parent is a struct.
    , parentTagOff :: !Word16 -- ^ offset from parentOff to the word containing
                              -- union tag for parent. The bit offset is in
                              -- childShift. This is only meaningful when parent
                              -- is a union.
    , childShift   :: !Word16 -- ^ The number of bits the child should be shifted
                            -- inside its word. Only meaningful for data fields
                            -- smaller than 64 bits (note that this includes
                            -- union tags).
    }

-- | A monadic builder of messages.
newtype BuilderT p s m a =
    -- This is just a wrapper around RWST; the reader keeps track of the
    -- parent object, the state keeps track of the underlying buffer/allocator,
    -- and the writer is just (), since we don't need it.
    BuilderT (RWST BuilderEnv () (BuilderState s) m a)
    deriving(Functor, Applicative, Monad)

-- | Run a builder and yield the constructed message.
--
-- The result is a pair. The first component is the still-mutable array,
-- wrapped in a BlobSlice. The slice indicates the part of the message that
-- is actually valid; anything past that is unused storage.
--
-- The second component of the pair is simply the value returned by the
-- builder.
runBuilderT :: (PrimMonad m)
    => BuilderT p (PrimState m) m a
    -> m (BlobSlice (MutableByteArray (PrimState m)), a)
runBuilderT (BuilderT m) = do
    initialArray <- newByteArray 0
    (x, bs, ()) <- runRWST
                       m
                       BuilderEnv   { parentOff = 0
                                    , parentDataSz = 0
                                    , parentTagOff = 0
                                    , childShift = 0
                                    }
                       BuilderState { array = initialArray, nextAlloc = 0 }
    return ( BlobSlice { blob = array bs
                       , offset = 0
                       , sliceLen = wordsToBytes $ nextAlloc bs
                       }
           , x
           )

-- | Add a message header to the result of another builder, which must build
-- a single segment (with a struct pointer at the start).
frameSegment :: (PrimMonad m, s ~ PrimState m)
    => BuilderT p s m a -> BuilderT p s m a
frameSegment builder = do
    -- reserve space for the header, run the builder, then find the
    -- segment length and patch it back in. The number of segments is
    -- always 1, and the format specifies we encode that is (n-1), so
    -- we don't have to do anything.
    start <- alloc 1
    result <- builder
    BuilderState{..} <- BuilderT get
    let segWords = fromIntegral (nextAlloc - start - 1) :: Word32
    -- XXX TODO: check for integer overflow
    lift $ writeWord array start (fromHi segWords)
    return result

instance MonadTrans (BuilderT p s) where
    lift = BuilderT . lift

-- | @ensureSpaceFor words@ ensures that the array in the builder state has
-- at least @words@ words of unused space; if not it is resized. resizing is
-- done in such a way that allocation of n words runs amortized O(n) time.
ensureSpaceFor :: (PrimMonad m) => WordCount -> BuilderT p (PrimState m) m ()
ensureSpaceFor sz = BuilderT $ do
    bs@BuilderState{..} <- get
    len <- bytesToWordsFloor <$> length array
    when (len - nextAlloc < sz) $ do
        array' <- lift $ grow array $ wordsToBytes $ len + sz
        put bs{ array = array' }

-- | @alloc words@ allocates space for @words@ words in the builder state, and
-- returns the index of the first word in the allocation.
alloc :: (PrimMonad m)
    => WordCount -> BuilderT p (PrimState m) m WordCount
alloc sz = do
    ensureSpaceFor sz
    bs@BuilderState{..} <- BuilderT get
    BuilderT $ put bs { nextAlloc = nextAlloc + sz }
    return nextAlloc


-- | @withParent words builder@ allocates a new object with size @words@, and
-- runs builder with that object set as the parent object. The location of the
-- newly allocated parent object is returned.
withParent :: (PrimMonad m)
    => WordCount -> BuilderT c (PrimState m) m () -> BuilderT p (PrimState m) m WordCount
withParent sz (BuilderT m) = do
    off <- alloc sz
    BuilderT $ local (\env -> env { parentOff = off }) m
    return off

class BuildSelf a where
    -- | @buildSelf x word offset@ stores the value @x@ in the message at the
    -- word @word@, shifted @offset@ bits from the start of the word. It should
    -- not distrupt other values in the word.
    --
    -- implementations may assume @offset@ is properly aligned; the caller is
    -- required to ensure this.
    buildSelf :: (PrimMonad m, s ~ PrimState m)
        => a -> WordCount -> Word16 -> BuilderT p s m ()

instance BuildSelf Word64 where
    buildSelf = buildSelfReplace

instance BuildSelf Word32 where
    buildSelf = buildSelfReplace

instance BuildSelf Word16 where
    buildSelf = buildSelfReplace

instance BuildSelf Word8 where
    buildSelf = buildSelfReplace

instance BuildSelf Bool where
    buildSelf n = buildSelfReplace (Word1 n)

instance BuildSelf Int64 where
    buildSelf n = buildSelf (fromIntegral n :: Word64)

instance BuildSelf Int32 where
    buildSelf n = buildSelf (fromIntegral n :: Word32)

instance BuildSelf Int16 where
    buildSelf n = buildSelf (fromIntegral n :: Word16)

instance BuildSelf Int8 where
    buildSelf n = buildSelf (fromIntegral n :: Word8)


-- | Helper function for buildSelf; it's a valid implementation for any type
-- that can be used with 'replaceBits'
buildSelfReplace :: (PrimMonad m, s ~ PrimState m, Bounded a, Integral a)
    => a -> WordCount -> Word16 -> BuilderT p s m ()
buildSelfReplace n words shift = BuilderT $ do
    arr <- array <$> get
    BuilderEnv{..} <- ask
    let i = fromIntegral (parentOff + words)
    word <- indexWord arr i
    writeWord arr i $ replaceBits n word (fromIntegral shift)

-- | @setField field value@ is a builder which sets the field @field@ in the
-- parent object to the value @value@.
setField, (%~) :: (PrimMonad m, s ~ PrimState m, BuildSelf c)
    => Field p c -> c                 -> BuilderT p s m ()
setField (DataField word shift) value =
    buildSelf value (fromIntegral word) shift
-- All of our instances of BuildSelf are data fields, so the this should never
-- happen, though it would be nice to improve the type safety:
setField field _ = error $ "setField called with non DataField: " ++ show field

-- | Infix alias for setField
(%~) = setField


-- | @buildField field builder@ is a builder which sets the field @field@ in
-- the parent object to the value built by @builder@.
buildField, (<~) :: (PrimMonad m, s ~ PrimState m)
    => Field p c -> BuilderT c s m () -> BuilderT p s m ()
buildField (DataField word shift) (BuilderT m) = BuilderT $ flip local m $
    \env@BuilderEnv{..} -> env { parentOff = parentOff + fromIntegral word
                               , childShift = shift
                               }
buildField (PtrField n) (BuilderT m) =
    BuilderT $ flip local m $ \env@BuilderEnv{..} ->
        env { parentOff = parentOff + fromIntegral (parentDataSz + n) }
buildField GroupField (BuilderT m) = BuilderT m
buildField (UnionField tagOff tagShift) (BuilderT m) = BuilderT $ flip local m $
    \env@BuilderEnv{..} -> env { parentTagOff = tagOff
                               , childShift = tagShift
                               }

-- | @buildStruct dataSz ptrSz builder@ builds a struct with the given data and
-- pointer section sizes, using @builder@ to fill its contents.
buildStruct :: (PrimMonad m, s ~ PrimState m) => Word16 -> Word16 -> BuilderT c s m () -> BuilderT p s m ()
buildStruct dataSz ptrSz (BuilderT m) = do
    addr <- withParent (fromIntegral dataSz + fromIntegral ptrSz) $ BuilderT $
        flip local m $ \env -> env { parentDataSz = dataSz }
    BuilderEnv{..} <- BuilderT ask
    let offset = addr - (parentOff + 1) -- the offset is from the *end* of the pointer.
    let ptr = P.serializePtr $ Just $ P.StructPtr (fromIntegral offset) dataSz ptrSz
    buildSelf ptr 0 0

-- | Infix alias for buildField
(<~) = buildField
