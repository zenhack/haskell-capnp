{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}
{-|
Module: Data.Capnp.Message.Generic
Description: Generic interfaces for message storage implementations.
-}
module Data.Capnp.Message.Generic
    ( Message(..)
    , MMessage(..)
    , Mutable(..)
    , getSegment
    , getWord
    ) where

import Prelude hiding (read)

import Data.Word

import Control.Monad.Catch      (MonadThrow)
import Control.Monad.Primitive  (PrimMonad)
import Data.ByteString          (ByteString)
import Data.Capnp.Address       (WordAddr(..))
import Data.Capnp.Bits          (WordCount(..))
import Data.Capnp.Internal.Util (checkIndex)

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

-- | An 'MMessage' is a mutable capnproto message.
class Message m msg => MMessage m msg where
    -- | @'internalSetSeg' message index segment@ sets the segment at the given
    -- index in the message. Most callers should use the 'setSegment' wrapper,
    -- instead of calling this directly.
    internalSetSeg :: msg -> Int -> Segment m msg -> m ()
    -- | @'write' segment index value@ writes a value to the 64-bit word
    -- at the provided index. Consider using 'setWord' on the message,
    -- instead of calling this directly.
    write :: Segment m msg -> Int -> Word64 -> m ()
    -- | @'grow' segment amount@ grows the segment by the specified number
    -- of 64-bit words. The original segment should not be used afterwards.
    grow  :: Segment m msg -> Int -> m (Segment m msg)

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
setSegment :: (MonadThrow m, MMessage m msg) => msg -> Int -> Segment m msg -> m ()
setSegment msg i seg = do
    checkIndex i =<< numSegs msg
    internalSetSeg msg i seg

-- | @'setWord' message address value@ sets the word at @address@ in the
-- message to @value@. If the address is not valid in the message, a
-- @BoundsError@ will be thrown.
setWord :: (MonadThrow m, MMessage m msg) => msg -> WordAddr -> Word64 -> m ()
setWord msg WordAt{wordIndex=WordCount i, segIndex} val = do
    seg <- getSegment msg segIndex
    checkIndex i =<< numWords seg
    write seg i val
