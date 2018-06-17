{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-|
Module: Data.Capnp.Message.Generic
Description: Generic interfaces for message storage implementations.
-}
module Data.Capnp.Message.Generic
    ( Segment(..)
    , MSegment(..)
    , Message(..)
    , MMessage(..)
    , getSegment
    , getWord
    ) where

import Prelude hiding (read)

import Data.Word

import Control.Monad.Catch      (MonadThrow)
import Data.ByteString          (ByteString)
import Data.Capnp.Address       (WordAddr(..))
import Data.Capnp.Bits          (WordCount(..))
import Data.Capnp.Internal.Util (checkIndex)

-- | The 'Segment' type class captures the interface for a (possibly read
-- only) segment in a capnproto message.
--
-- In addition to the segment type, the type class is parametrized over a
-- monad in which the operations are preformed.
class Monad m => Segment m seg where
    -- | Get the length of the segment, in units of 64-bit words.
    numWords :: seg -> m Int
    -- | @'slice' start length segment@ extracts a sub-section of the segment,
    -- starting at index @start@, of length @length@.
    slice   :: Int -> Int -> seg -> m seg
    -- | @'read' segment index@ reads a 64-bit word from the segement at the
    -- given index. Consider using 'getWord' on the message, instead of
    -- calling this directly.
    read    :: seg -> Int -> m Word64
    -- | Convert a ByteString to a segment.
    fromByteString :: ByteString -> m seg
    -- | Convert a segment to a byte string.
    toByteString :: seg -> m ByteString

-- | An 'MSegment' is a mutable segment in a capnproto message.
class Segment m seg => MSegment m seg where
    -- | @'write' segment index value@ writes a value to the 64-bit word
    -- at the provided index. Consider using 'setWord' on the message,
    -- instead of calling this directly.
    write :: seg -> Int -> Word64 -> m ()
    -- | @'grow' segment amount@ grows the segment by the specified number
    -- of 64-bit words. The original segment should not be used afterwards.
    grow  :: seg -> Int -> m seg

-- | A 'Message' is a (possibly read-only) capnproto message. It is
-- parameterized over a monad in which operations ar eperformed, and the
-- segment type.
class Segment m seg => Message m msg seg | msg -> seg where
    -- | 'numSegs' gets the number of segments in a message.
    numSegs :: msg -> m Int
    -- | @'internalGetSeg' message index@ gets the segment at index 'index'
    -- in 'message'. Most callers should use the 'getSegment' wrapper, instead
    -- of calling this directly.
    internalGetSeg :: msg -> Int -> m seg

-- | An 'MMessage' is a mutable capnproto message.
class (Message m msg seg, MSegment m seg) => MMessage m msg seg where
    -- | @'internalSetSeg' message index segment@ sets the segment at the given
    -- index in the message. Most callers should use the 'setSegment' wrapper,
    -- instead of calling this directly.
    internalSetSeg :: msg -> Int -> seg -> m ()

-- | @'getSegment' message index@ fetches the given segment in the message.
-- It throws a @BoundsError@ if the address is out of bounds.
getSegment :: (MonadThrow m, Message m msg seg) => msg -> Int -> m seg
getSegment msg i = do
    checkIndex i =<< numSegs msg
    internalGetSeg msg i

-- | @'getWord' msg addr@ returns the word at @addr@ within @msg@. It throws a
-- @BoundsError@ if the address is out of bounds.
getWord :: (MonadThrow m, Message m msg seg) => msg -> WordAddr -> m Word64
getWord msg WordAt{wordIndex=wordIndex@(WordCount i), segIndex} = do
    seg <- getSegment msg segIndex
    checkIndex i =<< numWords seg
    seg `read` i

-- | @'setSegment' message index segment@ sets the segment at the given index
-- in the message. It throws a @BoundsError@ if the address is out of bounds.
setSegment :: (MonadThrow m, MMessage m msg seg) => msg -> Int -> seg -> m ()
setSegment msg i seg = do
    checkIndex i =<< numSegs msg
    internalSetSeg msg i seg

-- | @'setWord' message address value@ sets the word at @address@ in the
-- message to @value@. If the address is not valid in the message, a
-- @BoundsError@ will be thrown.
setWord :: (MonadThrow m, MMessage m msg seg) => msg -> WordAddr -> Word64 -> m ()
setWord msg WordAt{wordIndex=WordCount i, segIndex} val = do
    seg <- getSegment msg segIndex
    checkIndex i =<< numWords seg
    write seg i val
