{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
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

class Segment m seg where
    segLen  :: seg -> m Int
    slice   :: Int -> Int -> seg -> m seg
    read    :: seg -> Int -> m Word64
    fromByteString :: ByteString -> m seg
    toByteString :: seg -> m ByteString

class Segment m seg => MSegment m seg where
    write :: seg -> Int -> Word64 -> m ()
    grow  :: seg -> Int -> m seg

class Segment m seg => Message m msg seg | msg -> seg where
    msgLen :: msg -> m Int
    getSeg :: msg -> Int -> m seg

class (Message m msg seg, MSegment m seg) => MMessage m msg seg where
    setSeg :: msg -> Int -> seg -> m ()

getSegment :: (MonadThrow m, Message m msg seg) => msg -> Int -> m seg
getSegment msg i = do
    checkIndex i =<< msgLen msg
    getSeg msg i

-- | @getWord msg addr@ returns the word at @addr@ within @msg@. It throws a
-- @BoundsError@ if the address is out of bounds.
getWord :: (MonadThrow m, Message m msg seg) => msg -> WordAddr -> m Word64
getWord msg WordAt{wordIndex=wordIndex@(WordCount i), segIndex} = do
    seg <- getSegment msg segIndex
    checkIndex i =<< segLen seg
    seg `read` i

setSegment :: (MonadThrow m, MMessage m msg seg) => msg -> Int -> seg -> m ()
setSegment msg i seg = do
    checkIndex i =<< msgLen msg
    setSeg msg i seg

setWord :: (MonadThrow m, MMessage m msg seg) => msg -> WordAddr -> Word64 -> m ()
setWord msg WordAt{wordIndex=WordCount i, segIndex} val = do
    seg <- getSegment msg segIndex
    checkIndex i =<< segLen seg
    write seg i val
