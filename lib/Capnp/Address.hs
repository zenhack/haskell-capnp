{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Capnp.Address
-- Description: Utilities for manipulating addresses within capnproto messages.
--
-- This module provides facilities for manipulating raw addresses within
-- Cap'N Proto messages.
--
-- This is a low level module that very few users will need to use directly.
module Capnp.Address
  ( WordAddr (..),
    CapAddr (..),
    Addr (..),
    OffsetError (..),
    computeOffset,
    pointerFrom,
    resolveOffset,
  )
where

import Capnp.Bits (WordCount)
import qualified Capnp.Pointer as P
import Data.Bits
import Data.Int
import Data.Word

-- | The address of a word within a message
data WordAddr = WordAt
  { -- | Segment number
    segIndex :: !Int,
    -- | offset in words from the start of the segment.
    wordIndex :: !WordCount
  }
  deriving (Show, Eq)

-- | The "address" of a capability
newtype CapAddr = Cap Word32 deriving (Show, Eq)

-- | An address, i.e. a location that a pointer may point at.
data Addr
  = -- | The address of some data in the message.
    WordAddr !WordAddr
  | -- | The "address" of a capability.
    CapAddr !CapAddr
  deriving (Show, Eq)

-- | An error returned by 'computeOffset'; this describes the reason why a
-- value cannot be directly addressed from a given location.
data OffsetError
  = -- | The pointer and the value are in different segments.
    DifferentSegments
  | -- | The pointer is in the correct segment, but too far away to encode the
    -- offset. (more than 30 bits would be required). This can only happen with
    -- segments that are > 8 GiB, which this library refuses to either decode
    -- or generate, so this should not come up in practice.
    OutOfRange

-- | @'computeOffset' ptrAddr valueAddr@ computes the offset that should be
-- stored in a struct or list pointer located at @ptrAddr@, in order to point
-- at a value located at @valueAddr@. If the value cannot be directly addressed
-- by a pointer at @ptrAddr@, then this returns 'Left', with the 'OffsetError'
-- describing the problem.
computeOffset :: WordAddr -> WordAddr -> Either OffsetError WordCount
computeOffset ptrAddr valueAddr
  | segIndex ptrAddr /= segIndex valueAddr = Left DifferentSegments
  | otherwise =
      let offset = wordIndex valueAddr - (wordIndex ptrAddr + 1)
       in if offset >= 1 `shiftL` 30
            then Left OutOfRange
            else Right offset

-- | @'pointerFrom' ptrAddr targetAddr ptr@ updates @ptr@, such that it is
-- correct to target a value located at @targetAddr@ given that the pointer
-- itself is located at @ptrAddr@. Returns 'Left' if this is not possible.
--
-- It is illegal to call this on a capability pointer.
--
-- For far pointers, @targetAddr@ is taken to be the address of the landing pad,
-- rather than the final value.
pointerFrom :: WordAddr -> WordAddr -> P.Ptr -> Either OffsetError P.Ptr
pointerFrom _ _ (P.CapPtr _) = error "pointerFrom called on a capability pointer."
pointerFrom _ WordAt {..} (P.FarPtr twoWords _ _) =
  Right $ P.FarPtr twoWords (fromIntegral wordIndex) (fromIntegral segIndex)
pointerFrom ptrAddr targetAddr (P.StructPtr _ dataSz ptrSz) =
  flip fmap (computeOffset ptrAddr targetAddr) $
    \off -> P.StructPtr (fromIntegral off) dataSz ptrSz
pointerFrom ptrAddr targetAddr (P.ListPtr _ eltSpec) =
  flip fmap (computeOffset ptrAddr targetAddr) $
    \off -> P.ListPtr (fromIntegral off) eltSpec

-- | Add an offset to a WordAddr.
resolveOffset :: WordAddr -> Int32 -> WordAddr
resolveOffset addr@WordAt {..} off =
  addr {wordIndex = wordIndex + fromIntegral off + 1}
