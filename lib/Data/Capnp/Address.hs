{-|
Module: Data.Capnp.Address
Description: Utilities for manipulating addresses within capnproto messages.
-}
{-# LANGUAGE RecordWildCards #-}
module Data.Capnp.Address
    ( WordAddr(..)
    , CapAddr(..)
    , Addr(..)
    , OffsetError(..)
    , computeOffset
    , pointerFrom
    , resolvePtr
    )
  where

import Data.Bits
import Data.Word

import Data.Capnp.Bits (WordCount)

import qualified Data.Capnp.Pointer as P

-- | The address of a word within a message
data WordAddr = WordAt
    { segIndex  :: !Int -- ^ Segment number
    , wordIndex :: !WordCount -- ^ offset in words from the start of the segment.
    } deriving(Show, Eq)

-- | The "address" of a capability
newtype CapAddr = Cap Word32 deriving(Show, Eq)

-- | An address, i.e. a location that a pointer may point at.
data Addr
    = WordAddr !WordAddr
    | CapAddr !CapAddr
    deriving(Show, Eq)

-- | An error returned by 'computeOffset'; this describes the reason why a
-- value cannot be directly addressed from a given location.
data OffsetError
    -- | The pointer and the value are in different segments.
    = DifferentSegments
    -- | The pointer is in the correct segment, but too far away to encode the
    -- offset. (more than 30 bits would be required). This can only happen with
    -- segments that are > 8 GiB, which this library refuses to either decode
    -- or generate, so this should not come up in practice (TODO: actually add
    -- the code to enforce this).
    | OutOfRange

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

-- | @'pointerFrom ptrAddr targetAddr ptr@ updates @ptr@, such that it is
-- correct to target a value located at @targetAddr@ given that the pointer
-- itself is located at @ptrAddr@. Returns @Left@ if this is not possible.
--
-- It is illegal to call this on a capability pointer.
--
-- For far pointers, @targetAddr@ is taken to be the address of the landing pad,
-- rather than the final value.
pointerFrom :: WordAddr -> WordAddr -> P.Ptr -> Either OffsetError P.Ptr
pointerFrom _ _ (P.CapPtr _) = error "pointerFrom called on a capability pointer."
pointerFrom _ WordAt{..} (P.FarPtr twoWords _ _) =
    Right $ P.FarPtr twoWords (fromIntegral wordIndex) (fromIntegral segIndex)
pointerFrom ptrAddr targetAddr (P.StructPtr _ dataSz ptrSz) =
    flip fmap (computeOffset ptrAddr targetAddr) $
        \off -> P.StructPtr (fromIntegral off) dataSz ptrSz
pointerFrom ptrAddr targetAddr (P.ListPtr _ eltSpec) =
    flip fmap (computeOffset ptrAddr targetAddr) $
        \off -> P.ListPtr (fromIntegral off) eltSpec

-- | @resolvePtr from ptr@ Resolves the pointer @ptr@ to an address
-- relative to @from@. Note that inter-segment pointers (FarPtr)
-- resolve to the address of the landing pad, *not* the the final
-- address of the object pointed to, as that would reqiure access
-- to the message.
resolvePtr :: WordAddr -> P.Ptr -> Addr
resolvePtr (WordAt seg word) (P.StructPtr off _dataSz _ptrSz) =
    WordAddr $ WordAt seg (word + fromIntegral off + 1)
resolvePtr (WordAt seg word) (P.ListPtr off _) =
    WordAddr (WordAt seg (word + fromIntegral off + 1))
resolvePtr _ (P.FarPtr _ word seg) =
    WordAddr $ WordAt
        (fromIntegral seg)
        (fromIntegral word)
resolvePtr _ (P.CapPtr cap) = CapAddr (Cap cap)
