{-|
Module: Capnp.Bits
Description: Utilities for bitwhacking useful for capnproto.

This module provides misc. utilities for bitwhacking that are useful
in dealing with low-level details of the Cap'N Proto wire format.

This is mostly an implementation detail; users are unlikely to need
to use this module directly.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Capnp.Bits
    ( BitCount(..)
    , ByteCount(..)
    , WordCount(..)
    , Word1(..)
    , bitsToBytesCeil
    , bytesToWordsCeil
    , bytesToWordsFloor
    , wordsToBytes
    , lo, hi
    , i32, i30, i29
    , fromLo, fromHi
    , fromI32, fromI30, fromI29
    , bitRange
    , replaceBits
    )
  where

-- TODO: the only reason this module is in exposed-modules is so it can be
-- seen by the test suite. We should find a way to avoid this.

import Data.Bits
import Data.Int
import Data.Word

-- | Wrapper type for a quantity of bits. This along with 'ByteCount' and
-- 'WordCount' are helpful for avoiding mixing up units
newtype BitCount = BitCount Int
    deriving(Num, Real, Integral, Bits, Ord, Eq, Enum, Show)

-- | A quantity of bytes
newtype ByteCount = ByteCount Int
    deriving(Num, Real, Integral, Bits, Ord, Eq, Enum, Show)

-- | A quantity of 64-bit words
newtype WordCount = WordCount Int
    deriving(Num, Real, Integral, Bits, Ord, Eq, Enum, Show)

-- | Convert bits to bytes. Rounds up.
bitsToBytesCeil :: BitCount -> ByteCount
bitsToBytesCeil (BitCount n) = ByteCount ((n + 7) `div` 8)

-- | Convert bytes to words. Rounds up.
bytesToWordsCeil :: ByteCount -> WordCount
bytesToWordsCeil (ByteCount n) = WordCount ((n + 7) `div` 8)

-- | Convert bytes to words. Rounds down.
bytesToWordsFloor :: ByteCount -> WordCount
bytesToWordsFloor (ByteCount n) = WordCount (n `div` 8)

-- | Convert words to bytes.
wordsToBytes :: WordCount -> ByteCount
wordsToBytes (WordCount n) = ByteCount (n * 8)

-- | lo and hi extract the low and high 32 bits of a 64-bit word, respectively.
lo, hi :: Word64 -> Word32

-- | iN (where N is 32, 30, or 29) extracts the high N bits of its argument,
-- and treats them as a signed 32-bit integer.
i32, i30, i29 :: Word32 -> Int32

-- | fromLo and fromHi convert a 32-bit word to the low or high portion of
-- a 64-bit word. In general, @fromHi (hi w) .|. fromLo (lo w) == w@.
fromLo, fromHi :: Word32 -> Word64

-- | fromIN (where N is 32, 30, or 29) treats its argument as the high N bits of
-- a 32-bit word, returning the word. If @w < 2 ** N@ then @fromIN (iN w) == w@.
fromI32, fromI30, fromI29 :: Int32 -> Word32

lo w = fromIntegral (w `shiftR`  0)
hi w = fromIntegral (w `shiftR` 32)
i32 = fromIntegral
i30 w = i32 w `shiftR` 2
i29 w = i32 w `shiftR` 3

fromLo w = fromIntegral w `shiftL`  0
fromHi w = fromIntegral w `shiftL` 32
fromI32 = fromIntegral
fromI30 w = fromI32 (w `shiftL` 2)
fromI29 w = fromI32 (w `shiftL` 3)

-- | @bitRange word lo hi@ is the unsigned integer represented by the
-- bits of @word@ in the range [lo, hi)
bitRange :: (Integral a => Word64 -> Int -> Int -> a)
bitRange word lo hi = fromIntegral $
    (word .&. ((1 `shiftL` hi) - 1)) `shiftR` lo

-- | @replaceBits new orig shift@ replaces the bits [shift, shift+N) in
-- @orig@ with the N bit integer @new@.
replaceBits :: (Bounded a, Integral a)
    => a -> Word64 -> Int -> Word64
replaceBits new orig shift =
    (orig .&. mask) .|. (fromIntegral new `shiftL` shift)
  where
    mask = complement $ fromIntegral (maxBound `asTypeOf` new) `shiftL` shift

-- | 1 bit datatype, in the tradition of Word8, Word16 et al.
newtype Word1 = Word1 { word1ToBool :: Bool }
    deriving(Ord, Eq, Enum, Bounded, Bits)

instance Num Word1 where
    (+) = w1ThruEnum (+)
    (*) = w1ThruEnum (*)
    abs = id
    signum = id
    negate = id
    fromInteger x = toEnum (fromIntegral x `mod` 2)

instance Real Word1 where
    toRational = fromIntegral . fromEnum

instance Integral Word1 where
    toInteger = toInteger . fromEnum
    quotRem x y = let (x', y') = quotRem (fromEnum x) (fromEnum y)
                  in (toEnum x', toEnum y')

instance Show Word1 where
    show = show . fromEnum
    -- TODO: implement Read?

w1ThruEnum :: (Int -> Int -> Int) -> Word1 -> Word1 -> Word1
w1ThruEnum op l r = toEnum $ (fromEnum l `op` fromEnum r) `mod` 2
