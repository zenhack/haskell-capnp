{-|
Module: Data.CapNProto.Bits
Description: Utilities for bitwhacking useful for capnproto.
-}
module Data.CapNProto.Bits where

import Data.Bits
import Data.Int
import Data.Word

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
