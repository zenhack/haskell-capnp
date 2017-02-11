module Data.CapNProto.Bits where

import Data.Bits
import Data.Int
import Data.Word

lo, hi :: Word64 -> Word32
i32, i30, i29 :: Word32 -> Int32
fromLo, fromHi :: Word32 -> Word64
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
