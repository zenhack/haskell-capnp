module Module.Capnp.Bits (bitsTests) where

import Data.Bits
import Data.Word
import Test.Hspec

import Data.Foldable (traverse_)

import Capnp.Bits


bitsTests = do
    describe "bitRange" bitRangeExamples
    describe "replaceBits" replaceBitsExamples

bitRangeExamples = do
    it "Should get single bits correctly" $
        traverse_ bitRangeTest ones
    it "Should work on this extra example" $
        bitRangeTest
            (0x0000000200000000, 32, 48, 2)
  where
    bitRangeTest (word, lo, hi, expected) =
        bitRange word lo hi `shouldBe` expected
    ones = map (\bit ->  (1 `shiftL` bit, bit, bit + 1, 1)) [0..63]

replaceBitsExamples =
    it "Should work with several examples" $
        sequence_
            [ replaceTest 8 (0xf :: Word8) 0      0 0xf
            , replaceTest 8 (0x1 :: Word8) 0xf    0 0x1
            , replaceTest 8 (0x2 :: Word8) 0x1    0 0x2
            , replaceTest 8 (0x1 :: Word8) 0xf    0 0x1
            , replaceTest 8 (0x2 :: Word8) 0x10   4 0x20
            , replaceTest 8 (0x1 :: Word8) 0x10   8 0x0110
            , replaceTest 8 (0xa :: Word8) 0xffff 8 0x0aff
            , replaceTest 1 (0x0 :: Word1) 0xff  4 0xef
            ]
 where
    replaceTest :: (Bounded a, Integral a, Show a)
        => Int -> a -> Word64 -> Int -> Word64 -> Expectation
    replaceTest len new orig shift expected =
        replaceBits new orig shift `shouldBe` expected
