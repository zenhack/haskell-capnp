module Tests.Data.CapNProto.Bits where

import Data.Bits
import Data.Word

import Data.CapNProto.Bits

import Test.HUnit (assertEqual, Test(TestCase, TestList), Assertion)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework (testGroup)


bitsTests = testGroup "bits tests" [bitRangeExamples, replaceBitsExamples]

bitRangeExamples = testGroup "bitRange examples" $ hUnitTestToTests $ TestList $
    map bitRangeTest $
        ones ++
        [ (0x0000000200000000, 32, 48, 2)
        ]
  where
    bitRangeTest (word, lo, hi, expected) = TestCase $ do
        assertEqual
            (concat [ "bitRange ", show word, " ", show lo, " ", show hi
                    , " == "
                    , show expected
                    ])
            expected
            (bitRange word lo hi)
    ones = map (\bit ->  (1 `shiftL` bit, bit, bit + 1, 1)) [0..63]

replaceBitsExamples = testGroup "replaceBits" $ hUnitTestToTests $ TestList $
    map TestCase
        [ replaceTest 8 (0xf :: Word8) 0      0 0xf
        , replaceTest 8 (0x1 :: Word8) 0xf    0 0x1
        , replaceTest 8 (0x2 :: Word8) 0x1    0 0x2
        , replaceTest 8 (0x1 :: Word8) 0xf    0 0x1
        , replaceTest 8 (0x2 :: Word8) 0x10   4 0x20
        , replaceTest 8 (0x1 :: Word8) 0x10   8 0x0110
        , replaceTest 8 (0xa :: Word8) 0xffff 8 0x0aff
        ]
 where
    replaceTest :: (Bounded a, Integral a, Bits a, Show a)
        => Int -> a -> Word64 -> Int -> Word64 -> Assertion
    replaceTest len new orig shift expected =
        assertEqual (concat [ "replaceBits (", show new, " :: Word", show len, ") "
                            , show orig, " ", show shift
                            , " == "
                            , show expected
                            ])
                    expected
                    (replaceBits new orig shift)
