module Tests.Module.Capnp.Bits (bitsTests) where

import Data.Bits
import Data.Word

import Test.Framework (testGroup)
import Test.HUnit     (Assertion, assertEqual)
import Tests.Util     (assertionsToTest)

import Capnp.Bits


bitsTests = testGroup "bits tests" [bitRangeExamples, replaceBitsExamples]

bitRangeExamples = assertionsToTest "bitRange examples" $
    map bitRangeTest $
        ones ++
        [ (0x0000000200000000, 32, 48, 2)
        ]
  where
    bitRangeTest (word, lo, hi, expected) =
        assertEqual
            (concat [ "bitRange ", show word, " ", show lo, " ", show hi
                    , " == "
                    , show expected
                    ])
            expected
            (bitRange word lo hi)
    ones = map (\bit ->  (1 `shiftL` bit, bit, bit + 1, 1)) [0..63]

replaceBitsExamples = assertionsToTest "replaceBits"
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
        => Int -> a -> Word64 -> Int -> Word64 -> Assertion
    replaceTest len new orig shift expected =
        assertEqual (concat [ "replaceBits (", show new, " :: Word", show len, ") "
                            , show orig, " ", show shift
                            , " == "
                            , show expected
                            ])
                    expected
                    (replaceBits new orig shift)
