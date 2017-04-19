module Tests.Data.CapNProto.Bits where

import Data.Bits

import Data.CapNProto.Bits

import Test.HUnit (assertEqual, Test(TestCase, TestList))
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework (testGroup)


bitsTests = testGroup "bits tests" [bitRangeExamples]

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
