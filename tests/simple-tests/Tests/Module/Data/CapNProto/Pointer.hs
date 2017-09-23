module Tests.Module.Data.CapNProto.Pointer where

import Data.Bits
import Data.CapNProto.Pointer
import Data.Int
import Data.Word

import Test.Framework                       (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit                           (assertEqual)
import Test.QuickCheck.Arbitrary            (Arbitrary, arbitrary)
import Test.QuickCheck.Gen                  (Gen, oneof)
import Tests.Util                           (assertionsToTest)

instance Arbitrary EltSpec where
    arbitrary = oneof [ EltNormal <$> arbitrary <*> arbitraryU29
                      , EltComposite <$> arbitraryI29
                      ]

instance Arbitrary ElementSize where
    arbitrary = oneof $ map return [ Sz0
                                   , Sz1
                                   , Sz8
                                   , Sz16
                                   , Sz32
                                   , Sz64
                                   , SzPtr
                                   ]


-- | arbitraryIN is an arbitrary N bit signed integer as an Int32.
arbitraryI32, arbitraryI30, arbitraryI29 :: Gen Int32
arbitraryI32 = arbitrary
arbitraryI30 = (`shiftR` 2) <$> arbitraryI32
arbitraryI29 = (`shiftR` 3) <$> arbitraryI32
-- | arbitraryUN is an arbitrary N bit unsigned integer as a Word32.
arbitraryU32, arbitraryU30, arbitraryU29 :: Gen Word32
arbitraryU32 = arbitrary
arbitraryU30 = (`shiftR` 2) <$> arbitraryU32
arbitraryU29 = (`shiftR` 3) <$> arbitraryU32


instance Arbitrary Ptr where
    arbitrary = oneof [ StructPtr <$> arbitraryI30
                                  <*> arbitrary
                                  <*> arbitrary
                      , ListPtr <$> arbitraryI30
                                <*> arbitrary
                      , FarPtr <$> arbitrary
                               <*> arbitraryU29
                               <*> arbitrary
                      , CapPtr <$> arbitrary
                      ]

ptrTests = testGroup "Pointer Tests" [ptrProps, parsePtrExamples]

ptrProps = testGroup "Pointer Properties"
    [ testProperty "parseEltSpec . serializeEltSpec == id"
        (\spec -> parseEltSpec (serializeEltSpec spec) == spec)
    , testProperty "parsePtr . serializePtr == id" $ \ptr ->
        case ptr of
            (Just (StructPtr 0 0 0)) -> True -- we skip this one, since it's
                                             -- the same bits as a null, so this
                                             -- shouldn't hold. TODO: the name
                                             -- of this test is a bit misleading
                                             -- because of this case; should fix
                                             -- that.
            _                        -> parsePtr (serializePtr ptr) == ptr
    ]


parsePtrExamples = assertionsToTest "parsePtr Examples" $
    map parseExample
        [ (0x0000000200000000, Just $ StructPtr 0 2 0)
        ]
  where
    parseExample (word, expected) =
        assertEqual
            (concat ["parsePtr ", show word, " == ", show expected])
            expected
            (parsePtr word)
