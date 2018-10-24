module Module.Capnp.Pointer (ptrTests) where

import Data.Bits
import Data.Int
import Data.Word
import Test.Hspec
import Test.QuickCheck

import Capnp.Pointer

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

ptrTests = do
    ptrProps
    parsePtrExamples

ptrProps = describe "Pointer Properties" $ do
    it "Should satisfy: parseEltSpec . serializeEltSpec == id" $
        property $ \spec -> parseEltSpec (serializeEltSpec spec) == spec
    it "Should satisfy: parsePtr . serializePtr == id" $
        property $ \ptr ->
            case ptr of
                (Just (StructPtr 0 0 0)) -> True -- we skip this one, since it's
                                                 -- the same bits as a null, so this
                                                 -- shouldn't hold. TODO: the name
                                                 -- of this test is a bit misleading
                                                 -- because of this case; should fix
                                                 -- that.
                _                        -> parsePtr (serializePtr ptr) == ptr


parsePtrExamples = describe "parsePtr (examples)" $
    it "Should parse this example correctly" $
        parsePtr 0x0000000200000000 `shouldBe` (Just $ StructPtr 0 2 0)
