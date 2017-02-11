module Main where

import Data.Bits
import Data.Word
import Data.Int
import Data.Proxy (Proxy(Proxy))
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneof, Gen)
import Data.CapNProto.Pointer


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


main :: IO ()
main = defaultMain
    [ testProperty "parseEltSpec . serializeEltSpec == id"
        (\spec -> parseEltSpec (serializeEltSpec spec) == spec)
    , testProperty "parsePtr . serializePtr == id"
        (\ptr -> parsePtr (serializePtr ptr) == ptr)
    ]
