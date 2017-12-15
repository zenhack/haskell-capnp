module Tests.MessageQuickCheck
    (msgEncodeDecodeQuickCheck)
    where

-- Testing framework imports
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- Schema generation imports
import Tests.MessageGeneration as MG

-- QuickCheck properties

prop_messageValid :: MG.Message -> Property
prop_messageValid msg = ioProperty $ do
    return True

msgEncodeDecodeQuickCheck = testProperty "valid message QuickCheck"
    (prop_messageValid <$> MG.genMessage)
