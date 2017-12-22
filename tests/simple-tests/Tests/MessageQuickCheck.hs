module Tests.MessageQuickCheck
    (msgEncodeDecodeQuickCheck)
    where

-- Testing framework imports
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- Schema generation imports
import Tests.MessageGeneration as MG
import Tests.Util

-- QuickCheck properties

prop_messageValid :: MG.Message -> Property
prop_messageValid (MG.Message msgSchema msgType msgContent) = ioProperty $ do
    let meta = (MsgMetaData (show msgSchema) msgType)
    encoded <- capnpEncode (show msgContent) meta
    return True

msgEncodeDecodeQuickCheck = testProperty "valid message QuickCheck"
    (prop_messageValid <$> MG.genMessage)
