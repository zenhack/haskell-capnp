module Tests.MessageQuickCheck
    (msgEncodeDecodeQuickCheck)
    where

import qualified Data.ByteString as BS

import           Data.CapNProto.Blob                                 (BlobSlice)
import qualified Data.CapNProto.Message                              as M
import qualified Data.CapNProto.Untyped                              as Untyped

-- Testing framework imports
import Test.Framework                       (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- Schema generation imports
import Tests.MessageGeneration as MG
import Tests.Util

-- Schema validation imports
import Control.Monad.Catch as C
import Control.Monad.Quota as Q

-- QuickCheck properties

prop_messageValid :: MG.Message -> Property
prop_messageValid msg = ioProperty $ do
    return True

msgEncodeDecodeQuickCheck = testProperty "valid message QuickCheck"
    (prop_messageValid <$> MG.genMessage)
