module Tests.Data.CapNProto.Untyped where

import Test.HUnit (assertEqual, Test(TestCase, TestList))
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import Tests.Util
import Control.Monad.Quota
import Data.CapNProto.Untyped

import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U

untypedTests = testGroup "Untyped Tests" $ hUnitTestToTests $ TestList $ map tst
    [ ( TestMessage { schemaName = "misc", typeName = "A", constName = "misc"}
      , 128
      , \(PtrStruct _) -> return ()
      , ((), Quota 127)
      )
    ]
  where
    tst (testMessage, quota, m, expected) = TestCase $ do
        msg <- getTestMessage testMessage quota
        actual <- runQuotaT (rootPtr msg >>= m) (Quota quota)
        assertEqual (show testMessage) actual expected
