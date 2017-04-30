module Tests.Data.CapNProto.Untyped where

import Prelude hiding (length)

import Control.Monad (void)

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
      , \(Just (PtrStruct root)) -> do
            s <- get root
            words <- dataSection s
            2 <- length words
            72 <- get =<< index 0 words
            1 <- get =<< index 1 words
            ptrs <- ptrSection s
            0 <- length ptrs
            return ()
      , ((), Quota 125)
      )
    ]
  where
    tst (testMessage, quota, m, expected) = TestCase $ do
        msg <- getTestMessage testMessage quota
        actual <- runQuotaT (rootPtr msg >>= m) (Quota quota)
        assertEqual (show testMessage) actual expected
