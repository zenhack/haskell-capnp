{-# LANGUAGE QuasiQuotes #-}
module Tests.Data.CapNProto.Untyped where


import Text.Heredoc(here)
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
    [ ( [here|@0xaa6fda3b444d262c;
               struct A {
                  a @0 :UInt64;
                  b @1 :Bool;
               }
            |]
      , "A"
      , "( a = 72, b = true )"
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
    tst (schema, typename, value, quota, m, expected) = TestCase $ do
        let testMessage = TestMessage schema typename value
        msg <- getTestMessage testMessage quota
        actual <- runQuotaT (rootPtr msg >>= m) (Quota quota)
        assertEqual (show testMessage) actual expected
