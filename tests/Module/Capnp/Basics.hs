{-# LANGUAGE ScopedTypeVariables #-}
module Module.Capnp.Basics (basicsTests) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.IO.Class    (liftIO)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.IO        (propertyIO)

import qualified Data.ByteString as BS
import qualified Data.Text       as T

import Capnp.Basics

import Capnp (cerialize, evalLimitT, newMessage)

import qualified Capnp.Untyped as U

basicsTests :: Spec
basicsTests =
    describe "textBuffer and textBytes agree" $
        it "Should return the same number of bytes" $
            property $ \(text :: T.Text) -> propertyIO $ evalLimitT maxBound $ do
                msg <- newMessage Nothing
                cerial <- cerialize msg text
                buf <- textBuffer cerial
                bytes <- textBytes cerial
                liftIO $ BS.length bytes `shouldBe` U.length buf
