{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-error=deprecations #-}
module Module.Capnp.Basics (basicsTests) where

import Prelude hiding (length)

import Data.Word
import Test.Hspec
import Test.QuickCheck

import Control.Monad.IO.Class    (liftIO)
import GHC.Prim                  (coerce)
import Test.QuickCheck.IO        (propertyIO)
import Test.QuickCheck.Instances ()

import qualified Data.ByteString as BS
import qualified Data.Text       as T

import Capnp.New.Basics

import Capnp.Mutability (freeze)
import Capnp.New
    (List, Mutability(..), Raw(..), encode, evalLimitT, length, newMessage)
-- import Data.Mutable (freeze)

basicsTests :: Spec
basicsTests =
    describe "textBuffer and textBytes agree" $
        it "Should return the same number of bytes" $
            property $ \(text :: T.Text) -> propertyIO $ evalLimitT maxBound $ do
                msg <- newMessage Nothing
                Raw untyped <- encode msg text
                raw :: Raw Text 'Const <- Raw <$> freeze untyped
                buf <- textBuffer raw
                bytes <- textBytes raw
                liftIO $ BS.length bytes `shouldBe` length (coerce buf :: Raw (List Word8) 'Const)
