{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Rpc.Unwrap (unwrapTests) where

import Capnp
  ( SomeServer (..),
    export,
    methodUnimplemented,
  )
import qualified Capnp.Gen.Aircraft as Aircraft
import qualified Capnp.Rpc as Rpc
import Data.Typeable (Typeable)
import qualified Data.Typeable as Typeable
import qualified Supervisors
import Test.Hspec

data OpaqueEcho = OpaqueEcho
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance SomeServer OpaqueEcho

instance Aircraft.Echo'server_ OpaqueEcho where
  echo'echo _ = methodUnimplemented

newtype TransparentEcho = TransparentEcho Int
  deriving (Show, Read, Eq, Ord, Bounded, Typeable)

instance SomeServer TransparentEcho where
  unwrap = Typeable.cast

instance Aircraft.Echo'server_ TransparentEcho where
  echo'echo _ = methodUnimplemented

unwrapTests :: Spec
unwrapTests = describe "Tests for client unwrapping" $ do
  it "Should return nothing for OpaqueEcho." $ do
    r :: Maybe OpaqueEcho <- exportAndUnwrap OpaqueEcho
    r `shouldBe` Nothing
  it "Should return nothing for the wrong type." $ do
    r :: Maybe () <- exportAndUnwrap (TransparentEcho 4)
    r `shouldBe` Nothing
  it "Should return the value for TransparentEcho." $ do
    r <- exportAndUnwrap (TransparentEcho 4)
    r `shouldBe` Just (TransparentEcho 4)

exportAndUnwrap :: (SomeServer a, Aircraft.Echo'server_ a, Typeable b) => a -> IO (Maybe b)
exportAndUnwrap srv = Supervisors.withSupervisor $ \sup -> do
  client <- export @Aircraft.Echo sup srv
  pure $ Rpc.unwrapServer client
