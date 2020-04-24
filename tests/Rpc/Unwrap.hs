{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Rpc.Unwrap (unwrapTests) where

import Test.Hspec

import qualified Data.Typeable as Typeable

import Data.Typeable (Typeable)

import qualified Capnp.Gen.Aircraft.Pure as Aircraft
import qualified Capnp.Rpc               as Rpc
import qualified Supervisors

data OpaqueEcho = OpaqueEcho
    deriving(Show, Read, Eq, Ord, Enum, Bounded)

instance Rpc.Server IO OpaqueEcho

instance Aircraft.Echo'server_ IO OpaqueEcho where
    echo'echo _ = Rpc.methodUnimplemented

newtype TransparentEcho = TransparentEcho Int
    deriving(Show, Read, Eq, Ord, Bounded, Typeable)

instance Rpc.Server IO TransparentEcho where
    unwrap = Typeable.cast

instance Aircraft.Echo'server_ IO TransparentEcho where
    echo'echo _ = Rpc.methodUnimplemented


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

exportAndUnwrap :: (Aircraft.Echo'server_ IO a, Typeable b) => a -> IO (Maybe b)
exportAndUnwrap srv = Supervisors.withSupervisor $ \sup -> do
    client <- Aircraft.export_Echo sup srv
    pure $ Rpc.unwrapServer client
