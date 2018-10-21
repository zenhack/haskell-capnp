module Tests.Module.Capnp.Rpc (rpcTests) where

import Test.Hspec

import Control.Concurrent     (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Capnp     (ConstMsg)
import Capnp.Rpc

-- | Make a pair of in-memory transports that are connected to one another. i.e,
-- messages sent on one are received on the other.
transportPair :: MonadIO m => m (Transport m, Transport m)
transportPair = liftIO $ do
    varA <- newEmptyMVar
    varB <- newEmptyMVar
    pure
        ( mVarTransport varA varB
        , mVarTransport varB varA
        )

-- | @'mVarTransport' sendVar recvVar@ is a 'Transport' which sends messages by
-- putting them into @sendVar@, and receives messages by taking them from
-- @recvVar@.
mVarTransport :: MonadIO m => MVar ConstMsg -> MVar ConstMsg -> Transport m
mVarTransport sendVar recvVar = Transport
    { sendMsg = liftIO . putMVar sendVar
    , recvMsg = liftIO (takeMVar recvVar)
    }

rpcTests :: Spec
rpcTests = pure ()
