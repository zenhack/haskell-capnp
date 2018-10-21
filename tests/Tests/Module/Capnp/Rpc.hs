{-# LANGUAGE OverloadedStrings #-}
module Tests.Module.Capnp.Rpc (rpcTests) where

import Test.Hspec

import Control.Concurrent       (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (race_)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.Function            ((&))

import Capnp     (ConstMsg, def)
import Capnp.Rpc

import Capnp.Gen.Echo.Pure

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
rpcTests = describe "Echo server & client" $
    it "Should echo back the same message." $ do
        -- XXX: this is copypasta from the examples dir. We should find
        -- a way to share more of the code, while keeping the example
        -- useful as documentation.
        (clientTrans, serverTrans) <- transportPair
        let runClient = runVat def
                { debugMode = True }
                clientTrans
                $ do
                    echoSrv <- Echo <$> bootstrap
                    let msgs =
                            [ def { query = "Hello #1" }
                            , def { query = "Hello #2" }
                            ]
                    rets <- traverse (\msg -> echoSrv & echo'echo msg) msgs
                    liftIO $ rets `shouldBe`
                        [ def { reply = "Hello #1" }
                        , def { reply = "Hello #2" }
                        ]
                    stopVat
            runServer = runVat def
                { debugMode = True
                , bootstrapServer = Just $ do
                    Echo client <- export_Echo TestEchoServer
                    pure client
                }
                serverTrans
                (pure ())
        race_ runServer runClient

data TestEchoServer = TestEchoServer

instance Echo'server_ TestEchoServer where
    echo'echo params TestEchoServer = pure def { reply = query params }
