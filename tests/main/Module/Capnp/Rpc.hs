{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Module.Capnp.Rpc (rpcTests) where

import Control.Concurrent.STM
import Data.Word
import Test.Hspec

import Control.Concurrent       (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (race_)
import Control.Monad            (replicateM)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.Foldable            (for_)
import Data.Function            ((&))

import Capnp (ConstMsg, def)

import Capnp.Gen.Aircraft.Pure
import Capnp.Rpc

import qualified Capnp.Gen.Echo.Pure as E

rpcTests :: Spec
rpcTests = do
    echoTests
    aircraftTests

-------------------------------------------------------------------------------
-- Tests using echo.capnp. This is the schema used by the example.
-------------------------------------------------------------------------------

echoTests :: Spec
echoTests = describe "Echo server & client" $
    it "Should echo back the same message." $ runVatPair
        (E.export_Echo TestEchoServer)
        (\echoSrv -> do
                let msgs =
                        [ def { E.query = "Hello #1" }
                        , def { E.query = "Hello #2" }
                        ]
                rets <- traverse (\msg -> echoSrv & E.echo'echo msg) msgs
                liftIO $ rets `shouldBe`
                    [ def { E.reply = "Hello #1" }
                    , def { E.reply = "Hello #2" }
                    ]
                stopVat
        )

data TestEchoServer = TestEchoServer

instance E.Echo'server_ TestEchoServer where
    echo'echo params TestEchoServer = pure def { E.reply = E.query params }

-------------------------------------------------------------------------------
-- Tests using aircraft.capnp.
--
-- These use the 'CallSequence' interface as a counter.
-------------------------------------------------------------------------------

-- | Bump a counter n times, returning a list of the results.
bumpN :: CallSequence -> Int -> RpcT IO [CallSequence'getNumber'results]
bumpN ctr n = replicateM n $ ctr & callSequence'getNumber def

aircraftTests :: Spec
aircraftTests = describe "aircraft.capnp rpc tests" $ do
    it "A counter should maintain state" $ runVatPair
        (newTestCtr 0 >>= export_CallSequence)
        (\ctr -> do
            results <- replicateM 4 $
                ctr & callSequence'getNumber def
            liftIO $ results `shouldBe`
                [ def { n = 1 }
                , def { n = 2 }
                , def { n = 3 }
                , def { n = 4 }
                ]
            stopVat
        )
    xit "Methods returning interfaces work" $ runVatPair
        (export_CounterFactory TestCtrFactory)
        (\factory -> do
            let newCounter start = do
                    CounterFactory'newCounter'results{counter} <-
                        factory & counterFactory'newCounter def { start }
                    pure counter

            ctrA <- newCounter 2
            ctrB <- newCounter 0

            r <- bumpN ctrA 4
            liftIO $ r `shouldBe`
                [ def { n = 3 }
                , def { n = 4 }
                , def { n = 5 }
                , def { n = 6 }
                ]

            r <- bumpN ctrB 2
            liftIO $ r `shouldBe`
                [ def { n = 1 }
                , def { n = 2 }
                ]

            ctrC <- newCounter 30

            r <- bumpN ctrA 3
            liftIO $ r `shouldBe`
                [ def { n = 7 }
                , def { n = 8 }
                , def { n = 9 }
                ]

            r <- bumpN ctrC 1
            liftIO $ r `shouldBe` [ def { n = 31 } ]

            stopVat
        )
    xit "Methods with interface parameters work" $ do
        ctrA <- newTestCtr 2
        ctrB <- newTestCtr 0
        ctrC <- newTestCtr 30
        runVatPair
            (export_CounterAcceptor TestCtrAcceptor)
            (\acceptor -> do
                for_ [ctrA, ctrB, ctrC] $ \ctrSrv -> do
                    ctr <- export_CallSequence ctrSrv
                    acceptor & counterAcceptor'accept def { counter = ctr }
                r <- traverse
                    (\(TestCtrServer var) -> liftIO $ readTVarIO var)
                    [ctrA, ctrB, ctrC]
                liftIO $ r `shouldBe` [6, 4, 34]
                stopVat
            )

data TestCtrAcceptor = TestCtrAcceptor

instance CounterAcceptor'server_ TestCtrAcceptor where
    counterAcceptor'accept CounterAcceptor'accept'params{counter} TestCtrAcceptor = do
        [start] <- map n <$> bumpN counter 1
        r <- bumpN counter 4
        liftIO $ r `shouldBe`
            [ def { n = start + 1 }
            , def { n = start + 2 }
            , def { n = start + 3 }
            , def { n = start + 4 }
            ]
        pure def

data TestCtrFactory = TestCtrFactory

instance CounterFactory'server_ TestCtrFactory where
    counterFactory'newCounter _ TestCtrFactory = do
        ctr <- newTestCtr 0 >>= export_CallSequence
        pure CounterFactory'newCounter'results { counter = ctr }

newTestCtr :: MonadIO m => Word32 -> m TestCtrServer
newTestCtr n = liftIO $ TestCtrServer <$> newTVarIO n

newtype TestCtrServer = TestCtrServer (TVar Word32)

instance CallSequence'server_ TestCtrServer  where
    callSequence'getNumber _ (TestCtrServer tvar) = do
        ret <- liftIO $ atomically $ do
            modifyTVar' tvar (+1)
            readTVar tvar
        pure def { n = ret }

-------------------------------------------------------------------------------
-- Utilties used by the tests.
-------------------------------------------------------------------------------

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

-- | @'runVatPair' server client@ runs a pair of vats connected to one another,
-- using 'server' as the 'offerBootstrap' field in the one vat's config, and
-- 'client' as the 'withBootstrap' field in the other's.
runVatPair :: IsClient c => RpcT IO c -> (c -> RpcT IO ()) -> IO ()
runVatPair offerBootstrap withBootstrap = do
    (clientTrans, serverTrans) <- transportPair
    let runClient = runVat $ (vatConfig $ const clientTrans)
            { debugMode = True
            , withBootstrap = Just (withBootstrap . fromClient)
            }
        runServer = runVat $ (vatConfig $ const serverTrans)
            { debugMode = True
            , offerBootstrap = Just (toClient <$> offerBootstrap)
            }
    race_ runServer runClient
