{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Module.Capnp.Rpc (rpcTests) where

import Control.Concurrent.STM
import Data.Word
import Test.Hspec

import Control.Concurrent     (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad          (replicateM)
import Control.Monad.Catch    (throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable          (for_)
import Data.Function          ((&))
import UnliftIO               (concurrently_, race_, timeout, try)

import qualified Data.Text as T

import Capnp (ConstMsg, createPure, def, msgToValue, valueToMsg)

import Capnp.Gen.Aircraft.Pure
import Capnp.Gen.Capnp.Rpc.Pure
import Capnp.Rpc

import qualified Capnp.Gen.Echo.Pure as E

rpcTests :: Spec
rpcTests = do
    echoTests
    aircraftTests
    unusualTests

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
    it "Should propogate server-side exceptions to client method calls" $ runVatPair
        (export_CallSequence ExnCtrServer)
        (expectException
            (callSequence'getNumber def)
            def
                { type_ = Exception'Type'failed
                , reason = "Something went sideways."
                }
        )
    it "Should receive unimplemented when calling a method on a null cap." $ runVatPair
        (pure $ CallSequence nullClient)
        (expectException
            (callSequence'getNumber def)
            def
                { type_ = Exception'Type'unimplemented
                , reason = "Client is null"
                }
        )
    it "Should throw an unimplemented exception if the server doesn't implement a method" $ runVatPair
        (export_CallSequence NoImplServer)
        (expectException
            (callSequence'getNumber def)
            def
                { type_ = Exception'Type'unimplemented
                , reason = "Method unimplemented"
                }
        )
    it "Should throw an opaque exception when the server throws a non-rpc exception" $ runVatPair
        (export_CallSequence NonRpcExnServer)
        (expectException
            (callSequence'getNumber def)
            def
                { type_ = Exception'Type'failed
                , reason = "Method threw an unhandled exception."
                }
        )
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
    it "Methods returning interfaces work" $ runVatPair
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
    it "Methods with interface parameters work" $ do
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
                liftIO $ r `shouldBe` [7, 5, 35]
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

-------------------------------------------------------------------------------
-- Implementations of various interfaces for testing purposes.
-------------------------------------------------------------------------------

data TestCtrFactory = TestCtrFactory

instance CounterFactory'server_ TestCtrFactory where
    counterFactory'newCounter CounterFactory'newCounter'params{start} TestCtrFactory = do
        ctr <- newTestCtr start >>= export_CallSequence
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

-- a 'CallSequence' which always throws an exception.
data ExnCtrServer = ExnCtrServer

instance CallSequence'server_ ExnCtrServer where
    callSequence'getNumber _ _ =
        throwM def
            { type_ = Exception'Type'failed
            , reason = "Something went sideways."
            }

-- a 'CallSequence' which doesn't implement its methods.
data NoImplServer = NoImplServer

instance CallSequence'server_ NoImplServer -- TODO: can we silence the warning somehow?

-- Server that throws some non-rpc exception.
data NonRpcExnServer = NonRpcExnServer

instance CallSequence'server_ NonRpcExnServer where
    callSequence'getNumber _ _ = error "OOPS"

-------------------------------------------------------------------------------
-- Tests for unusual patterns of messages .
--
-- Some of these will never come up when talking to a correct implementation of
-- capnproto, and others just won't come up when talking to Haskell
-- implementation. Accordingly, these tests start a vat in one thread and
-- directly manipulate the transport in the other.
-------------------------------------------------------------------------------

unusualTests :: Spec
unusualTests = describe "Tests for unusual message patterns" $ do
    it "Should raise ReceivedAbort in response to an abort message." $ do
        -- Send an abort message to the remote vat, and verify that
        -- the vat actually aborts.
        let exn = def
                { type_ = Exception'Type'failed
                , reason = "Testing abort"
                }
        (vatTrans, probeTrans) <- transportPair
        ret <- try $ concurrently_
            (runVat $ (vatConfig $ const vatTrans) { debugMode = True})
            $ do
                msg <- createPure maxBound $ valueToMsg $ Message'abort exn
                sendMsg probeTrans msg
        ret `shouldBe` Left (ReceivedAbort exn)
    triggerAbort (Message'unimplemented $ Message'abort def) $
        "Your vat sent an 'unimplemented' message for an abort message " <>
        "that its remote peer never sent. This is likely a bug in your " <>
        "capnproto library."


-- | Verify that the given message triggers an abort with the specified 'reason'
-- field.
triggerAbort :: Message -> T.Text -> Spec
triggerAbort msg reason =
    it ("Should abort when sent the message " ++ show msg ++ " on startup") $ do
        let wantAbortExn = def
                { reason = reason
                , type_ = Exception'Type'failed
                }
        (vatTrans, probeTrans) <- transportPair
        concurrently_
            (do
                ret <- try $ runVat $ (vatConfig $ const vatTrans) { debugMode = True }
                ret `shouldBe` Left (SentAbort wantAbortExn)
            )
            (do
                rawMsg <- createPure maxBound $ valueToMsg msg
                sendMsg probeTrans rawMsg
                -- 4 second timeout. The remote vat's timeout before killing the
                -- connection is one second, so if this happens we're never going
                -- to receive the message. In theory this is possible, but if it
                -- happens something is very wrong.
                r <- timeout 4000000 $ recvMsg probeTrans
                case r of
                    Nothing ->
                        error "Test timed out waiting on abort message."
                    Just rawResp -> do
                        resp <- msgToValue rawResp
                        resp `shouldBe` Message'abort wantAbortExn
            )

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

expectException call wantExn cap = do
    ret <- try $ cap & call
    case ret of
        Left (e :: Exception) -> do
            liftIO $ e `shouldBe` wantExn
            stopVat
        Right val ->
            error $ "Should have received exn, but got " ++ show val
