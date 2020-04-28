{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Module.Capnp.Rpc (rpcTests) where

import Control.Concurrent.STM
import Data.Word
import Test.Hspec

import Control.Concurrent.Async (concurrently_, race_)
import Control.Exception.Safe   (bracket, try)
import Control.Monad            (replicateM, void, (>=>))
import Control.Monad.Catch      (throwM)
import Control.Monad.IO.Class   (liftIO)
import Data.Foldable            (for_)
import Data.Mutable             (freeze)
import System.Timeout           (timeout)

import qualified Data.ByteString.Builder as BB
import qualified Data.Text               as T
import qualified Network.Socket          as Socket
import qualified Supervisors

import Capnp
    ( createPure
    , def
    , defaultLimit
    , evalLimitT
    , lbsToMsg
    , msgToValue
    , valueToMsg
    )
import Capnp.Bits       (WordCount)
import Capnp.Rpc.Errors (eFailed)

import Capnp.Gen.Aircraft.Pure  hiding (Left, Right)
import Capnp.Gen.Capnp.Rpc.Pure
import Capnp.Rpc
import Capnp.Rpc.Untyped

import qualified Capnp.Gen.Echo.Pure as E
import qualified Capnp.Pointer       as P

rpcTests :: Spec
rpcTests = do
    echoTests
    aircraftTests
    unusualTests

-------------------------------------------------------------------------------
-- Tests using echo.capnp.
-------------------------------------------------------------------------------

echoTests :: Spec
echoTests = describe "Echo server & client" $
    it "Should echo back the same message." $ runVatPair
        (`E.export_Echo` TestEchoServer)
        (\_sup echoSrv -> do
                let msgs =
                        [ def { E.query = "Hello #1" }
                        , def { E.query = "Hello #2" }
                        ]
                rets <- traverse ((E.echo'echo echoSrv ?) >=> wait) msgs
                liftIO $ rets `shouldBe`
                    [ def { E.reply = "Hello #1" }
                    , def { E.reply = "Hello #2" }
                    ]
        )

data TestEchoServer = TestEchoServer

instance Server IO TestEchoServer
instance E.Echo'server_ IO TestEchoServer where
    echo'echo = pureHandler $ \_ params -> pure def { E.reply = E.query params }

-------------------------------------------------------------------------------
-- Tests using aircraft.capnp.
--
-- These use the 'CallSequence' interface as a counter.
-------------------------------------------------------------------------------

-- | Bump a counter n times, returning a list of the results.
bumpN :: CallSequence -> Int -> IO [CallSequence'getNumber'results]
bumpN ctr n = bumpNPromise ctr n >>= traverse wait

-- | Like 'bumpN', but doesn't wait for the results -- returns a list of promises.
bumpNPromise :: CallSequence -> Int -> IO [Promise CallSequence'getNumber'results]
bumpNPromise ctr n = replicateM n (callSequence'getNumber ctr ? def)

aircraftTests :: Spec
aircraftTests = describe "aircraft.capnp rpc tests" $ do
    describe "newPromiseClient" $
        it "Should preserve E-order" $
            Supervisors.withSupervisor $ \sup -> do
                (pc, f) <- newPromiseClient
                firsts <- bumpNPromise pc 2
                atomically (newTestCtr 0)
                    >>= export_CallSequence sup
                    >>= fulfill f
                nexts <- bumpN pc 2
                firstsResolved <- traverse wait firsts
                firstsResolved `shouldBe`
                    [ def { n = 1 }
                    , def { n = 2 }
                    ]
                nexts `shouldBe`
                    [ def { n = 3 }
                    , def { n = 4 }
                    ]
    it "Should propogate server-side exceptions to client method calls" $ runVatPair
        (`export_CallSequence` ExnCtrServer)
        (\_sup -> expectException
            (\cap -> callSequence'getNumber cap ? def)
            def
                { type_ = Exception'Type'failed
                , reason = "Something went sideways."
                }
        )
    it "Should receive unimplemented when calling a method on a null cap." $ runVatPair
        (\_sup -> pure $ CallSequence nullClient)
        (\_sup -> expectException
            (\cap -> callSequence'getNumber cap ? def)
            def
                { type_ = Exception'Type'unimplemented
                , reason = "Method unimplemented"
                }
        )
    it "Should throw an unimplemented exception if the server doesn't implement a method" $ runVatPair
        (`export_CallSequence` NoImplServer)
        (\_sup -> expectException
            (\cap -> callSequence'getNumber cap ? def)
            def
                { type_ = Exception'Type'unimplemented
                , reason = "Method unimplemented"
                }
        )
    it "Should throw an opaque exception when the server throws a non-rpc exception" $ runVatPair
        (`export_CallSequence` NonRpcExnServer)
        (\_sup -> expectException
            (\cap -> callSequence'getNumber cap ? def)
            def
                { type_ = Exception'Type'failed
                , reason = "Unhandled exception"
                }
        )
    it "A counter should maintain state" $ runVatPair
        (\sup -> newTestCtr 0 >>= export_CallSequence sup)
        (\_sup ctr -> do
            results <- bumpN ctr 4
            liftIO $ results `shouldBe`
                [ def { n = 1 }
                , def { n = 2 }
                , def { n = 3 }
                , def { n = 4 }
                ]
        )
    it "Methods returning interfaces work" $ runVatPair
        (\sup -> export_CounterFactory sup (TestCtrFactory sup))
        (\_sup factory -> do
            let newCounter start = do
                    CounterFactory'newCounter'results{counter} <-
                        counterFactory'newCounter factory ? def { start }
                        >>= wait
                    pure counter

            ctrA <- newCounter 2
            ctrB <- newCounter 0

            r1 <- bumpN ctrA 4
            liftIO $ r1 `shouldBe`
                [ def { n = 3 }
                , def { n = 4 }
                , def { n = 5 }
                , def { n = 6 }
                ]

            r2 <- bumpN ctrB 2
            liftIO $ r2 `shouldBe`
                [ def { n = 1 }
                , def { n = 2 }
                ]

            ctrC <- newCounter 30

            r3 <- bumpN ctrA 3
            liftIO $ r3 `shouldBe`
                [ def { n = 7 }
                , def { n = 8 }
                , def { n = 9 }
                ]

            r4 <- bumpN ctrC 1
            liftIO $ r4 `shouldBe` [ def { n = 31 } ]
        )
    it "Methods with interface parameters work" $ do
        ctrA <- atomically $ newTestCtr 2
        ctrB <- atomically $ newTestCtr 0
        ctrC <- atomically $ newTestCtr 30
        runVatPair
            (`export_CounterAcceptor` TestCtrAcceptor)
            (\sup acceptor -> do
                for_ [ctrA, ctrB, ctrC] $ \ctrSrv -> do
                    ctr <- atomically $ export_CallSequence sup ctrSrv
                    counterAcceptor'accept acceptor ? CounterAcceptor'accept'params { counter = ctr }
                        >>= wait
                r <- traverse
                    (\(TestCtrServer var) -> liftIO $ readTVarIO var)
                    [ctrA, ctrB, ctrC]
                liftIO $ r `shouldBe` [7, 5, 35]
            )

data TestCtrAcceptor = TestCtrAcceptor

instance Server IO TestCtrAcceptor
instance CounterAcceptor'server_ IO TestCtrAcceptor where
    counterAcceptor'accept =
        pureHandler $ \_ CounterAcceptor'accept'params{counter} -> do
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

newtype TestCtrFactory = TestCtrFactory { sup :: Supervisor }

instance Server IO TestCtrFactory
instance CounterFactory'server_ IO TestCtrFactory where
    counterFactory'newCounter =
        pureHandler $ \TestCtrFactory{sup} CounterFactory'newCounter'params{start} -> do
            ctr <- atomically $ newTestCtr start >>= export_CallSequence sup
            pure CounterFactory'newCounter'results { counter = ctr }

newTestCtr :: Word32 -> STM TestCtrServer
newTestCtr n = TestCtrServer <$> newTVar n

newtype TestCtrServer = TestCtrServer (TVar Word32)

instance Server IO TestCtrServer
instance CallSequence'server_ IO TestCtrServer  where
    callSequence'getNumber = pureHandler $ \(TestCtrServer tvar) _ -> do
        ret <- liftIO $ atomically $ do
            modifyTVar' tvar (+1)
            readTVar tvar
        pure def { n = ret }

-- a 'CallSequence' which always throws an exception.
data ExnCtrServer = ExnCtrServer

instance Server IO ExnCtrServer
instance CallSequence'server_ IO ExnCtrServer where
    callSequence'getNumber = pureHandler $ \_ _ ->
        throwM def
            { type_ = Exception'Type'failed
            , reason = "Something went sideways."
            }

-- a 'CallSequence' which doesn't implement its methods.
data NoImplServer = NoImplServer

instance Server IO NoImplServer
instance CallSequence'server_ IO NoImplServer -- TODO: can we silence the warning somehow?

-- Server that throws some non-rpc exception.
data NonRpcExnServer = NonRpcExnServer

instance Server IO NonRpcExnServer
instance CallSequence'server_ IO NonRpcExnServer where
    callSequence'getNumber = pureHandler $ \_ _ -> error "OOPS"

-------------------------------------------------------------------------------
-- Tests for unusual patterns of messages .
--
-- Some of these will never come up when talking to a correct implementation of
-- capnproto, and others just won't come up when talking to the Haskell
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
        withTransportPair $ \(vatTrans, probeTrans) -> do
            ret <- try $ concurrently_
                (handleConn (vatTrans defaultLimit) def { debugMode = True})
                $ do
                    msg <- createPure maxBound $ valueToMsg $ Message'abort exn
                    sendMsg (probeTrans defaultLimit) msg
            ret `shouldBe` Left (ReceivedAbort exn)
    triggerAbort (Message'unimplemented $ Message'abort def) $
        "Your vat sent an 'unimplemented' message for an abort message " <>
        "that its remote peer never sent. This is likely a bug in your " <>
        "capnproto library."
    triggerAbort
        (Message'call def
            { target = MessageTarget'importedCap 443
            }
        )
        "No such export: 443"
    triggerAbort
        (Message'call def
            { target = MessageTarget'promisedAnswer def { questionId=300 }
            }
        )
        "No such answer: 300"
    triggerAbort
        (Message'return def { answerId = 234 })
        "No such question: 234"
    it "Should respond with an abort if sent junk data" $ do
        let wantAbortExn = def
                { reason = "Unhandled exception: TraversalLimitError"
                , type_ = Exception'Type'failed
                }
        withTransportPair $ \(vatTrans, probeTrans) ->
            concurrently_
                (do
                    Left (e :: RpcError) <- try $
                        handleConn (vatTrans defaultLimit) def { debugMode = True }
                    e `shouldBe` SentAbort wantAbortExn
                )
                (do
                    let bb = mconcat
                            [ BB.word32LE 0 -- 1 segment - 1 = 0
                            , BB.word32LE 2 -- 2 words in first segment
                            -- a pair of structs that point to each other:
                            , BB.word64LE (P.serializePtr (Just (P.StructPtr   0  0 1)))
                            , BB.word64LE (P.serializePtr (Just (P.StructPtr (-1) 0 1)))
                            ]
                        lbs = BB.toLazyByteString bb
                    msg <- lbsToMsg lbs
                    sendMsg (probeTrans defaultLimit) msg
                    msg' <- recvMsg (probeTrans defaultLimit)
                    resp <- msgToValue msg'
                    resp `shouldBe` Message'abort wantAbortExn
                )
    it "Should respond with an abort if erroneously sent return = resultsSentElsewhere" $

        withTransportPair $ \(vatTrans, probeTrans) ->
            let wantExn = eFailed $
                    "Received Return.resultsSentElswhere for a call "
                    <> "with sendResultsTo = caller."
            in concurrently_
                (do
                    Left (e :: RpcError) <- try $
                        handleConn (vatTrans defaultLimit) def
                            { debugMode = True
                            , withBootstrap = Just $ \_sup client ->
                                let ctr :: CallSequence = fromClient client
                                in void $ (callSequence'getNumber ctr ? def) >>= wait
                            }
                    e `shouldBe` SentAbort wantExn
                )
                (do
                    let send msg =
                            evalLimitT maxBound (valueToMsg msg >>= freeze)
                            >>= sendMsg (probeTrans defaultLimit)
                        recv = recvMsg (probeTrans defaultLimit) >>= msgToValue
                    Message'bootstrap Bootstrap{} <- recv
                    Message'call Call{questionId} <- recv
                    send $ Message'return def
                        { answerId = questionId
                        , union' = Return'resultsSentElsewhere
                        }
                    msg <- recv
                    msg `shouldBe` Message'abort wantExn
                )
    it "Should reply with unimplemented when sent a join (level 4 only)." $
        withTransportPair $ \(vatTrans, probeTrans) ->
        race_
            (handleConn (vatTrans defaultLimit) def { debugMode = True })
            $ do
                msg <- createPure maxBound $ valueToMsg $ Message'join def
                sendMsg (probeTrans defaultLimit) msg
                msg' <- recvMsg (probeTrans defaultLimit) >>= msgToValue
                msg' `shouldBe` Message'unimplemented (Message'join def)


-- | Verify that the given message triggers an abort with the specified 'reason'
-- field.
triggerAbort :: Message -> T.Text -> Spec
triggerAbort msg reason =
    it ("Should abort when sent the message " ++ show msg ++ " on startup") $ do
        let wantAbortExn = def
                { reason = reason
                , type_ = Exception'Type'failed
                }
        withTransportPair $ \(vatTrans, probeTrans) ->
            concurrently_
                (do
                    ret <- try $ handleConn (vatTrans defaultLimit) def { debugMode = True }
                    ret `shouldBe` Left (SentAbort wantAbortExn)
                )
                (do
                    rawMsg <- createPure maxBound $ valueToMsg msg
                    sendMsg (probeTrans defaultLimit) rawMsg
                    -- 4 second timeout. The remote vat's timeout before killing the
                    -- connection is one second, so if this happens we're never going
                    -- to receive the message. In theory this is possible, but if it
                    -- happens something is very wrong.
                    r <- timeout 4000000 $ recvMsg (probeTrans defaultLimit)
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

withSocketPair :: ((Socket.Socket, Socket.Socket) -> IO a) -> IO a
withSocketPair =
    bracket
        (Socket.socketPair Socket.AF_UNIX Socket.Stream 0)
        (\(x, y) -> Socket.close x >> Socket.close y)

withTransportPair ::
    ( ( WordCount -> Transport
      , WordCount -> Transport
      ) -> IO a
    ) -> IO a
withTransportPair f =
    withSocketPair $ \(x, y) -> f (socketTransport x, socketTransport y)

-- | @'runVatPair' server client@ runs a pair of vats connected to one another,
-- using 'server' as the 'offerBootstrap' field in the one vat's config, and
-- 'client' as the 'withBootstrap' field in the other's.
runVatPair :: IsClient c => (Supervisor -> STM c) -> (Supervisor -> c -> IO ()) -> IO ()
runVatPair getBootstrap withBootstrap = withTransportPair $ \(clientTrans, serverTrans) -> do
    let runClient = handleConn (clientTrans defaultLimit) def
            { debugMode = True
            , withBootstrap = Just $ \sup -> withBootstrap sup . fromClient
            }
        runServer = handleConn (serverTrans defaultLimit) def
            { debugMode = True
            , getBootstrap = fmap (Just . toClient) . getBootstrap
            }
    race_ runServer runClient

expectException :: Show a => (cap -> IO (Promise a)) -> Exception -> cap -> IO ()
expectException callFn wantExn cap = do
    ret <- try $ callFn cap >>= wait
    case ret of
        Left (e :: Exception) ->
            liftIO $ e `shouldBe` wantExn
        Right val ->
            error $ "Should have received exn, but got " ++ show val
