{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-error=missing-methods #-}
module Module.Capnp.Rpc (rpcTests) where

import Control.Concurrent.STM
import Data.Word
import Test.Hspec

import Capnp.Mutability         (freeze)
import Control.Concurrent.Async (concurrently_, race_)
import Control.Exception.Safe   (bracket, try)
import Control.Monad            (replicateM, void, (>=>))
import Control.Monad.Catch      (throwM)
import Control.Monad.IO.Class   (liftIO)
import Data.Foldable            (for_)
import Data.Function            ((&))
import Data.Traversable         (for)
import System.Timeout           (timeout)

import qualified Data.ByteString.Builder as BB
import qualified Data.Text               as T
import qualified Network.Socket          as Socket
import qualified Supervisors

import Capnp.Bits       (WordCount)
import Capnp.New
    ( Client
    , IsCap
    , Parse(..)
    , Pipeline
    , SomeServer
    , TypeParam
    , Which
    , callP
    , callR
    , createPure
    , def
    , defaultLimit
    , evalLimitT
    , export
    , handleParsed
    , lbsToMsg
    , msgToParsed
    , parsedToMsg
    , waitPipeline
    )
import Capnp.Rpc.Errors (eFailed)

import Capnp.Gen.Aircraft.New  hiding (Left, Right)
import Capnp.Gen.Capnp.Rpc.New
import Capnp.Rpc               hiding (Client)
import Capnp.Rpc.Untyped       hiding (Client, Pipeline, export, waitPipeline)

import qualified Capnp.Gen.Echo.New as E
import qualified Capnp.Pointer2     as P

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
        (\sup -> export @E.Echo sup TestEchoServer)
        (\_sup echoSrv -> do
                let msgs =
                        [ def { E.query = "Hello #1" }
                        , def { E.query = "Hello #2" }
                        ]
                rets <- for msgs $ \arg -> echoSrv
                    & callP #echo arg
                    >>= waitPipeline
                    >>= evalLimitT defaultLimit . parse
                liftIO $ rets `shouldBe`
                    [ def { E.reply = "Hello #1" }
                    , def { E.reply = "Hello #2" }
                    ]
        )

data TestEchoServer = TestEchoServer

instance SomeServer TestEchoServer
instance E.Echo'server_ TestEchoServer where
    echo'echo _ = handleParsed $ \params -> pure def { E.reply = E.query params }

-------------------------------------------------------------------------------
-- Tests using aircraft.capnp.
--
-- These use the 'CallSequence' interface as a counter.
-------------------------------------------------------------------------------

-- | Bump a counter n times, returning a list of the results.
bumpN :: Client CallSequence -> Int -> IO [Parsed CallSequence'getNumber'results]
bumpN ctr n = bumpNPipeline ctr n
    >>= traverse waitPipeline
    >>= traverse (evalLimitT defaultLimit . parse)

-- | Like 'bumpN', but doesn't wait for the results -- returns a list of pipelines.
bumpNPipeline :: Client CallSequence -> Int -> IO [Pipeline CallSequence'getNumber'results]
bumpNPipeline ctr n = replicateM n (ctr & callR #getNumber def)

aircraftTests :: Spec
aircraftTests = describe "aircraft.capnp rpc tests" $ do
    describe "newPromiseClient" $
        it "Should preserve E-order" $
            Supervisors.withSupervisor $ \sup -> do
                (pc, f) <- newPromiseClient
                firsts <- bumpNPipeline pc 2
                atomically (newTestCtr 0)
                    >>= export @CallSequence sup
                    >>= fulfill f
                nexts <- bumpN pc 2
                firstsResolved <- for firsts $
                    waitPipeline >=> evalLimitT defaultLimit . parse
                firstsResolved `shouldBe`
                    [ def { n = 1 }
                    , def { n = 2 }
                    ]
                nexts `shouldBe`
                    [ def { n = 3 }
                    , def { n = 4 }
                    ]
    it "Should propogate server-side exceptions to client method calls" $ runVatPair
        (\sup -> export @CallSequence sup ExnCtrServer)
        (\_sup -> expectException
            (callR #getNumber def)
            def
                { type_ = Exception'Type'failed
                , reason = "Something went sideways."
                }
        )
    it "Should receive unimplemented when calling a method on a null cap." $ runVatPair
        (\_sup -> pure (fromClient nullClient :: Client CallSequence))
        (\_sup -> expectException
            (callR #getNumber def)
            def
                { type_ = Exception'Type'unimplemented
                , reason = "Method unimplemented"
                }
        )
    it "Should throw an unimplemented exception if the server doesn't implement a method" $ runVatPair
        (\sup -> export @CallSequence sup NoImplServer)
        (\_sup -> expectException
            (callR #getNumber def)
            def
                { type_ = Exception'Type'unimplemented
                , reason = "Method unimplemented"
                }
        )
    it "Should throw an opaque exception when the server throws a non-rpc exception" $ runVatPair
        (\sup -> export @CallSequence sup NonRpcExnServer)
        (\_sup -> expectException
            (callR #getNumber def)
            def
                { type_ = Exception'Type'failed
                , reason = "Unhandled exception"
                }
        )
    it "A counter should maintain state" $ runVatPair
        (\sup -> newTestCtr 0 >>= export @CallSequence sup)
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
        (\sup -> export @CounterFactory sup (TestCtrFactory sup))
        (\_sup factory -> do
            let newCounter start = do
                    CounterFactory'newCounter'results{counter} <-
                        factory & callP #newCounter def { start }
                        >>= waitPipeline
                        >>= evalLimitT defaultLimit . parse
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
            (\sup -> export @CounterAcceptor sup TestCtrAcceptor)
            (\sup acceptor -> do
                for_ [ctrA, ctrB, ctrC] $ \ctrSrv -> do
                    ctr <- atomically $ export @CallSequence sup ctrSrv
                    acceptor
                        & callP #accept CounterAcceptor'accept'params { counter = ctr }
                        >>= waitPipeline @CounterAcceptor'accept'results
                r <- traverse
                    (\(TestCtrServer var) -> liftIO $ readTVarIO var)
                    [ctrA, ctrB, ctrC]
                liftIO $ r `shouldBe` [7, 5, 35]
            )

data TestCtrAcceptor = TestCtrAcceptor

instance SomeServer TestCtrAcceptor
instance CounterAcceptor'server_ TestCtrAcceptor where
    counterAcceptor'accept _ =
        handleParsed $ \CounterAcceptor'accept'params{counter} -> do
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

instance SomeServer TestCtrFactory
instance CounterFactory'server_ TestCtrFactory where
    counterFactory'newCounter TestCtrFactory{sup} =
        handleParsed $ \CounterFactory'newCounter'params{start} -> do
            ctr <- atomically $ newTestCtr start >>= export @CallSequence sup
            pure CounterFactory'newCounter'results { counter = ctr }

newTestCtr :: Word32 -> STM TestCtrServer
newTestCtr n = TestCtrServer <$> newTVar n

newtype TestCtrServer = TestCtrServer (TVar Word32)

instance SomeServer TestCtrServer
instance CallSequence'server_ TestCtrServer  where
    callSequence'getNumber (TestCtrServer tvar) =
        handleParsed $ \_ -> do
            ret <- liftIO $ atomically $ do
                modifyTVar' tvar (+1)
                readTVar tvar
            pure def { n = ret }

-- a 'CallSequence' which always throws an exception.
data ExnCtrServer = ExnCtrServer

instance SomeServer ExnCtrServer
instance CallSequence'server_ ExnCtrServer where
    callSequence'getNumber _ = handleParsed $ \_ ->
        throwM def
            { type_ = Exception'Type'failed
            , reason = "Something went sideways."
            }

-- a 'CallSequence' which doesn't implement its methods.
data NoImplServer = NoImplServer

instance SomeServer NoImplServer
instance CallSequence'server_ NoImplServer -- TODO: can we silence the warning somehow?

-- Server that throws some non-rpc exception.
data NonRpcExnServer = NonRpcExnServer

instance SomeServer NonRpcExnServer
instance CallSequence'server_ NonRpcExnServer where
    callSequence'getNumber _ = handleParsed $ \_ -> error "OOPS"

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
                    msg <- createPure maxBound $ parsedToMsg $ Message $ Message'abort exn
                    sendMsg (probeTrans defaultLimit) msg
            ret `shouldBe` Left (ReceivedAbort exn)
    triggerAbort (Message'unimplemented $ Message $ Message'abort def) $
        "Your vat sent an 'unimplemented' message for an abort message " <>
        "that its remote peer never sent. This is likely a bug in your " <>
        "capnproto library."
    triggerAbort
        (Message'call (def
            { target = MessageTarget $ MessageTarget'importedCap 443
            } :: Parsed Call)
        )
        "No such export: 443"
    triggerAbort
        (Message'call (def
            { target = MessageTarget $
                MessageTarget'promisedAnswer (def { questionId=300 } :: Parsed PromisedAnswer)
            } :: Parsed Call)
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
                    resp <- evalLimitT maxBound $ msgToParsed msg'
                    resp `shouldBe` Message (Message'abort wantAbortExn)
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
                                let ctr :: Client CallSequence = fromClient client
                                in void $ (ctr & callR #getNumber def) >>= waitPipeline
                            }
                    e `shouldBe` SentAbort wantExn
                )
                (do
                    let send msg =
                            evalLimitT maxBound (parsedToMsg msg >>= freeze)
                            >>= sendMsg (probeTrans defaultLimit)
                        recv = recvMsg (probeTrans defaultLimit)
                                >>= evalLimitT maxBound . msgToParsed
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
                msg <- createPure maxBound $ parsedToMsg $ Message'join def
                sendMsg (probeTrans defaultLimit) msg
                msg' <- recvMsg (probeTrans defaultLimit)
                        >>= evalLimitT maxBound . msgToParsed
                msg' `shouldBe` Message (Message'unimplemented (Message (Message'join def)))


-- | Verify that the given message triggers an abort with the specified 'reason'
-- field.
triggerAbort :: Parsed (Which Message) -> T.Text -> Spec
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
                    rawMsg <- createPure maxBound $ parsedToMsg msg
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
                            resp <- evalLimitT maxBound $ msgToParsed rawResp
                            resp `shouldBe` Message (Message'abort wantAbortExn)
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

expectException
    :: (IsCap c, TypeParam a, Parse a pa, Show pa)
    => (Client c -> IO (Pipeline a)) -> Parsed Exception -> Client c -> IO ()
expectException callFn wantExn cap = do
    ret <- try $ callFn cap >>= waitPipeline
    case ret of
        Left (e :: Parsed Exception) ->
            liftIO $ e `shouldBe` wantExn
        Right val -> do
            parsed <- evalLimitT defaultLimit (parse val)
            error $ "Should have received exn, but got " ++ show parsed
