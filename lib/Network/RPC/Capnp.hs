{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-|
Module: Network.RPC.Capnp
Description: Cap'N Proto RPC support.

This module implements the RPC protocol (two-party only). See also the protocol
specification:

<https://github.com/capnproto/capnproto/blob/master/c%2B%2B/src/capnp/rpc.capnp>
-}
module Network.RPC.Capnp
    ( RpcT
    , Client
    , Server(..)
    , VatConfig(..)
    , Transport(..)
    , handleTransport
    , socketTransport
    , runVat
    , bootstrap
    , call

    , Rpc.Exception(..)
    , Rpc.Exception'Type(..)

    , Promise
    , Fulfiller
    , newPromise
    , fulfill
    , isResolved
    , wait
    , waitIO

    , export

    , nullClient
    ) where

import Control.Concurrent.STM
import Data.Word

import Control.Concurrent.Async        (concurrently_)
import Control.Exception
    (Exception, SomeException, fromException, throwIO, try)
import Control.Monad                   (forever)
import Control.Monad.Catch             (MonadThrow(..))
import Control.Monad.IO.Class          (MonadIO, liftIO)
import Control.Monad.Primitive         (PrimMonad(..))
import Control.Monad.Reader            (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class       (MonadTrans(lift))
import Data.Binary.Get                 (Decoder(..), runGetIncremental)
import Data.Default                    (Default(..))
import Data.IORef                      (newIORef, readIORef, writeIORef)
import Data.Maybe                      (isJust)
import Network.Socket                  (Socket)
import Network.Socket.ByteString       (recv)
import System.IO                       (Handle)
import Text.ParserCombinators.ReadPrec (pfail)
import Text.Read                       (Lexeme(Ident), lexP, readPrec)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V

import Capnp.Capnp.Rpc.Pure hiding (Exception)

import Data.Capnp.Message      (getMsgBinary)
import Data.Capnp.Untyped.Pure (PtrType(PtrStruct), Struct)

import Data.Capnp (def, hGetValue, hPutValue, msgToValue, sPutValue)

import qualified Capnp.Capnp.Rpc.Pure as Rpc

instance Exception Rpc.Exception

-- These aliases are actually defined in the schema, but the schema compiler
-- doesn't expose them to the code generator plugin, so we re-define them
-- ourselves.
type QuestionId = Word32
type AnswerId = Word32
type ExportId = Word32
type ImportId = Word32

-- | A @'Transport' m@ handles transmitting RPC messages, inside of a monadic
-- context @m@.
data Transport m = Transport
    { sendMsg :: Message -> m ()
    , recvMsg :: m Message
    }

-- | @'handleTransport' limit handle@ creates a new transport which reads
-- and writes messages from/to @handle@. It uses @limit@ as the traversal
-- limit when reading messages and decoding.
handleTransport :: MonadIO m => Int -> Handle -> Transport m
handleTransport limit handle = Transport
    { sendMsg = liftIO . hPutValue handle
    , recvMsg = liftIO $ hGetValue handle limit
    }

-- | @'socketTransport' limit socket@ creates a new transport which reads
-- and writes messages to/from a socket. It uses @limit@ as the traversal
-- limit when reading messages and decoing. Note that when reading messages,
-- it may read past the end of the message due to buffering; if you attempt
-- to read data from the socket after the transport is no longer in use, it
-- is possible some additional data has been consumed beyond the last message
-- received.
socketTransport :: MonadIO m => Int -> Socket -> m (Transport m)
socketTransport limit socket = do
    bufferedData <- liftIO $ newIORef BS.empty
    let decodeMessage !limitRemaining bytes = \case
            Fail{} ->
                error "TODO: handle errors."
            Done excess _ msg -> do
                writeIORef bufferedData excess
                pure msg
            Partial push -> do
                bytes <- if BS.null bytes
                    then recv socket limitRemaining
                    else pure bytes
                decodeMessage
                    (limitRemaining - BS.length bytes)
                    BS.empty
                    (push (Just bytes))
    pure Transport
        { sendMsg = liftIO . sPutValue socket
        , recvMsg = liftIO $ do
            bytes <- readIORef bufferedData
            msg <- decodeMessage
                limit
                bytes
                (runGetIncremental $ getMsgBinary limit)
            msgToValue msg
        }


-- | Get a new exportId/questionId. The argument gets the pool to allocate from.
newId :: MonadIO m => (Vat -> TVar [Word32]) -> RpcT m Word32
newId pool = do
    vat <- RpcT ask
    liftIO $ atomically $ do
        oldIds <- readTVar (pool vat)
        case oldIds of
            [] ->
                retry
            (id:ids) -> do
                writeTVar (pool vat) ids
                pure id

-- | A "null" client, which throws unimplemented in response to all method
-- calls.
nullClient :: Client
nullClient = NullClient

-- | Allocate an unused export id.
newExportId :: MonadIO m => RpcT m ExportId
newExportId = newId exportIdPool

-- | Allocate an unused question id.
newQuestionId :: MonadIO m => RpcT m QuestionId
newQuestionId = newId questionIdPool

-- | 'RpcT' is a monad transformer that supports making capnproto rpc calls.
-- The underlying monad must be an instance of 'MonadIO'.
newtype RpcT m a = RpcT (ReaderT Vat m a)
    deriving(Functor, Applicative, Monad)

instance MonadTrans RpcT where
    lift = RpcT . lift

instance MonadIO m => MonadIO (RpcT m) where
    liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (RpcT m) where
    throwM = lift . throwM

instance PrimMonad m => PrimMonad (RpcT m) where
    type PrimState (RpcT m) = PrimState m
    primitive = lift . primitive

-- | A client for a capnproto capability. A client is used to call methods on a
-- remote object; see 'call'.
--
-- Note that client's 'Show' instance does not have a way of retaininig the
-- connection, so doing @'read' . 'show'@ will reseult in a disconnected
-- capability (except in the case of 'nullClient', which is the only capability
-- that can be represented statically)..
data Client
    = RemoteClient
        { target   :: MessageTarget
        , localVat :: Vat
        }
    | LocalClient
        { exportId    :: ExportId
        , localServer :: Server
        , localVat    :: Vat
        }
    | NullClient
    | DisconnectedClient

-- | A 'Server' contains functions for handling requests to an object. It
-- can be converted to a 'Client' and then shared via RPC.
data Server = Server
    { handleCall :: forall m. MonadIO m => Word64 -> Word16 -> Payload -> RpcT m (Promise Struct)
    -- ^ @'handleCall' interfaceId methodId params@ handles a method call.
    -- The method is as specified by interfaceId and methodId, with @params@
    -- being the argument to the method call. It returns a 'Promise' for the
    -- result.
    , handleStop :: forall m. MonadIO m => RpcT m ()
    -- ^ 'handleStop' is executed when the last reference to the object is
    -- dropped.
    }

instance Eq Client where
    RemoteClient{target=ta, localVat=va} == RemoteClient{target=tb, localVat=vb} =
        (ta, va) == (tb, vb)
    LocalClient{exportId=ea, localVat=va} == LocalClient{exportId=eb, localVat=vb} =
        (ea, va) == (eb, vb)
    NullClient == NullClient = True
    DisconnectedClient == DisconnectedClient = True
    _ == _ = False

instance Read Client where
    readPrec = lexP >>= \case
        Ident "nullClient" ->
            pure nullClient
        Ident "DisconnectedClient" ->
            -- TODO: figure out something else to put here.
            pure DisconnectedClient
        _ ->
            pfail

instance Show Client where
    show NullClient = "nullClient"
    -- TODO: we should put something here that makes sense given the exposed API:
    show _          = "DisconnectedClient"

-- | Export a local interface server, so it may be offered on the network.
export :: MonadIO m => Server -> RpcT m Client
export localServer = do
    exportId <- newExportId
    localVat <- RpcT ask
    pure LocalClient{exportId, localServer, localVat}

-- | Get a client for the bootstrap interface from the remote vat.
bootstrap :: MonadIO m => RpcT m Client
bootstrap = do
    questionId <- newQuestionId
    vat <- RpcT ask
    let msg = Message'bootstrap def { questionId }
    liftIO $ atomically $ sendQuestion vat (BootstrapQuestion questionId)
    pure RemoteClient
        { target = MessageTarget'promisedAnswer def { questionId }
        , localVat = vat
        }
-- | send a question to the remote vat. This inserts it into the local
-- vat's  questions table, in addition to actually sending the message.
sendQuestion :: Vat -> Question -> STM ()
sendQuestion Vat{sendQ,questions} question = do
    writeTBQueue sendQ $ getQuestionMessage question
    modifyTVar questions $ M.insert (getQuestionId question) question

-- | @'call' interfaceId methodId params client@ calls an RPC method
-- on @client@. The method is as specified by @interfaceId@ and
-- @methodId@. The return value is a promise for the result.
call :: MonadIO m => Word64 -> Word16 -> Payload -> Client -> RpcT m (Promise Struct)
call interfaceId methodId params RemoteClient{ target, localVat } = do
    questionId <- newQuestionId
    let callMsg = Call
            { sendResultsTo = Call'sendResultsTo'caller
            , allowThirdPartyTailCall = False
            , ..
            }
    (promise, fulfiller) <- liftIO $ atomically newPromise
    liftIO $ atomically $ sendQuestion localVat $ CallQuestion
        { callMsg
        , sendReturn = fulfiller
        }
    pure promise
call interfaceId methodId params LocalClient{localServer=Server{handleCall}} =
    handleCall interfaceId methodId params
call _ _ _ NullClient = alwaysThrow def
    { reason = "Client is null"
    , type_ = Exception'Type'unimplemented
    }
call _ _ _ DisconnectedClient = alwaysThrow def
    { reason = "Client is disconnected"
    , type_ = Exception'Type'disconnected
    }

-- | Return an already-broken promise, which raises the specified
-- exception.
alwaysThrow :: MonadIO m => Rpc.Exception -> RpcT m (Promise Struct)
alwaysThrow exn = do
    (promise, fulfiller) <- liftIO $ atomically newPromise
    liftIO $ atomically $ breakPromise fulfiller exn
    pure promise

-- | A 'Fulfiller' is used to fulfill a promise.
newtype Fulfiller a = Fulfiller
    { var :: TVar (Maybe (Either Rpc.Exception a))
    }

-- | Fulfill a promise by supplying the specified value. It is an error to
-- call 'fulfill' if the promise has already been fulfilled (or broken).
fulfill :: Fulfiller a -> a -> STM ()
fulfill Fulfiller{var} val = modifyTVar' var $ \case
    Nothing ->
        Just (Right val)
    Just _ ->
        -- TODO: report this in a more controlled way.
        error "BUG: tried to fullfill a promise twice!"

-- | Break a promise. When the user of the promise executes 'wait', the
-- specified exception will be raised. It is an error to call 'breakPromise'
-- if the promise has already been fulfilled (or broken).
breakPromise :: Fulfiller a -> Rpc.Exception -> STM ()
breakPromise Fulfiller{var} exn = modifyTVar' var $ \case
    Nothing ->
        Just (Left exn)
    Just _ ->
        error "BUG: tried to break an already resolved promise!"

-- | Wait for a promise to resolve, and return the result. If the promise
-- is broken, this raises an exception instead (see 'breakPromise').
wait :: Promise a -> STM a
wait Promise{var} = do
    val <- readTVar var
    case val of
        Nothing ->
            retry
        Just (Right result) ->
            pure result
        Just (Left exn) ->
            throwSTM exn

-- | Like 'wait', but in the 'IO' monad.
waitIO :: MonadIO m => Promise a -> m a
waitIO = liftIO . atomically . wait

-- | Check whether a promise is resolved.
isResolved :: Promise a -> STM Bool
isResolved Promise{var} = isJust <$> readTVar var

-- | Create a new promise and an associated fulfiller.
newPromise :: STM (Promise a, Fulfiller a)
newPromise = do
    var <- newTVar Nothing
    pure (Promise{var}, Fulfiller{var})

-- | A promise is a value that may not be ready yet.
newtype Promise a = Promise
    { var :: TVar (Maybe (Either Rpc.Exception a))
    }

-- | A 'Question' is an outstanding question message.
data Question
    = CallQuestion
        { callMsg    :: Call
        , sendReturn :: Fulfiller Struct
        }
    | BootstrapQuestion !QuestionId

newtype Answer
    = ServerAnswer Server

newtype Export
    = ExportServer Server

-- | Get a 'Message' corresponding to the question.
getQuestionMessage :: Question -> Message
getQuestionMessage CallQuestion{callMsg} = Message'call callMsg
getQuestionMessage (BootstrapQuestion questionId) =
    Message'bootstrap def { questionId }

-- | Get the question's id.
getQuestionId :: Question -> QuestionId
getQuestionId CallQuestion{callMsg=Call{questionId}} = questionId
getQuestionId (BootstrapQuestion questionId)         = questionId

-- | A 'Vat' is where objects live. It represents the state of one
-- side of a connection.
data Vat = Vat
    { questions       :: TVar (M.Map QuestionId Question)
    , answers         :: TVar (M.Map AnswerId Answer)
    , imports         :: TVar (M.Map ImportId CapDescriptor)
    , exports         :: TVar (M.Map ExportId Server)

    , questionIdPool  :: TVar [Word32]
    , exportIdPool    :: TVar [Word32]

    , bootstrapServer :: Maybe Server

    , sendQ           :: TBQueue Message
    , recvQ           :: TBQueue Message
    }

instance Eq Vat where
    -- it is sufficient to compare any of the TVars, since we create the whole
    -- vat as a unit:
    Vat{questions=qa} == Vat{questions=qb} = qa == qb

data VatConfig = VatConfig
    { maxQuestions    :: !Word32
    , maxExports      :: !Word32
    , bootstrapServer :: Maybe Server
    }

instance Default VatConfig where
    def = VatConfig
        { maxQuestions = 32
        , maxExports = 32
        , bootstrapServer = Nothing
        }

runRpcT :: Vat -> RpcT m a -> m a
runRpcT vat (RpcT m) = runReaderT m vat

runVat :: VatConfig -> Transport IO -> RpcT IO () -> IO ()
runVat config transport m = do
    vat <- newVat config
    foldl concurrently_
        (recvLoop transport vat)
        [ sendLoop transport vat
        , coordinator vat
        , runRpcT vat m
        ]

newVat :: VatConfig -> IO Vat
newVat VatConfig{..} = atomically $ do
    questions <- newTVar M.empty
    answers <- newTVar M.empty
    imports  <- newTVar M.empty
    exports <- newTVar M.empty

    questionIdPool <- newTVar [0..maxQuestions-1]
    exportIdPool <- newTVar [0..maxExports-1]

    sendQ <- newTBQueue $ fromIntegral maxQuestions
    recvQ <- newTBQueue $ fromIntegral maxQuestions

    pure Vat{..}

sendLoop :: Transport IO -> Vat -> IO ()
sendLoop transport Vat{sendQ} =
    forever $ atomically (readTBQueue sendQ) >>= sendMsg transport

recvLoop :: Transport IO -> Vat -> IO ()
recvLoop transport Vat{recvQ} =
    forever $ recvMsg transport >>= atomically . writeTBQueue recvQ

-- | Report the specified message to the remote vat as unimplemented.
replyUnimplemented :: Vat -> Message -> STM ()
replyUnimplemented Vat{sendQ} =
    writeTBQueue sendQ . Message'unimplemented

-- | The coordinator handles incoming messages, dispatching them as
-- method calls to local objects, forwarding return values to the right
-- place, etc.
coordinator :: Vat -> IO ()
coordinator vat@Vat{..} = forever $ do
    msg <- atomically $ readTBQueue recvQ
    case msg of
        Message'abort exn ->
            throwIO exn
        Message'return ret ->
            handleReturn vat ret
        Message'bootstrap bs ->
            handleBootstrap vat bs
        Message'call call ->
            handleCallMsg vat call
        _ ->
            atomically $ replyUnimplemented vat msg

-- | Handle a bootstrap message.
handleBootstrap :: Vat -> Bootstrap -> IO ()
handleBootstrap vat@Vat{..} msg@Bootstrap{questionId} =
    case bootstrapServer of
        Nothing ->
            atomically $ replyUnimplemented vat $ Message'bootstrap msg
        Just server -> atomically $
            modifyTVar' answers $ M.insert questionId (ServerAnswer server)
            -- TODO: also add it to exports and send a Return.

handleCallMsg :: Vat -> Call -> IO ()
-- TODO: can't call this handleCall because that's taken by the field in 'Server'.
-- rework things so we can be consistent.
handleCallMsg vat@Vat{..} msg@Call{target,interfaceId,methodId,params} =
    case target of
        MessageTarget'importedCap _ ->
            atomically $ replyUnimplemented vat $ Message'call msg
        MessageTarget'unknown' _ ->
            atomically $ replyUnimplemented vat $ Message'call msg
        MessageTarget'promisedAnswer PromisedAnswer{questionId, transform}
            | V.length transform /= 0 ->
                atomically $ replyUnimplemented vat $ Message'call msg
            | otherwise -> do
                result <- atomically $ M.lookup questionId <$> readTVar answers
                case result of
                    Nothing -> do
                        let exn = def
                                { reason = "Received 'Call' on non-existant promised answer #"
                                    <> T.pack (show questionId)
                                , type_ = Exception'Type'failed
                                }
                        atomically $ writeTBQueue sendQ $ Message'abort exn
                        -- TODO: adjust this so we don't throw (and thus kill the connection)
                        -- before the abort message is actually sent.
                        throwIO exn
                    Just (ServerAnswer Server{handleCall}) -> do
                        result <- try $ do
                            ret <- runRpcT vat $ handleCall interfaceId methodId params
                            liftIO $ waitIO ret
                        atomically $ writeTBQueue sendQ $ Message'return $ case result of
                            -- The server returned successfully; pass along the result.
                            Right ok -> def
                                { answerId = questionId
                                , union' = Return'results def
                                    { content = Just (PtrStruct ok)
                                    }
                                }
                            Left (e :: SomeException) -> def
                                -- The server threw an exception; we need to report this
                                -- to the caller.
                                { answerId = questionId
                                , union' = Return'exception $
                                    case fromException e of
                                        Just (e :: Rpc.Exception) ->
                                            -- If the exception was a capnp exception,
                                            -- just pass it along.
                                            e
                                        Nothing -> def
                                            -- Otherwise, return something opaque to
                                            -- the caller; the exception could potentially
                                            -- contain sensitive info.
                                            { reason = "unhandled exception"
                                            , type_ = Exception'Type'failed
                                            }
                                }

-- | Handle receiving a 'Return' message.
handleReturn :: Vat -> Return -> IO ()
handleReturn vat@Vat{..} msg@Return{..} = handleErrs $ do
    question <- M.lookup answerId <$> readTVar questions
    case question of
        Nothing ->
            abort def
                { reason = "Received 'Return' for non-existant question #"
                    <> T.pack (show answerId)
                , type_ = Exception'Type'failed
                }
        Just (BootstrapQuestion _) -> do
            -- This will case the other side to drop the resolved cap; we'll
            -- just keep using the promise.
            replyUnimplemented vat $ Message'return msg
            modifyTVar questions $ M.delete answerId
            ok
        Just CallQuestion{callMsg, sendReturn} -> do
            -- We don't support caps other than the bootstrap yet, so we can
            -- send Finish right away.
            writeTBQueue sendQ $ Message'finish def { questionId = answerId }
            modifyTVar questions $ M.delete answerId
            case union' of
                Return'results Payload{content} ->
                    handleResult sendReturn content
                Return'exception exn -> do
                    breakPromise sendReturn exn
                    ok
                _ ->
                    abort def
                        { reason = "Received unexpected return variant " <>
                            "(we only support results and exception)."
                        , type_ = Exception'Type'failed
                        }

  where
    -- | Run a transaction, which may *return* an error (as opposed to throwing
    -- one) via 'Left'; if it does, throw the error, otherwise return ().
    --
    -- The reason for this is so we can report errors while still commiting the
    -- transaction; if we did 'throwSTM' it would roll back the effects.
    handleErrs transaction = do
        ret <- atomically transaction
        case ret of
            Left exn ->
                throwIO exn
            Right () ->
                pure ()

    -- | Send an exception to the remote vat, and also return it (to be raised
    -- by handleErrs).
    abort exn = do
        writeTBQueue sendQ $ Message'abort exn
        pure $ Left exn
    -- | Shorthand to return from a transaction with no error.
    ok = pure $ Right ()

    -- | handle the @result@ variant of a 'Return'.
    handleResult sendReturn content = case content of
        Nothing -> do
            fulfill sendReturn def
            ok
        Just (PtrStruct s) -> do
            fulfill sendReturn s
            ok
        Just _ -> abort def
            -- TODO: I(zenhack) am not sure it's actually invalid to have
            -- capability in a return value, so maybe we should relax this
            -- a bit.
            { reason = "Received non-struct pointer in " <>
                "a return message."
            , type_ = Exception'Type'failed
            }
