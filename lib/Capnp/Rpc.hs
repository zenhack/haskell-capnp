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
{-# LANGUAGE UndecidableInstances       #-}
{-|
Module: Capnp.Rpc
Description: Cap'N Proto RPC support.

This module implements the RPC protocol (two-party only). See also the protocol
specification:

<https://github.com/capnproto/capnproto/blob/master/c%2B%2B/src/capnp/rpc.capnp>
-}
module Capnp.Rpc
    ( RpcT

    -- * Making remote method calls
    , Client
    , call
    , bootstrap
    , nullClient
    , disconnectedClient

    , IsClient(..)

    -- * Handling method calls
    , Server(..)
    , export

    -- * Starting and stopping connections
    , VatConfig(..)
    , vatConfig
    , runVat
    , stopVat

    -- ** Transmitting messages
    , module Capnp.Rpc.Transport

    -- * Exceptions
    , RpcError(..)

    , methodUnimplemented
    , throwMethodUnimplemented
    , throwFailed

    -- ** Re-exported from the generated rpc.capnp module.
    , Exception(..)
    , Exception'Type(..)

    -- * Promises

    , module Capnp.Promise
    ) where

import Prelude hiding (fail)

import Data.Word
import UnliftIO  hiding (Exception, wait)

import Control.Concurrent              (threadDelay)
import Control.Concurrent.STM.TSem     (TSem, newTSem, signalTSem, waitTSem)
import Control.Monad                   (forever)
import Control.Monad.Catch             (MonadThrow(..))
import Control.Monad.Fail              (MonadFail(..))
import Control.Monad.IO.Class          (MonadIO, liftIO)
import Control.Monad.Primitive         (PrimMonad(..))
import Control.Monad.Reader            (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class       (MonadTrans(lift))
import Data.Foldable                   (for_, traverse_)
import Text.ParserCombinators.ReadPrec (pfail)
import Text.Read                       (Lexeme(Ident), lexP, readPrec)

import qualified Data.Map.Strict    as M
import qualified Data.Text          as T
import qualified Data.Vector        as V
import qualified UnliftIO.Exception as HsExn

import Capnp.Gen.Capnp.Rpc.Pure
import Capnp.Promise
import Capnp.Rpc.Transport

import Capnp
    ( ConstMsg
    , createPure
    , decerialize
    , def
    , defaultLimit
    , evalLimitT
    , msgToValue
    , valueToMsg
    )
import Capnp.Bits         (WordCount)
import Capnp.Untyped.Pure (PtrType(PtrCap, PtrStruct), Struct(..), sliceIndex)
import Supervisors        (Supervisor, superviseSTM, withSupervisor)

import qualified Capnp.Gen.Capnp.Rpc as Rpc
import qualified Capnp.Message       as Message
import qualified Capnp.Untyped       as Untyped

-- | Types which may converted to and from 'Client's.
class IsClient a where
    -- | Convert a typed capability to a client.
    toClient :: a -> Client

    -- | Convert an (untyped) client to a typed capability.
    fromClient :: Client -> a

instance IsClient Client where
    toClient c = c
    fromClient c = c

-- | Shortcut to throw an @unimplemented@ exception.
throwMethodUnimplemented :: MonadThrow m => m a
throwMethodUnimplemented = throwM methodUnimplemented

-- | Throw an exception with a type field of 'Exception'Type'failed' and
-- the argument as a reason.
throwFailed :: MonadThrow m => T.Text -> m a
throwFailed reason = throwM (def
    { reason = reason
    , type_ = Exception'Type'failed
    } :: Exception)

methodUnimplemented :: Exception
methodUnimplemented = Exception
    { reason = "Method unimplemented"
    , type_ = Exception'Type'unimplemented
    , obsoleteIsCallersFault = False
    , obsoleteDurability = 0
    }

-- These aliases are actually defined in the schema, but the schema compiler
-- doesn't expose them to the code generator plugin, so we re-define them
-- ourselves.
type QuestionId = Word32
type AnswerId = Word32
type ExportId = Word32
type ImportId = Word32
type EmbargoId = Word32

-- | Get a new exportId/questionId. The first argument gets the pool to
-- allocate from.
newId :: (Vat -> TVar [Word32]) -> Vat -> STM Word32
newId pool vat = do
    oldIds <- readTVar (pool vat)
    case oldIds of
        [] ->
            retrySTM
        (id:ids) -> do
            writeTVar (pool vat) ids
            pure id

-- | A "null" client, which throws unimplemented in response to all method
-- calls.
nullClient :: Client
nullClient = NullClient

-- | A client which is disconnected; throws disconnected in response to all
-- method calls.
disconnectedClient :: Client
disconnectedClient = ExnClient def
    { reason = "Client is disconnected"
    , type_ = Exception'Type'disconnected
    }

-- | Allocate an unused export id.
newExportId :: Vat -> STM Word32
newExportId = newId exportIdPool

-- | Allocate an unused question id.
newQuestionId :: Vat -> STM Word32
newQuestionId = newId questionIdPool

-- | 'RpcT' is a monad transformer that supports making capnproto rpc calls.
-- The underlying monad must be an instance of 'MonadIO'.
newtype RpcT m a = RpcT (ReaderT Vat m a)
    deriving(Functor, Applicative, Monad)

instance MonadTrans RpcT where
    lift = RpcT . lift

instance MonadIO m => MonadIO (RpcT m) where
    liftIO = lift . liftIO

instance MonadUnliftIO m => MonadUnliftIO (RpcT m) where
    askUnliftIO = do
        u <- lift askUnliftIO
        vat <- RpcT ask
        pure $ UnliftIO $ unliftIO u . runRpcT vat

instance MonadThrow m => MonadThrow (RpcT m) where
    throwM = lift . throwM

instance MonadFail m => MonadFail (RpcT m) where
    fail = lift . fail

instance PrimMonad m => PrimMonad (RpcT m) where
    type PrimState (RpcT m) = PrimState m
    primitive = lift . primitive

-- | A client for a capnproto capability. A client is used to call methods on a
-- remote object; see 'call'.
--
-- Note that 'Client''s 'Show' instance does not have a way of retaininig the
-- connection, so doing @'read' . 'show'@ will reseult in a disconnected
-- capability (except in the case of 'nullClient', which is the only capability
-- that can be represented statically).
data Client
    = RefClient
        { refClient :: RefClient
        , localVat  :: Vat
        }
    | NullClient
    | ExnClient Exception
    deriving(Eq)

data RefClient
    = QuestionClient
        { target   :: PromisedAnswer
        }
    | ImportClient
        { importId :: !Word32
        }
    | ExportClient
        { exportId     :: !ExportId
        , promiseState :: Maybe (TVar ExportPromiseState)
        , serverQueue  :: ServerQueue
        }
    deriving(Eq)

type ServerQueue = TQueue ServerOp

data ServerOp
    = ServerCall Word64 Word16 (Maybe (Untyped.Ptr ConstMsg)) (Fulfiller Struct)
    | ServerStop

-- | A 'Server' contains functions for handling requests to an object. It
-- can be converted to a 'Client' using 'export' and then shared via RPC.
data Server = Server
    { handleCall :: Word64 -> Word16 -> Maybe (Untyped.Ptr ConstMsg) -> Fulfiller Struct -> RpcT IO ()
    -- ^ @handleCall interfaceId methodId params@ handles a method call.
    -- The method is as specified by interfaceId and methodId, with @params@
    -- being the argument to the method call. It returns a 'Promise' for the
    -- result.
    , handleStop :: RpcT IO ()
    -- ^ handleStop is executed when the last reference to the object is
    -- dropped.
    }

-- | 'StopVat' is an exception used to terminate a capnproto connection; it is
-- raised by `stopVat`.
data StopVat = StopVat deriving(Show)
instance HsExn.Exception StopVat

-- | An eror raised by the RPC system.
data RpcError
    = ReceivedAbort Exception
    -- ^ We received an abort message from the remote vat.
    | SentAbort Exception
    -- ^ We sent an abort message to the remote vat; this is thrown after
    -- sending the abort.
    deriving(Show, Eq)
instance HsExn.Exception RpcError

-- | Shut down the rpc connection, and all resources managed by the vat. This
-- does not return (it raises an exception used to actually signal termination
-- of the connection).
stopVat :: MonadIO m => RpcT m ()
stopVat = liftIO (throwIO StopVat)

instance Read Client where
    readPrec = lexP >>= \case
        Ident "nullClient" ->
            pure nullClient
        Ident "disconnectedClient" ->
            pure disconnectedClient
        _ ->
            pfail

instance Show Client where
    show NullClient = "nullClient"
    show _          = "disconnectedClient"

-- | Create a new client that wraps a promise. The returned 'Fulfiller' can be
-- used to resolve the promise.
--
-- The promise client buffers calls until the promise is resolved, and then
-- forwards them (and any future calls) to the final client.
newPromiseClient :: (IsClient c, MonadUnliftIO m) => RpcT m (c, Fulfiller c)
newPromiseClient = do
    vat@Vat{exports, supervisor} <- RpcT ask
    atomically $ do
        (promise, fulfiller) <- newPromise
        queue <- newTQueue
        exportId <- newExportId vat
        pStateVar <- newTVar Unresolved
        modifyTVar' exports $ M.insert exportId Export
            { serverQueue = queue
            , promiseState = Just pStateVar
            , refCount = 0
            }
        superviseSTM supervisor $ runRpcT vat $ runServer
            vat
            Server
                { handleCall = \interfaceId methodId paramContents methodFulfiller -> do
                    client <- try (waitIO promise) >>= \case
                        Left  e -> pure $ ExnClient e
                        Right c -> pure $ toClient  c
                    -- TODO: deal with embargos, send resolve messages, update
                    -- the promiseState in the exports table, etc.
                    call client interfaceId methodId paramContents methodFulfiller
                , handleStop = pure ()
                }
            queue
        let retClient = RefClient
                { localVat = vat
                , refClient = ExportClient
                    { exportId
                    , promiseState = Just pStateVar
                    , serverQueue = queue
                    }
                }
        pure (fromClient retClient, fulfiller)

-- | Export a local interface server, so it may be offered on the network.
export :: MonadUnliftIO m => Server -> RpcT m Client
export localServer = do
    localVat@Vat{exports, supervisor} <- RpcT ask

    -- We add an entry to our exports table. The refcount *starts* at zero,
    -- and will be incremented to one the first time we actually send this
    -- capability to the remote vat. Because the refcount checking happens
    -- in response to interaction with the remote vat, it won't be removed
    -- until it goes *back* to being zero.
    atomically $ do
        queue <- newTQueue
        exportId <- newExportId localVat
        modifyTVar' exports $ M.insert exportId Export
            { serverQueue = queue
            , promiseState = Nothing
            , refCount = 0
            }
        superviseSTM supervisor $
            runRpcT localVat $ runServer localVat localServer queue
        pure RefClient
            { refClient = ExportClient
                { exportId
                , promiseState = Nothing
                , serverQueue = queue
                }
            , localVat
            }

runServer :: Vat -> Server -> TQueue ServerOp -> RpcT IO ()
runServer Vat{availLocalCalls} Server{handleCall} queue = go where
  go = do
    op <- atomically $ readTQueue queue
    case op of
        ServerStop ->
            pure ()

        ServerCall interfaceId methodId params fulfiller -> do
            handleCall interfaceId methodId params fulfiller
                `finally`
                atomically (signalTSem availLocalCalls)
            go

-- | Get a client for the bootstrap interface from the remote vat.
bootstrap :: MonadIO m => RpcT m Client
bootstrap = do
    vat <- RpcT ask
    atomically $ do
        questionId <- newQuestionId vat
        sendQuestion vat (BootstrapQuestion questionId)
        pure RefClient
            { refClient = QuestionClient
                { target = PromisedAnswer { questionId, transform = V.empty }
                }
            , localVat = vat
            }

-- | send a question to the remote vat. This updates the local vat's
-- tables as needed, in addition to actually sending the message.
sendQuestion :: Vat -> Question -> STM ()
sendQuestion vat@Vat{sendQ,questions,limit} question = do
    case question of
        CallQuestion{callMsg=Call{params=Payload{capTable}}} ->
            traverse_ (updateSendWithCap vat) capTable
        BootstrapQuestion _ ->
            pure ()
    writeTBQueue sendQ =<< createPure limit
        (valueToMsg $ getQuestionMessage question)
    modifyTVar' questions $ M.insert (getQuestionId question) question

-- | 'updateSendWithCap' updates the vat's tables as needed, assuming the CapDescriptor
-- is being sent to the remote vat as part of a call or return.
updateSendWithCap :: Vat -> CapDescriptor -> STM ()
updateSendWithCap Vat{exports} = \case
    CapDescriptor'none -> pure ()

    CapDescriptor'senderHosted exportId -> incrExport exportId
    CapDescriptor'senderPromise exportId -> incrExport exportId

    -- TODO: I(zenhack) don't *think* we need to do anything in these cases,
    -- but I need to think about it more carefully.
    CapDescriptor'receiverHosted _ -> pure ()
    CapDescriptor'receiverAnswer _ -> pure ()

    CapDescriptor'thirdPartyHosted cap ->
        error $
            "BUG: we tried to send a third party cap descriptor. This is weird; " ++
            "we don't support level 3 yet. " ++ show cap
    CapDescriptor'unknown' tag ->
        error $
            "BUG: we tried to send a cap descriptor with a tag we don't know about: " ++
            show tag
  where
    -- | Increment the reference count for the specified export id.
    incrExport :: Word32 -> STM ()
    incrExport =
        modifyTVar' exports . M.adjust
            (\e@Export{refCount} -> e { refCount = refCount + 1 })


-- | @'call' client interfaceId methodId params fulfiller@ calls an RPC method
-- on @client@. The method is as specified by @interfaceId@ and @methodId@.
-- when the call finishes, its result will be communicated via @fulfiller@.
call :: Client -> Word64 -> Word16 -> Maybe (Untyped.Ptr ConstMsg) -> Fulfiller Struct -> RpcT IO ()
call client interfaceId methodId paramContent fulfiller = atomically (go client) where
    go RefClient{localVat, refClient=QuestionClient { target }} =
        callRemote interfaceId methodId paramContent (MessageTarget'promisedAnswer target) localVat fulfiller
    go RefClient{localVat, refClient=ImportClient { importId }} =
        callRemote interfaceId methodId paramContent (MessageTarget'importedCap importId) localVat fulfiller
    go RefClient{refClient=ExportClient{serverQueue},localVat=Vat{availLocalCalls}} = do
        waitTSem availLocalCalls
        writeTQueue serverQueue $ ServerCall interfaceId methodId paramContent fulfiller
    go NullClient = breakPromise fulfiller def
        { reason = "Client is null"
        , type_ = Exception'Type'unimplemented
        }
    go (ExnClient e) = breakPromise fulfiller e

-- helper for call; handles remote cases.
callRemote :: Word64 -> Word16 -> Maybe (Untyped.Ptr ConstMsg) -> MessageTarget -> Vat -> Fulfiller Struct -> STM ()
callRemote interfaceId methodId paramContent target localVat@Vat{limit} fulfiller = do
    let capTable = maybe
            V.empty
            (V.map makeCapDescriptor . Message.getCapTable . Untyped.message)
            paramContent
    paramContent <- evalLimitT limit (decerialize paramContent)
    questionId <- newQuestionId localVat
    let callMsg = Call
            { sendResultsTo = Call'sendResultsTo'caller
            , allowThirdPartyTailCall = False
            , params = def
                { content = paramContent
                , capTable = capTable
                }
            , ..
            }
    sendQuestion localVat $ CallQuestion
        { callMsg
        , sendReturn = fulfiller
        }

-- | A 'Question' is an outstanding question message.
data Question
    = CallQuestion
        { callMsg    :: Call
        , sendReturn :: Fulfiller Struct
        }
    | BootstrapQuestion !QuestionId

data Answer
    = ClientAnswer Client
    -- ^ An answer which has already resolved to a capability
    | PromiseAnswer
        { promise   :: Promise Struct
        , transform :: V.Vector PromisedAnswer'Op
        }
    -- ^ An answer which will be fulfilled by a promise

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
    -- The four tables:
    { questions       :: TVar (M.Map QuestionId Question)
    , answers         :: TVar (M.Map AnswerId Answer)
    , imports         :: TVar (M.Map ImportId Word32) -- values are the refcount
    , exports         :: TVar (M.Map ExportId Export)

    -- Pools of available IDs for questions and exported objects.
    , questionIdPool  :: TVar [Word32]
    , exportIdPool    :: TVar [Word32]

    -- Semaphore for limiting the size of our answers table; this is to guard
    -- against resource usage attacks.
    , availAnswers    :: TSem

    -- Like availAnswers, but this limits the number of questions that can be
    -- pending on objects hosted by this vat. This is subtly different, in that
    -- it covers the case where an attacker has attempted to cause resource
    -- exhaustion by triggering a larger number of intra-vat calls on our end.
    --
    -- TODO: is availAnswers still necessary? I(zenhack) need to think about
    -- the implications.
    , availLocalCalls :: TSem

    -- Create the bootstrap interface for a connection. If 'Nothing', bootstrap
    -- messages return unimplemented.
    , offerBootstrap  :: Maybe (RpcT IO Client)

    -- queues on which to send and receive messages.
    , sendQ           :: TBQueue ConstMsg
    , recvQ           :: TBQueue ConstMsg

    -- Supervisor which monitors object lifetimes.
    , supervisor      :: Supervisor

    -- same as the corresponding fields in 'VatConfig'
    , debugMode       :: !Bool
    , limit           :: !WordCount

    }

data ExportPromiseState
    = Unresolved
    | Embargoed !EmbargoId
    | Ready ServerQueue

data Export = Export
    { serverQueue  :: ServerQueue
    , promiseState :: Maybe (TVar ExportPromiseState)
    , refCount     :: !Word32
    }

-- | @'makeCapDescriptor' client@ creates a cap descriptor suitable
-- for sending to a remote vat to describe the specified @client@.
makeCapDescriptor :: Client -> CapDescriptor
makeCapDescriptor NullClient    = CapDescriptor'none
makeCapDescriptor (ExnClient _) = CapDescriptor'none
makeCapDescriptor RefClient{refClient=ImportClient{importId}} =
    CapDescriptor'receiverHosted importId
makeCapDescriptor RefClient{refClient=QuestionClient{target}} =
    CapDescriptor'receiverAnswer target
makeCapDescriptor RefClient{refClient=ExportClient{exportId, promiseState=Nothing}} =
    CapDescriptor'senderHosted exportId
makeCapDescriptor RefClient{refClient=ExportClient{exportId, promiseState=Just _}} =
    CapDescriptor'senderPromise exportId

-- | Convert a 'CapDescriptor' from an incoming message into a Client, updating
-- the local vat's table if needed.
--
-- aborts the connection (and raises SentAbort) if the cap descriptor is invalid.
interpCapDescriptor :: Vat -> CapDescriptor -> IO Client
interpCapDescriptor vat@Vat{..} = \case
    CapDescriptor'none -> pure NullClient

    -- We treat senderPromise the same as senderHosted. We respond to resolve
    -- messages with unimplemented and keep using the promise; someday we'll
    -- want to actually observe resolution.
    --
    -- This is the same workaround used by the Go implementation; see
    -- https://github.com/capnproto/go-capnproto2/issues/2#issuecomment-221664672
    CapDescriptor'senderHosted  importId -> getImportClient importId
    CapDescriptor'senderPromise importId -> getImportClient importId

    CapDescriptor'receiverHosted exportId -> (throwLeft =<<) $ atomically $ do
        exports <- readTVar exports
        case M.lookup exportId exports of
            Nothing ->
                abort vat $
                    "Incoming capability table referenced non-existent " <>
                    "receiverHosted capability #" <> T.pack (show exportId)
            Just Export{serverQueue, promiseState} ->
                ok $ pure $ RefClient
                    { refClient = ExportClient
                        { serverQueue
                        , promiseState
                        , exportId = exportId
                        }
                    , localVat = vat
                    }
    CapDescriptor'receiverAnswer PromisedAnswer{questionId} -> (throwLeft =<<) $ atomically $ do
        answer <- M.lookup questionId <$> readTVar answers
        case answer of
            Nothing ->
                abort vat $
                    "Incoming capability table referenced non-existent " <>
                    "answer #" <> T.pack (show questionId)
            Just (ClientAnswer client) ->
                ok $ pure client
            Just _ ->
                abort vat "TODO: support other answer types"
    CapDescriptor'thirdPartyHosted _ -> abortIO vat "thirdPartyHosted not supported"
    CapDescriptor'unknown' tag ->
        abortIO vat $ T.pack $ "unknown cap descriptor variant #" ++ show tag
  where
    -- create a client based on an import id. This increments the refcount for
    -- that import.
    getImportClient importId = atomically $ do
        -- TODO: set up a finalizer to decrement the refcount.
        let client = RefClient
                { refClient = ImportClient { importId = importId }
                , localVat = vat
                }
        modifyTVar' imports $ flip M.alter importId $ \case
            Nothing -> Just 1
            Just !n -> Just (n+1)
        pure client


instance Eq Vat where
    -- it is sufficient to compare any of the TVars, since we create the whole
    -- vat as a unit:
    Vat{questions=qa} == Vat{questions=qb} = qa == qb

-- | A 'VatConfig' carries various parameters controlling the behavior of a
-- 'Vat'.
--
-- 'vatConfig' can be used to create a 'VatConfig', using sensible defaults
-- for most fields. The default values for these fields are documented
-- in-line.
data VatConfig = VatConfig
    { maxQuestions   :: !Word32
    -- ^ The maximum number of simultanious outstanding requests to the peer
    -- vat. Once this limit is reached, further questsions will block until
    -- some of the existing questions have been answered.
    --
    -- Defaults to 32.

    , maxAnswers     :: !Word32
    -- ^ The maximum size of our answers table. This has the effect of limiting
    -- the number of outstanding questions we may be servicing from the remote
    -- vat. Once this limit is reached, further questions from the remote vat
    -- will block.
    --
    -- Defaults to 32.


    , maxLocalCalls  :: !Word32
    -- Like maxAnswers, but this limits the number of calls that can be
    -- queued on objects hosted by this vat. This is subtly different, in that
    -- it covers the case where an attacker has attempted to cause resource
    -- exhaustion by triggering a large number of intra-vat calls rather than
    -- making a large number of calls themselves.
    --
    -- Defaults to 32.
    --
    -- TODO: is availAnswers still necessary? I(zenhack) need to think about
    -- the implications.

    , maxExports     :: !Word32
    -- ^ The maximum number of objects which may be exported by this vat.
    --
    -- Defaults to 32.

    , offerBootstrap :: Maybe (RpcT IO Client)
    -- ^ If not 'Nothing', 'offerBootstrap' is used to create the bootstrap
    -- interface if and when the peer vat first requests it. If this is
    -- 'Nothing', this vat will respond to bootstrap messages with
    -- @unimplemented@.
    --
    -- Defaults to 'Nothing'

    , withBootstrap  :: Maybe (Client -> RpcT IO ())
    -- ^ An action to perform with a client for the remote vat's bootstrap
    -- interface, on connection startup. If 'Nothing', the remote vat's
    -- bootstrap interface will not be requested.

    , debugMode      :: !Bool
    -- ^ In debug mode, errors reported by the RPC system to its peers will
    -- contain extra information. This should not be used in production, as
    -- it is possible for these messages to contain sensitive information,
    -- but it can be useful for debugging.
    --
    -- Defaults to 'False'.

    , getTransport   :: WordCount -> Transport
    -- ^ get the transport to use, as a function of the limit.

    , limit          :: !WordCount
    -- ^ The limit to use when reading and decoding messages.
    --
    -- Defaults to 'defaultLimit'
    }

-- | Create a new vat config, using the given function to create a
-- transport as a function of the limit. sets default values for
-- other fields; see the documentation for 'VatConfig'.
vatConfig :: (WordCount -> Transport) -> VatConfig
vatConfig getTransport = VatConfig
    { maxQuestions = 32
    , maxAnswers = 32
    , maxExports = 32
    , maxLocalCalls = 32
    , offerBootstrap = Nothing
    , withBootstrap = Nothing
    , debugMode = False
    , getTransport = getTransport
    , limit = defaultLimit
    }

-- | @'runVat' config@ starts an rpc session with a new vat, using the
-- settings in @config@
--
-- 'runVat' does not return until the session terminates. If it is
-- terminated via 'stopVat', it will return normally, otherwise it will
-- raise an exception of type 'RpcError'. See the documentation for
-- 'RpcError' for what these errors mean.
runVat :: VatConfig -> IO ()
runVat config@VatConfig{limit, getTransport, withBootstrap} = do
    let transport = getTransport limit
    ret <- try $ withSupervisor $ \sup -> do
        vat <- newVat config sup
        foldl concurrently_
            (recvLoop transport vat)
            [ sendLoop transport vat
            , do
                ret <- try $ coordinator vat
                case ret of
                    Left e@(SentAbort _) -> do
                        -- Give the message a bit of time to reach
                        -- the remote vat.
                        threadDelay 100000
                        throwIO e
                    Left e@(ReceivedAbort _) ->
                        throwIO e
                    Right _ ->
                        error $
                            "BUG: coordinator returned normally; it should " ++
                            "loop until an exception is thrown."
            , runRpcT vat $ for_ withBootstrap $ \with ->
                bootstrap >>= with
            ]
    case ret of
        Left (_ :: StopVat) ->
            -- The StopVat exception is used to signal that we should shut
            -- down the connection.
            pure ()
        Right () ->
            error $
                "runVat stopped without a call to `stopVat`; this should " ++
                "never happen!"

-- | Run an 'RpcT'.
runRpcT :: Vat -> RpcT m a -> m a
runRpcT vat (RpcT m) = runReaderT m vat

-- | Create a new 'Vat', based on the information in the 'VatConfig'. Use the
-- given supervisor to monitor exported objects.
newVat :: VatConfig -> Supervisor -> IO Vat
newVat VatConfig{..} supervisor = atomically $ do
    questions <- newTVar M.empty
    answers <- newTVar M.empty
    imports  <- newTVar M.empty
    exports <- newTVar M.empty

    questionIdPool <- newTVar [0..maxQuestions-1]
    exportIdPool <- newTVar [0..maxExports-1]

    availAnswers <- newTSem $ fromIntegral maxAnswers
    availLocalCalls <- newTSem $ fromIntegral maxLocalCalls

    sendQ <- newTBQueue $ fromIntegral maxQuestions
    recvQ <- newTBQueue $ fromIntegral maxQuestions

    pure Vat{..}

-- | 'sendLoop' shunts messages from the send queue into the transport.
sendLoop :: Transport -> Vat -> IO ()
sendLoop transport Vat{sendQ} =
    forever $ atomically (readTBQueue sendQ) >>= sendMsg transport

-- | 'recvLoop' shunts messages from the transport into the receive queue.
recvLoop :: Transport -> Vat -> IO ()
recvLoop transport Vat{recvQ} =
    forever $ recvMsg transport >>= atomically . writeTBQueue recvQ

-- | The coordinator handles incoming messages, dispatching them as
-- method calls to local objects, forwarding return values to the right
-- place, etc.
coordinator :: Vat -> IO ()
coordinator vat@Vat{..} = forever $ do
    msg <- atomically $ readTBQueue recvQ
    pureMsg <- case msgToValue msg of
        Right msg ->
            pure msg
        Left (e :: SomeException) ->
            abortIO vat $
                "Error decoding message" <>
                if debugMode then
                    ": " <> T.pack (show e)
                else
                    "."
    case pureMsg of
        Message'unimplemented msg ->
            handleUnimplementedMsg vat msg
        Message'abort exn ->
            handleAbortMsg vat exn
        -- Level 0:
        Message'bootstrap bs ->
            handleBootstrapMsg vat bs
        Message'call call -> do
            rawMsg <- msgToValue msg
            case rawMsg of
                Rpc.Message'call rawCall ->
                    handleCallMsg rawCall vat call
                _ ->
                    error $
                        "BUG: decoding as pure resulted in a 'call' message, " ++
                        "but decoding as raw did not. This should never happen!"
        Message'return ret -> do
            -- TODO: factor out this and the bit for call above.
            rawRet' <- evalLimitT limit $ do
                ret <- msgToValue msg
                case ret of
                    Rpc.Message'return rawReturn ->
                        Rpc.get_Return'union' rawReturn
                    _ ->
                        error $
                            "BUG: decoding as pure resulted in a 'return' message, " ++
                            "but decoding as raw did not. This should never happen!"
            handleReturnMsg rawRet' vat ret
        Message'finish finish ->
            handleFinishMsg vat finish
        -- Level >= 1:
        _ ->
            atomically $ replyUnimplemented vat pureMsg

-----------------------------------------------------------------------------
-- Helpers for common responses.
-----------------------------------------------------------------------------

-- | Report the specified message to the remote vat as unimplemented.
replyUnimplemented :: Vat -> Message -> STM ()
replyUnimplemented Vat{sendQ, limit} msg =
    createPure limit (valueToMsg $ Message'unimplemented msg)
    >>= writeTBQueue sendQ

-- | Send an abort message to the remote vat with the given reason field
-- and a type field of @failed@, and return the exception that was included
-- in the message.
replyAbort :: Vat -> T.Text -> STM Exception
replyAbort Vat{sendQ, limit} reason = do
    let exn = def
            { reason
            , type_ = Exception'Type'failed
            }
    msg <- createPure limit $ valueToMsg $ Message'abort exn
    writeTBQueue sendQ msg
    pure exn

-- | Send an abort message to the remote vat with the given reason field
-- and a type field of @failed@, and then *return* the exception, wrapped
-- in @'Left' . 'SentAbort'@.
--
-- The exception is *returned*, rather than thrown, so that the transaction
-- is not rolled back.
abort :: Vat -> T.Text -> STM (Either RpcError a)
abort vat text = Left . SentAbort <$> replyAbort vat text

-- | Send an abort message to the remote vat with the given reason field
-- and a type field of @failed@, and then throw the exception, wrapped in
-- 'SentAbort'.
abortIO :: MonadIO m => Vat -> T.Text -> m a
abortIO vat reason = liftIO $ do
    exn <- atomically $ replyAbort vat reason
    throwIO $ SentAbort exn

-- | helper function that converts an Either into a value, throwing the
-- exception if it is a 'Left'. There's no conceptual reason this needs
-- to be 'RpcError'; it could be any exception. But we only currently use
-- it in places where we only want to throw 'RpcError'.
throwLeft :: Either RpcError v -> IO v
throwLeft (Left e)  = throwIO e
throwLeft (Right v) = pure v

ok :: Monad m => m a -> m (Either RpcError a)
ok = fmap Right

-- | @'convertExn' debugMode exn@ converts an arbitrary haskell exception
-- into a capnproto 'Exception'. If the exception is already the correct type
-- it is returned as-is. Otherwise, the result has a type of 'Exception'Type'failed'
-- and a reason including the text "unhandled exception." If @debugMode@ is 'True',
-- the reason field will also include extra information about the original exception.
convertExn :: Bool -> SomeException -> Exception
convertExn debugMode e = case fromException e of
    Just (e :: Exception) ->
        e
    Nothing -> def
        { reason = "unhandled exception" <>
                if debugMode
                    then ": " <> T.pack (show e)
                    else ""
        , type_ = Exception'Type'failed
        }

-----------------------------------------------------------------------------
-- Misc. helpers for manipiulating the vat's state.
-----------------------------------------------------------------------------

-- | Insert a new answer into our answers table. Blocks if we've reached the
-- set limit on outstanding questions from the remote vat.
insertAnswer :: Vat -> AnswerId -> Answer -> STM ()
insertAnswer Vat{..} key value = do
    waitTSem availAnswers
    modifyTVar' answers $ M.insert key value

-- | Delete an answer from our answers table, keeping track of the change to
-- the number of available answers.
deleteAnswer :: Vat -> AnswerId -> STM ()
deleteAnswer Vat{..} answerId = do
    signalTSem availAnswers
    modifyTVar' answers (M.delete answerId)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- Handler code for specific types of messages.
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Handle an @unimplemented@ message.
handleUnimplementedMsg :: Vat -> Message -> IO ()
handleUnimplementedMsg vat msg = case msg of
    Message'unimplemented _ ->
        -- If the client itself doesn't handle unimplemented messages, that's
        -- their problem.
        pure ()
    Message'abort _ ->
        -- There's something very wrong on the other vat; we didn't send an
        -- abort, since we only do that right before, you know, aborting the
        -- connection.
        abortIO vat $
            "Your vat sent an 'unimplemented' message for an abort message " <>
            "that its remote peer never sent. This is likely a bug in your " <>
            "capnproto library."
    msg ->
        abortIO vat $
            "Your vat replied with 'unimplemented' for a required message." <>
            case vat of
                Vat{debugMode=True} ->
                    T.pack $ " The message was:" ++ show msg
                _ ->
                    ""

-- | Handle an @abort@ message.
handleAbortMsg :: Vat -> Exception -> IO ()
handleAbortMsg _ exn =
    throwIO (ReceivedAbort exn)

-----------------------------------------------------------------------------
-- level 0 messages
-----------------------------------------------------------------------------

-- | Handle a bootstrap message.
handleBootstrapMsg :: Vat -> Bootstrap -> IO ()
handleBootstrapMsg vat@Vat{..} msg@Bootstrap{questionId} =
    -- TODO: right now this will run getServer for each bootstrap message;
    -- do we want to re-use the same server in the event that our peer
    -- sends more than bootstrap message?
    case offerBootstrap of
        Nothing ->
            atomically $ replyUnimplemented vat $ Message'bootstrap msg
        Just getServer -> do
            server <- runRpcT vat getServer
            atomically $
                insertAnswer vat questionId (ClientAnswer server)
                -- TODO: also add it to exports and send a Return.

-- | Handle a call message.
handleCallMsg :: Rpc.Call ConstMsg -> Vat -> Call -> IO ()
handleCallMsg rawCall vat@Vat{..} msg@Call{questionId=callQuestionId,target,interfaceId,methodId,params=Payload{capTable}} =
    case target of
        MessageTarget'unknown' _ ->
            atomically $ replyUnimplemented vat $ Message'call msg
        MessageTarget'importedCap exportId -> do
            result <- atomically $ M.lookup exportId <$> readTVar exports
            case result of
                Nothing ->
                    abortIO vat $
                        "Received 'Call' on non-existent export #" <>
                            T.pack (show exportId)
                Just Export{promiseState, serverQueue} ->
                    handleCallToClient rawCall vat msg RefClient
                        { localVat = vat
                        , refClient = ExportClient
                            { serverQueue
                            , promiseState
                            , exportId = exportId
                            }
                        }
        MessageTarget'promisedAnswer PromisedAnswer{questionId=targetQuestionId, transform} -> do
            result <- atomically $ M.lookup targetQuestionId <$> readTVar answers
            case result of
                Nothing ->
                    abortIO vat $
                        "Received 'Call' on non-existent promised answer #"
                            <> T.pack (show targetQuestionId)
                Just (ClientAnswer client) ->
                    handleCallToClient rawCall vat msg client
                Just PromiseAnswer{promise, transform=oldTransform} -> do
                    -- TODO(perf): the append here is O(n); we could stand to improve
                    -- on this, if it is a bottleneck.
                    let newTransform = oldTransform <> transform
                    atomically $ do
                        insertAnswer vat callQuestionId $
                            PromiseAnswer{ promise, transform = newTransform }
                        superviseSTM supervisor $ do
                            -- FIXME: this is broken, in that it doesn't respect E-order.
                            result <- try $ waitIO promise
                            case result of
                                Left e ->
                                    throwExnAnswer $ convertExn debugMode e
                                Right ret ->
                                    case followTransform (Just (PtrStruct ret)) newTransform of
                                        Left exn ->
                                            throwExnAnswer exn
                                        Right Nothing ->
                                            handleCallToClient rawCall vat msg nullClient
                                        Right (Just (PtrCap client)) ->
                                            handleCallToClient rawCall vat msg client
                                        Right _ ->
                                            throwExnAnswer def
                                                { type_ = Exception'Type'failed
                                                , reason =
                                                    "Tried to call a method on a non-capability pointer."
                                                }
  where
    throwExnAnswer exn = do
        msg <- createPure limit $ valueToMsg $ Message'return def
                { answerId = callQuestionId
                , union' = Return'exception exn
                }
        atomically $ do
            writeTBQueue sendQ msg
            insertAnswer vat callQuestionId (ClientAnswer (ExnClient exn))
    followTransform :: Maybe PtrType -> V.Vector PromisedAnswer'Op -> Either Exception (Maybe PtrType)
    followTransform ptr ops | V.length ops == 0 = Right ptr
    followTransform ptr ops = case (ptr, ops V.! 0) of
        (_, PromisedAnswer'Op'noop) ->
            followTransform ptr (V.drop 1 ops)
        (_, PromisedAnswer'Op'unknown' n) ->
            Left def
                { type_ = Exception'Type'failed
                , reason = "Unknown PromisedAnswer.Op #" <> T.pack (show n)
                }
        (Nothing, PromisedAnswer'Op'getPointerField idx) ->
            -- a null pointer is interpreted as a struct with zero size; in this case
            -- the whole pointer section is semantically full of 'Nothing':
            followTransform Nothing (V.drop 1 ops)
        (Just (PtrStruct (Struct _ ptrSection)), PromisedAnswer'Op'getPointerField idx) ->
            followTransform (sliceIndex (fromIntegral idx) ptrSection) (V.drop 1 ops)
        (Just _, PromisedAnswer'Op'getPointerField _) ->
            Left def
                { type_ = Exception'Type'failed
                , reason = "Call tried to fetch a pointer field of a non-struct."
                }


-- helper for 'handleCallMsg'; this handles the case where we have a 'Client'
-- available to service the call.
handleCallToClient :: Rpc.Call ConstMsg -> Vat -> Call -> Client -> IO ()
handleCallToClient
        rawCall
        vat@Vat{..}
        msg@Call
            { questionId=callQuestionId
            , target
            , interfaceId
            , methodId
            , params=Payload{capTable}
            }
        client = do
    -- We fish out the low-level representation of the params, set the
    -- cap table based on the value in the call message, and then
    -- pass it to the handler. This ensures that any Clients in the value
    -- are actually connected; on the initial decode they will be null,
    -- since the cap table is empty.
    --
    -- the handler has to decode again anyway. We should try to make this
    -- whole business more efficient. See also #52.
    clients <- traverse (interpCapDescriptor vat) capTable
    rawCall <- Untyped.tMsg (pure . Message.withCapTable clients) rawCall
    paramContent <- evalLimitT limit $
        Rpc.get_Call'params rawCall
        >>= Rpc.get_Payload'content
    (ret, fulfiller) <- newPromiseIO
    runRpcT vat $ call client interfaceId methodId paramContent fulfiller
    atomically $ do
        insertAnswer vat callQuestionId PromiseAnswer{ promise = ret, transform = V.empty }
        superviseSTM supervisor $ do
            result <- try $ waitIO ret
            union' <- case result of
                Left e ->
                    pure $ Return'exception $ convertExn debugMode e
                -- The server returned successfully; pass along the result.
                Right ret -> do
                    clients <- createPure limit $ valueToMsg ret
                    pure $ Return'results def
                        { content = Just (PtrStruct ret)
                        , capTable = V.map makeCapDescriptor (Message.getCapTable clients)
                        }
            msg <- createPure limit $ valueToMsg <$> Message'return $ def
                { answerId = callQuestionId
                , union' = union'
                }
            atomically $ writeTBQueue sendQ msg

-- | Handle receiving a 'Return' message.
handleReturnMsg :: Rpc.Return' ConstMsg -> Vat -> Return -> IO ()
handleReturnMsg rawRet' vat@Vat{..} msg@Return{..} = do
    question <- atomically $ M.lookup answerId <$> readTVar questions
    case question of
        Nothing -> abortIO vat $
            "Received 'Return' for non-existant question #"
                <> T.pack (show answerId)
        Just (BootstrapQuestion _) ->
            -- This will cause the other side to drop the resolved cap; we'll
            -- just keep using the promise.
            atomically $ replyUnimplemented vat $ Message'return msg
        Just CallQuestion{callMsg, sendReturn} ->
            case union' of
                Return'results Payload{capTable} -> do
                    clients <- traverse (interpCapDescriptor vat) capTable
                    (throwLeft =<<) $ atomically $ do
                        sendFinish
                        rawRet' <- Untyped.tMsg (pure . Message.withCapTable clients) rawRet'
                        rawRet'' <- evalLimitT limit $ Rpc.get_Return'' rawRet'
                        content <- case rawRet'' of
                            Rpc.Return'results payload ->
                                evalLimitT limit $ Rpc.get_Payload'content payload >>= decerialize
                            _ ->
                                error $
                                    "BUG: first decode of message yielded 'results', " ++
                                    "but second did not."
                        handleResult sendReturn content
                        ok $ modifyTVar questions $ M.delete answerId

                Return'exception exn ->
                    atomically $ do
                        sendFinish
                        breakPromise sendReturn exn

                -- These shouldn't come up in practice yet, since our
                -- implementation doesn't do anything that can trigger
                -- them, but when they do we won't need to do anything:
                Return'canceled -> atomically sendFinish
                Return'resultsSentElsewhere -> atomically sendFinish

                _ ->
                    abortIO vat $
                        "Received unexpected return variant " <>
                        "(we only support results and exception)."

  where
    sendFinish = do
        -- We don't support caps other than the bootstrap yet, so we can
        -- send Finish right away.
        msg <- createPure limit $ valueToMsg $
            Message'finish def { questionId = answerId }
        writeTBQueue sendQ msg
        modifyTVar questions $ M.delete answerId


    -- | handle the @result@ variant of a 'Return'.
    handleResult sendReturn content = case content of
        Nothing ->
            ok $ fulfill sendReturn def
        Just (PtrStruct s) ->
            ok $ fulfill sendReturn s
        Just _ -> abort vat $
            "Received non-struct pointer in a return message. " <>
            "Return values must always be structs."

handleFinishMsg :: Vat -> Finish -> IO ()
handleFinishMsg vat@Vat{..} Finish{questionId} =
    atomically $ deleteAnswer vat questionId


-----------------------------------------------------------------------------
-- level 1+ messages
-----------------------------------------------------------------------------
