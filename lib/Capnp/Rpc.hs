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
    , Transport(..)
    , handleTransport
    , socketTransport

    -- * Exceptions
    , methodUnimplemented
    , throwMethodUnimplemented

    -- ** Re-exported from the generated rpc.capnp module.
    , Exception(..)
    , Exception'Type(..)

    -- * Promises

    , Promise
    , Fulfiller
    , newPromise
    , newPromiseIO
    , fulfill
    , fulfillIO
    , breakPromise
    , isResolved
    , wait
    , waitIO
    ) where

import Prelude hiding (fail)

import Data.Word
import UnliftIO  hiding (Exception, wait)

import Control.Concurrent.STM          (throwSTM)
import Control.Monad                   (forever, when, (>=>))
import Control.Monad.Catch             (MonadThrow(..))
import Control.Monad.Fail              (MonadFail(..))
import Control.Monad.IO.Class          (MonadIO, liftIO)
import Control.Monad.Primitive         (PrimMonad(..))
import Control.Monad.Reader            (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class       (MonadTrans(lift))
import Data.Foldable                   (for_, traverse_)
import Data.Maybe                      (isJust)
import Network.Socket                  (Socket)
import System.IO                       (Handle)
import Text.ParserCombinators.ReadPrec (pfail)
import Text.Read                       (Lexeme(Ident), lexP, readPrec)

import qualified Data.Map.Strict    as M
import qualified Data.Text          as T
import qualified Data.Vector        as V
import qualified UnliftIO.Exception as HsExn

import Capnp.Gen.Capnp.Rpc.Pure

import Capnp
    ( ConstMsg
    , createPure
    , decerialize
    , def
    , defaultLimit
    , evalLimitT
    , hGetMsg
    , hPutMsg
    , msgToValue
    , sGetMsg
    , sPutMsg
    , valueToMsg
    )
import Capnp.Untyped.Pure   (PtrType(PtrStruct), Struct)
import Internal.CommitThrow (atomicallyCommitErrs, throwAndCommit)
import Internal.Supervisors (Supervisor, supervise, withSupervisor)

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

methodUnimplemented :: Exception
methodUnimplemented = Exception
    { reason = "Method unimplemented"
    , type_ = Exception'Type'unimplemented
    , obsoleteIsCallersFault = False
    , obsoleteDurability = 0
    }

instance HsExn.Exception Exception

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
    { sendMsg :: ConstMsg -> m ()
    -- ^ Send a message
    , recvMsg :: m ConstMsg
    -- ^ Receive a message
    }

-- | @'handleTransport' handle limit@ is a transport which reads and writes
-- messages from/to @handle@. It uses @limit@ as the traversal limit when
-- reading messages and decoding.
handleTransport :: MonadIO m => Handle -> Int -> Transport m
handleTransport handle limit = Transport
    { sendMsg = liftIO . hPutMsg handle
    , recvMsg = liftIO $ hGetMsg handle limit
    }

-- | @'socketTransport' socket limit@ is a transport which reads and writes
-- messages to/from a socket. It uses @limit@ as the traversal limit when
-- reading messages and decoing.
socketTransport :: MonadIO m => Socket -> Int -> Transport m
socketTransport socket limit = Transport
    { sendMsg = liftIO . sPutMsg socket
    , recvMsg = liftIO $ sGetMsg socket limit
    }

-- | Get a new exportId/questionId. The argument gets the pool to allocate from.
newId :: MonadIO m => (Vat -> TVar [Word32]) -> RpcT m Word32
newId pool = do
    vat <- RpcT ask
    liftIO $ atomically $ do
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
    = RemoteClient
        { target   :: MessageTarget
        , localVat :: Vat
        }
    | LocalClient
        { exportId    :: !ExportId
        , serverQueue :: ServerQueue
        , localVat    :: Vat
        }
    | NullClient
    | DisconnectedClient

type ServerQueue = TBQueue (Server -> RpcT IO Bool)

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
    deriving(Show)
instance HsExn.Exception RpcError

-- | Shut down the rpc connection, and all resources managed by the vat. This
-- does not return (it raises an exception used to actually signal termination
-- of the connection).
stopVat :: MonadIO m => RpcT m ()
stopVat = liftIO (throwIO StopVat)

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
export :: MonadUnliftIO m => Server -> RpcT m Client
export localServer = do
    exportId <- newExportId
    localVat@Vat{exports, maxObjectCalls, supervisor} <- RpcT ask

    queue <- atomically $ newTBQueue (fromIntegral maxObjectCalls)

    -- We add an entry to our exports table. The refcount *starts* at zero,
    -- and will be incremented to one the first time we actually send this
    -- capability to the remote vat. Because the refcount checking happens
    -- in response to interaction with the remote vat, it won't be removed
    -- until it goes *back* to being zero.
    atomically $ modifyTVar exports $ M.insert exportId Export
        { serverQueue = queue
        , refCount = 0
        }
    liftIO $ supervise (runRpcT localVat $ runServer localServer queue) supervisor
    pure LocalClient{exportId, localVat, serverQueue = queue}

runServer server queue = do
    job <- atomically $ readTBQueue queue
    continue <- job server
    when continue (runServer server queue)

-- | Get a client for the bootstrap interface from the remote vat.
bootstrap :: MonadIO m => RpcT m Client
bootstrap = do
    questionId <- newQuestionId
    vat <- RpcT ask
    let msg = Message'bootstrap def { questionId }
    atomically $ sendQuestion vat (BootstrapQuestion questionId)
    pure RemoteClient
        { target = MessageTarget'promisedAnswer def { questionId }
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

    -- I(zenhack) don't *think* we need to do anything in these cases, but I
    -- need to think about it more carefully.
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


-- | @'call' interfaceId methodId params client@ calls an RPC method
-- on @client@. The method is as specified by @interfaceId@ and
-- @methodId@. The return value is a promise for the result.
call :: Word64 -> Word16 -> Maybe (Untyped.Ptr ConstMsg) -> Client -> RpcT IO (Promise Struct)
call interfaceId methodId paramContent RemoteClient{ target, localVat } = do
    Vat{limit} <- RpcT ask
    questionId <- newQuestionId
    let capTable = maybe
            V.empty
            (V.map makeCapDescriptor . Message.getCapTable . Untyped.message)
            paramContent
    paramContent <- evalLimitT limit (decerialize paramContent)
    let callMsg = Call
            { sendResultsTo = Call'sendResultsTo'caller
            , allowThirdPartyTailCall = False
            , params = def
                { content = paramContent
                , capTable = capTable
                }
            , ..
            }
    (promise, fulfiller) <- atomically newPromise
    atomically $ sendQuestion localVat $ CallQuestion
        { callMsg
        , sendReturn = fulfiller
        }
    pure promise
call interfaceId methodId params LocalClient{serverQueue} = do
    (promise, fulfiller) <- newPromiseIO
    atomically $ writeTBQueue serverQueue $ \server -> do
        handleCall server interfaceId methodId params fulfiller
        pure True
    pure promise
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
alwaysThrow :: MonadIO m => Exception -> RpcT m (Promise Struct)
alwaysThrow exn = do
    (promise, fulfiller) <- atomically newPromise
    atomically $ breakPromise fulfiller exn
    pure promise

-- | A 'Fulfiller' is used to fulfill a promise.
newtype Fulfiller a = Fulfiller
    { var :: TVar (Maybe (Either Exception a))
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

-- | Like 'fulfill', but in the IO monad.
fulfillIO :: MonadIO m => Fulfiller a -> a -> m ()
fulfillIO fulfiller = atomically . fulfill fulfiller

-- | Break a promise. When the user of the promise executes 'wait', the
-- specified exception will be raised. It is an error to call 'breakPromise'
-- if the promise has already been fulfilled (or broken).
breakPromise :: Fulfiller a -> Exception -> STM ()
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
            retrySTM
        Just (Right result) ->
            pure result
        Just (Left exn) ->
            throwSTM exn

-- | Like 'wait', but in the 'IO' monad.
waitIO :: MonadIO m => Promise a -> m a
waitIO = atomically . wait

-- | Check whether a promise is resolved.
isResolved :: Promise a -> STM Bool
isResolved Promise{var} = isJust <$> readTVar var

-- | Create a new promise and an associated fulfiller.
newPromise :: STM (Promise a, Fulfiller a)
newPromise = do
    var <- newTVar Nothing
    pure (Promise{var}, Fulfiller{var})

-- | Like 'newPromise', but in the IO monad.
newPromiseIO :: MonadIO m => m (Promise a, Fulfiller a)
newPromiseIO = atomically newPromise

-- | A promise is a value that may not be ready yet.
newtype Promise a = Promise
    { var :: TVar (Maybe (Either Exception a))
    }

-- | A 'Question' is an outstanding question message.
data Question
    = CallQuestion
        { callMsg    :: Call
        , sendReturn :: Fulfiller Struct
        }
    | BootstrapQuestion !QuestionId

newtype Answer
    = ClientAnswer Client

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
    { questions      :: TVar (M.Map QuestionId Question)
    , answers        :: TVar (M.Map AnswerId Answer)
    , imports        :: TVar (M.Map ImportId Word32) -- values are the refcount
    , exports        :: TVar (M.Map ExportId Export)

    -- Pools of available IDs for questions and exported objects.
    , questionIdPool :: TVar [Word32]
    , exportIdPool   :: TVar [Word32]

    -- Create the bootstrap interface for a connection. If 'Nothing', bootstrap
    -- messages return unimplemented.
    , offerBootstrap :: Maybe (RpcT IO Client)

    -- queues on which to send and receive messages.
    , sendQ          :: TBQueue ConstMsg
    , recvQ          :: TBQueue ConstMsg

    -- Supervisor which monitors object lifetimes.
    , supervisor     :: Supervisor

    -- same as the corresponding fields in 'VatConfig'
    , debugMode      :: !Bool
    , limit          :: !Int
    , maxObjectCalls :: !Word32

    }

data Export = Export
    { serverQueue :: ServerQueue
    , refCount    :: !Word32
    }

-- | @'makeCapDescriptor' client@ creates a cap descriptor suitable
-- for sending to a remote vat to describe the specified @client@.
makeCapDescriptor :: Client -> CapDescriptor
makeCapDescriptor NullClient         = CapDescriptor'none
makeCapDescriptor DisconnectedClient = CapDescriptor'none
makeCapDescriptor RemoteClient{target} = case target of
    MessageTarget'importedCap importId ->
        CapDescriptor'receiverHosted importId
    MessageTarget'promisedAnswer pa ->
        CapDescriptor'receiverAnswer pa
    MessageTarget'unknown' _ ->
        CapDescriptor'none
makeCapDescriptor LocalClient{exportId} =
    CapDescriptor'senderHosted exportId

-- | Convert a 'CapDescriptor' from an incoming message into a Client, updating
-- the local vat's table if needed.
--
-- This may call 'throwAndCommit', if the cap descriptor is invalid.
interpCapDescriptor :: Vat -> CapDescriptor -> STM Client
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

    CapDescriptor'receiverHosted exportId -> do
        exports <- readTVar exports
        case M.lookup exportId exports of
            Nothing ->
                abort vat $
                    "Incoming capability table referenced non-existent " <>
                    "receiverHosted capability #" <> T.pack (show exportId)
            Just Export{serverQueue} ->
                pure LocalClient
                    { serverQueue
                    , localVat = vat
                    , exportId = exportId
                    }
    CapDescriptor'receiverAnswer _ -> abort vat "receiverAnswer not supported"
    CapDescriptor'thirdPartyHosted _ -> abort vat "thirdPartyHosted not supported"
    CapDescriptor'unknown' tag ->
        abort vat $ T.pack $ "unknown cap descriptor variant #" ++ show tag
  where
    -- create a client based on an import id. This increments the refcount for
    -- that import.
    getImportClient importId = do
        -- TODO: set up a finalizer to decrement the refcount.
        let client = RemoteClient
                { target = MessageTarget'importedCap importId
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

    , maxExports     :: !Word32
    -- ^ The maximum number of objects which may be exported by this vat.
    --
    -- Defaults to 32.

    , maxObjectCalls :: !Word32
    -- ^ The maximum number of calls which may be outstanding on a single
    -- object. If this number is exceeded, the vat will block on handling
    -- other incoming messages until there is free space in the object's
    -- queue.
    --
    -- Defaults to 10.

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

    , getTransport   :: Int -> Transport IO
    -- ^ get the transport to use, as a function of the limit.

    , limit          :: !Int
    -- ^ The limit to use when reading and decoding messages.
    --
    -- Defaults to 'defaultLimit'
    }

-- | Create a new vat config, using the given function to create a
-- transport as a function of the limit. sets default values for
-- other fields; see the documentation for 'VatConfig'.
vatConfig :: (Int -> Transport IO) -> VatConfig
vatConfig getTransport = VatConfig
    { maxQuestions = 32
    , maxExports = 32
    , maxObjectCalls = 10
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
            , coordinator vat
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

    sendQ <- newTBQueue $ fromIntegral maxQuestions
    recvQ <- newTBQueue $ fromIntegral maxQuestions

    pure Vat{..}

-- | 'sendLoop' shunts messages from the send queue into the transport.
sendLoop :: Transport IO -> Vat -> IO ()
sendLoop transport Vat{sendQ} =
    forever $ atomically (readTBQueue sendQ) >>= sendMsg transport

-- | 'recvLoop' shunts messages from the transport into the receive queue.
recvLoop :: Transport IO -> Vat -> IO ()
recvLoop transport Vat{recvQ} =
    forever $ recvMsg transport >>= atomically . writeTBQueue recvQ

-- | The coordinator handles incoming messages, dispatching them as
-- method calls to local objects, forwarding return values to the right
-- place, etc.
coordinator :: Vat -> IO ()
coordinator vat@Vat{..} = forever $ do
    msg <- atomically $ readTBQueue recvQ
    pureMsg <- evalLimitT limit (msgToValue msg)
    case pureMsg of
        Message'unimplemented msg ->
            handleUnimplementedMsg vat msg
        Message'abort exn ->
            handleAbortMsg vat exn
        -- Level 0:
        Message'bootstrap bs ->
            handleBootstrapMsg vat bs
        Message'call call -> do
            rawMsg <- evalLimitT limit (msgToValue msg)
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

----------------- Helpers for common responses. ----------------------------

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
    msg <- createPure limit $ valueToMsg exn
    writeTBQueue sendQ msg
    pure exn

-- | Send an abort message to the remote vat with the given reason field
-- and a type field of @failed@, and then throw the same exception via
-- 'throwAndCommit'.
--
-- FIXME: This will likely usually not actually send the abort, since we throw
-- an exception right after putting it in the send queue, which will kill the
-- send loop. We should add a delay here.
abort :: Vat -> T.Text -> STM a
abort vat = replyAbort vat >=> throwAndCommit . SentAbort

-- | Like 'abort', but runs in IO.
abortIO :: MonadIO m => Vat -> T.Text -> m a
abortIO vat reason =
    liftIO $ atomicallyCommitErrs $ abort vat reason

-- | Like 'abortIO', but runs in 'RpcT' and does not require a 'Vat' argument.
abortT :: T.Text -> RpcT IO a
abortT reason = do
    vat <- RpcT ask
    abortIO vat reason

----------------- Handler code for specific types of messages. ---------------

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

-- level 0 --

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
                modifyTVar' answers $ M.insert questionId (ClientAnswer server)
                -- TODO: also add it to exports and send a Return.

handleCallMsg :: Rpc.Call ConstMsg -> Vat -> Call -> IO ()
handleCallMsg rawCall vat@Vat{..} msg@Call{target,interfaceId,methodId,params=Payload{capTable}} =
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
                Just Export{serverQueue} ->
                    handleCallToClient rawCall vat msg LocalClient
                        { serverQueue
                        , localVat = vat
                        , exportId = exportId
                        }
        MessageTarget'promisedAnswer PromisedAnswer{questionId=targetQuestionId, transform}
            | V.length transform /= 0 ->
                atomically $ replyUnimplemented vat $ Message'call msg
            | otherwise -> do
                result <- atomically $ M.lookup targetQuestionId <$> readTVar answers
                case result of
                    Nothing ->
                        -- TODO: adjust this so we don't throw (and thus kill the connection)
                        -- before the abort message is actually sent.
                        abortIO vat $
                            "Received 'Call' on non-existent promised answer #"
                                <> T.pack (show targetQuestionId)
                    Just (ClientAnswer client) ->
                        handleCallToClient rawCall vat msg client

-- helper for 'handleCallMsg'; this handles the case where we have a Client available to service the call.
handleCallToClient :: Rpc.Call ConstMsg -> Vat -> Call -> Client -> IO ()
handleCallToClient rawCall vat@Vat{..} msg@Call{questionId=callQuestionId,target,interfaceId,methodId,params=Payload{capTable}} client = do
    result <- try $ do
        -- We fish out the low-level representation of the params, set the
        -- cap table based on the value in the call message, and then
        -- pass it to the handler. This ensures that any Clients in the value
        -- are actually connected; on the initial decode they will be null,
        -- since the cap table is empty.
        --
        -- the handler has to decode again anyway. We should try to make this
        -- whole business more efficient. See also #52.
        rawCall <- atomicallyCommitErrs $ updateRawCapTable vat capTable rawCall
        paramContent <- evalLimitT limit $
            Rpc.get_Call'params rawCall
            >>= Rpc.get_Payload'content
        ret <- runRpcT vat $ call interfaceId methodId paramContent client
        waitIO ret
    union' <- case result of
        -- The server returned successfully; pass along the result.
        Right ok -> do
            clients <- createPure limit $ valueToMsg ok
            pure $ Return'results def
                { content = Just (PtrStruct ok)
                , capTable = V.map makeCapDescriptor (Message.getCapTable clients)
                }
        Left (e :: SomeException) -> pure $ def
            -- The server threw an exception; we need to report this
            -- to the caller.
            Return'exception $
                case fromException e of
                    Just (e :: Exception) ->
                        -- If the exception was a capnp exception,
                        -- just pass it along.
                        e
                    Nothing -> def
                        -- Otherwise, return something opaque to
                        -- the caller; the exception could potentially
                        -- contain sensitive info.
                        { reason = "unhandled exception" <>
                                if debugMode
                                    then ": " <> T.pack (show e)
                                    else ""
                        , type_ = Exception'Type'failed
                        }
    msg <- createPure limit $ valueToMsg <$> Message'return $ def
        { answerId = callQuestionId
        , union' = union'
        }
    atomically $ writeTBQueue sendQ msg

updateRawCapTable vat capTable raw = do
    clients <- traverse (interpCapDescriptor vat) capTable
    Untyped.tMsg (pure . Message.withCapTable clients) raw

-- | Handle receiving a 'Return' message.
handleReturnMsg :: Rpc.Return' ConstMsg -> Vat -> Return -> IO ()
handleReturnMsg rawRet' vat@Vat{..} msg@Return{..} = atomicallyCommitErrs $ do
    question <- M.lookup answerId <$> readTVar questions
    case question of
        Nothing -> abort vat $
            "Received 'Return' for non-existant question #"
                <> T.pack (show answerId)
        Just (BootstrapQuestion _) -> do
            -- This will cause the other side to drop the resolved cap; we'll
            -- just keep using the promise.
            replyUnimplemented vat $ Message'return msg
            modifyTVar questions $ M.delete answerId
        Just CallQuestion{callMsg, sendReturn} -> do
            -- We don't support caps other than the bootstrap yet, so we can
            -- send Finish right away.
            msg <- createPure limit $ valueToMsg $
                Message'finish def { questionId = answerId }
            writeTBQueue sendQ msg
            modifyTVar questions $ M.delete answerId
            case union' of
                Return'results Payload{capTable} -> do
                    rawRet' <- updateRawCapTable vat capTable rawRet'
                    rawRet'' <- evalLimitT limit $ Rpc.get_Return'' rawRet'
                    content <- case rawRet'' of
                        Rpc.Return'results payload ->
                            evalLimitT limit $ Rpc.get_Payload'content payload >>= decerialize
                        _ ->
                            error "TODO: better error message"
                    handleResult sendReturn content
                Return'exception exn ->
                    breakPromise sendReturn exn

                -- These shouldn't come up in practice yet, since our
                -- implementation doesn't do anything that can trigger
                -- them, but when they do we won't need to do anything:
                Return'canceled -> pure ()
                Return'resultsSentElsewhere -> pure ()

                _ ->
                    abort vat $
                        "Received unexpected return variant " <>
                        "(we only support results and exception)."

  where
    -- | handle the @result@ variant of a 'Return'.
    handleResult sendReturn content = case content of
        Nothing ->
            fulfill sendReturn def
        Just (PtrStruct s) ->
            fulfill sendReturn s
        Just _ -> abort vat $
            "Received non-struct pointer in a return message. " <>
            "Return values must always be structs."

handleFinishMsg :: Vat -> Finish -> IO ()
handleFinishMsg Vat{..} Finish{questionId} =
    atomically $ modifyTVar' answers (M.delete questionId)

-- level >= 1 --
