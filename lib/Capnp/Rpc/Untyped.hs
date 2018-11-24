{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Capnp.Rpc.Untyped
    (
    -- * Connections to other vats
      ConnConfig(..)
    , handleConn

    -- * Clients for capabilities
    , Client
    , incRef
    , decRef
    , call
    , nullClient

    , IsClient(..)

    -- * Exporting local objects
    , export
    , clientMethodHandler

    -- * Errors
    , RpcError(..)
    , RpcGen.Exception(..)
    , RpcGen.Exception'Type(..)

    -- * Shutting down the connection
    , stopVat

#ifdef SELF_TESTS
    , selfTests
#endif
    ) where

#ifdef SELF_TESTS
import Test.Hspec
#endif

-- Note [Organization]
-- ===================
--
-- As much as possible, the logic in this module is centralized according to
-- type types of objects it concerns.
--
-- As an example, consider how we handle embargos: The 'Conn' type's 'embargos'
-- table has values that are arbitrary 'STM' transactions. This allows the code
-- which triggers sending embargoes to have full control over what happens when
-- they return, while the code that routes incoming messages (in 'coordinator')
-- doesn't need to concern itself with the details of embargos -- it just needs
-- to route them to the right place.
--
-- This approach generally results in better separation of concerns.

-- Note [Implementation checklist]
-- ===============================
--
-- While RPC support is still incomplete, we keep a checklist of some things
-- that still need is implemented. In many cases, it's more natural to put
-- error "TODO: ..." in the relevant spots in the source code, but there are
-- a few cross-cutting concerns that we keep track of here.
--
-- * [ ] Handle decode errors
-- * [ ] Resource limits (see Note [Limiting resource usage])

import Data.Word
import UnliftIO.STM

import Control.Concurrent     (threadDelay)
import Control.Concurrent.STM (catchSTM, flushTQueue, throwSTM)
import Control.Monad          (forever)
import Data.Default           (Default(def))
import Data.DList             (DList)
import Data.Foldable          (traverse_)
import Data.Hashable          (Hashable, hash, hashWithSalt)
import Data.String            (fromString)
import Data.Text              (Text)
import GHC.Generics           (Generic)
import Supervisors            (Supervisor, superviseSTM, withSupervisor)
import System.Mem.StableName
    (StableName, eqStableName, hashStableName, makeStableName)
import System.Mem.Weak        (addFinalizer)
import UnliftIO.Async         (concurrently_)
import UnliftIO.Exception     (Exception, bracket, handle, throwIO, try)

import qualified Data.DList        as DList
import qualified Data.Vector       as V
import qualified Focus
import qualified StmContainers.Map as M

import Capnp.Classes        (cerialize, decerialize)
import Capnp.Convert        (msgToValue, valueToMsg)
import Capnp.Message        (ConstMsg)
import Capnp.Promise        (breakPromise, fulfill, newCallback)
import Capnp.Rpc.Errors
    (eDisconnected, eFailed, eMethodUnimplemented, wrapException)
import Capnp.Rpc.Transport  (Transport(recvMsg, sendMsg))
import Capnp.TraversalLimit (defaultLimit, evalLimitT)
import Internal.BuildPure   (createPure)

import qualified Capnp.Gen.Capnp.Rpc      as RawRpc
import qualified Capnp.Gen.Capnp.Rpc.Pure as RpcGen
import qualified Capnp.Message            as Message
import qualified Capnp.Rpc.Server         as Server
import qualified Capnp.Untyped            as UntypedRaw
import qualified Capnp.Untyped.Pure       as Untyped


-- | Errors which can be thrown by the rpc system.
data RpcError
    = ReceivedAbort RpcGen.Exception
    -- ^ The remote vat sent us an abort message.
    | SentAbort RpcGen.Exception
    -- ^ We sent an abort to the remote vat.
    deriving(Show, Eq, Generic)

instance Exception RpcError

-- | 'StopVat' is an exception used to terminate a capnproto connection; it is
-- raised by `stopVat`.
--
-- XXX TODO: this is not a good mechanism. For one thing, it only actually works
-- if thrown from the same thread as 'handleConn'. Come up with something better.
data StopVat = StopVat deriving(Show)
instance Exception StopVat

-- | Shut down the rpc connection, and all resources managed by the vat. This
-- does not return (it raises an exception used to actually signal termination
-- of the connection).
stopVat :: IO ()
stopVat = throwIO StopVat

-- These aliases are the same ones defined in rpc.capnp; unfortunately the
-- schema compiler doesn't supply information about type aliases, so we
-- have to re-define them ourselves. See the comments in rpc.capnp for
-- more information.
type QuestionId = Word32
type AnswerId   = QuestionId
type ExportId   = Word32
type ImportId   = ExportId
type EmbargoId  = Word32

-- | A connection to a remote vat
data Conn = Conn
    { stableName     :: StableName ()
    -- So we can use the connection as a map key.

    , sendQ          :: TBQueue ConstMsg
    , recvQ          :: TBQueue ConstMsg
    -- queues of messages to send and receive; each of these has a dedicated
    -- thread doing the IO (see 'sendLoop' and 'recvLoop'):

    , supervisor     :: Supervisor
    -- Supervisor managing the lifetimes of threads bound to this connection.

    , questionIdPool :: IdPool
    , exportIdPool   :: IdPool
    -- Pools of identifiers for new questions and exports

    , maxAnswerCalls :: !Int
    -- Maximum number of calls that can be outstanding on an unresolved answer.

    , questions      :: M.Map QuestionId Question
    , answers        :: M.Map AnswerId Answer
    , exports        :: M.Map ExportId Export
    , imports        :: M.Map ImportId Import

    , embargos       :: M.Map EmbargoId (STM ())
    -- Outstanding embargos. When we receive a 'Disembargo' message with its
    -- context field set to receiverLoopback, we look up the embargo id in
    -- this table, and execute the STM we have registered.

    , bootstrap      :: Maybe Client
    -- The capability which should be served as this connection's bootstrap
    -- interface (if any).

    , debugMode      :: !Bool
    -- whether to include extra (possibly sensitive) info in error messages.
    }

instance Eq Conn where
    x == y = stableName x `eqStableName` stableName y

instance Hashable Conn where
    hash Conn{stableName} = hashStableName stableName
    hashWithSalt _ = hash

-- | Configuration information for a connection.
data ConnConfig = ConnConfig
    { maxQuestions   :: !Word32
    -- ^ The maximum number of simultanious outstanding requests to the peer
    -- vat. Once this limit is reached, further questsions will block until
    -- some of the existing questions have been answered.
    --
    -- Defaults to 32.

    , maxExports     :: !Word32
    -- ^ The maximum number of objects which may be exported on this connection.
    --
    -- Defaults to 32.

    , maxAnswerCalls :: !Int
    -- Maximum number of calls that can be outstanding on an unresolved answer.
    --
    -- Defaults to 16.

    , debugMode      :: !Bool
    -- ^ In debug mode, errors reported by the RPC system to its peers will
    -- contain extra information. This should not be used in production, as
    -- it is possible for these messages to contain sensitive information,
    -- but it can be useful for debugging.
    --
    -- Defaults to 'False'.

    , getBootstrap   :: Supervisor -> STM (Maybe Client)
    -- ^ Get the bootstrap interface we should serve for this connection.
    -- the argument is a supervisor whose lifetime is bound to the
    -- connection. If 'getBootstrap' returns 'Nothing', we will respond
    -- to bootstrap messages with an exception.
    --
    -- The default always returns 'nullClient'.

    , withBootstrap  :: Maybe (Supervisor -> Client -> IO ())
    -- ^ An action to perform with access to the remote vat's bootstrap
    -- interface. The supervisor argument is bound to the lifetime of the
    -- connection. If this is 'Nothing' (the default), the bootstrap
    -- interface will not be requested.
    }

instance Default ConnConfig where
    def = ConnConfig
        { maxQuestions   = 32
        , maxExports     = 32
        , maxAnswerCalls = 16
        , debugMode      = False
        , getBootstrap   = \_ -> pure Nothing
        , withBootstrap  = Nothing
        }

-- | Get a new question id. retries if we are out of available question ids.
newQuestion :: Conn -> STM QuestionId
newQuestion = newId . questionIdPool

-- | Return a question id to the pool of available ids.
freeQuestion :: Conn -> QuestionId -> STM ()
freeQuestion = freeId . questionIdPool

-- | Get a new export id. retries if we are out of available export ids.
newExport :: Conn -> STM ExportId
newExport = newId . exportIdPool

-- | Return a export id to the pool of available ids.
freeExport :: Conn -> ExportId -> STM ()
freeExport = freeId . exportIdPool

-- | Handle a connection to another vat. Returns when the connection is closed.
handleConn :: Transport -> ConnConfig -> IO ()
handleConn
    transport
    cfg@ConnConfig
        { maxQuestions
        , maxExports
        , withBootstrap
        , debugMode
        , maxAnswerCalls
        }
    = withSupervisor $ \sup ->
        handle
            (\StopVat -> pure ())
            $ bracket
                (newConn sup)
                stopConn
                runConn
  where
    newConn sup = do
        stableName <- makeStableName ()
        atomically $ do
            bootstrap <- getBootstrap cfg sup
            questionIdPool <- newIdPool maxQuestions
            exportIdPool <- newIdPool maxExports

            sendQ <- newTBQueue $ fromIntegral maxQuestions
            recvQ <- newTBQueue $ fromIntegral maxQuestions

            questions <- M.new
            answers <- M.new
            exports <- M.new
            imports <- M.new

            embargos <- M.new

            pure Conn
                { stableName
                , supervisor = sup
                , questionIdPool
                , exportIdPool
                , recvQ
                , sendQ
                , maxAnswerCalls
                , questions
                , answers
                , exports
                , imports
                , embargos
                , bootstrap
                , debugMode
                }
    runConn conn =
        coordinator conn
            `concurrently_` sendLoop transport conn
            `concurrently_` recvLoop transport conn
            `concurrently_` useBootstrap conn
    stopConn conn@Conn{bootstrap=Nothing} =
        pure ()
    stopConn conn@Conn{bootstrap=Just client} =
        atomically $ decRef client
    useBootstrap conn = case withBootstrap of
        Nothing -> pure ()
        Just f  -> atomically (requestBootstrap conn) >>= f (supervisor conn)


-- | A pool of ids; used when choosing identifiers for questions and exports.
newtype IdPool = IdPool (TVar [Word32])

-- | @'newIdPool' size@ creates a new pool of ids, with @size@ available ids.
newIdPool :: Word32 -> STM IdPool
newIdPool size = IdPool <$> newTVar [0..size-1]

-- | Get a new id from the pool. Retries if the pool is empty.
newId :: IdPool -> STM Word32
newId (IdPool pool) = readTVar pool >>= \case
    [] -> retrySTM
    (id:ids) -> do
        writeTVar pool $! ids
        pure id

-- | Return an id to the pool.
freeId :: IdPool -> Word32 -> STM ()
freeId (IdPool pool) id = modifyTVar' pool (id:)

newtype Question = Question
    { onReturn :: RpcGen.Return -> STM ()
    -- ^ Called when the remote vat sends a return message for this question.
    }

-- | An entry in our answers table.
data Answer
    -- | An answer entry for which we have neither received a finish, nor sent a
    -- return. Contains two callbacks, to invoke when we receive each type of
    -- message.
    = NewAnswer
        { onFinish :: RpcGen.Finish -> STM ()
        , onReturn :: RpcGen.Return -> STM ()
        }
    -- | An answer entry for which we've sent a return, but not received a
    -- finish. Contains the return message we sent, and a callback to invoke
    -- when we receive a finish.
    | HaveReturn
        { returnMsg :: RpcGen.Return
        , onFinish  :: RpcGen.Finish -> STM ()
        }
    -- | An answer entry for which we've received a finish, but not sen a
    -- return. Contains the finish message we received, and a callback to
    -- invoke when we send the return.
    | HaveFinish
        { finishMsg :: RpcGen.Finish
        , onReturn  :: RpcGen.Return -> STM ()
        }

data Export = Export
    { client   :: Client
    , refCount :: !Word32
    }

-- | An entry in our imports table.
data Import = Import
    { client   :: Client
    -- ^ The client. We cache it in the table so there's only one object
    -- floating around, which lets us attach a finalizer without worrying
    -- about it being run more than once.
    , refCount :: !Word32
    -- ^ The refcount as understood by the remote vat. This tells us what
    -- to put in a release message when we're ready to free the object.
    }

-- | Types which may be converted to and from 'Client's. Typically these
-- will be simple type wrappers for capabilities.
class IsClient a where
    -- | Convert a value to a client.
    toClient :: a -> Client
    -- | Convert a client to a value.
    fromClient :: Client -> a

instance Show Client where
    show NullClient = "nullClient"
    show _          = "({- capability; not statically representable -})"

-- | A reference to a capability, which may be live either in the current vat
-- or elsewhere. Holding a client affords making method calls on a capability
-- or modifying the local vat's reference count to it.
data Client
    -- | A client corresponding to a null pointer. Replies to all
    -- method calls with an "unimplemented" exception.
    = NullClient
    -- | A client pointing at a capability local to our own vat.
    | LocalClient
        { connRefs :: ConnRefs
        -- ^ Record of what export IDs this client has on different remote
        -- connections.
        , opQ      :: TQueue Server.ServerMsg
        -- ^ A queue of operations for the local capability to handle.
        , refCount :: TVar Word32
        -- ^ The number of live references to this capability. when this hits
        -- zero, we send a stop message to the server.
        }
    -- | A client which will resolve to some other capability at
    -- some point.
    | PromiseClient
        { pState :: TVar PromiseState
        -- ^ The current state of the promise; the indirection allows
        -- the promise to be updated.
        }
    -- | A client which points to a (resolved) capability in a remote vat.
    | ImportClient ImportRef

-- | The current state of a 'PromiseClient'.
data PromiseState
    -- | The promise is fully resolved.
    = Ready
        { target :: Client
        -- ^ Capability to which the promise resolved.
        }
    -- | The promise has resolved, but is waiting on a Disembargo message
    -- before it is safe to send it messages.
    | Embargo
        { callBuffer :: TQueue Server.CallInfo
        -- ^ A queue in which to buffer calls while waiting for the
        -- disembargo.
        }
    -- | The promise has not yet resolved.
    | Pending
        { tmpDest :: TmpDest
        -- ^ A temporary destination to send calls, while we wait for the
        -- promise to resolve.
        }
    -- | The promise resolved to an exception.
    | Error RpcGen.Exception

-- | A temporary destination for calls on an unresolved promise.
data TmpDest
    -- | Queue the calls locally, rather than sending them elsewhere.
    = LocalBuffer { callBuffer :: TQueue Server.CallInfo }
    -- | Send call messages to a remote vat, targeting the results
    -- of an outstanding question.
    | AnswerDest
        { conn      :: Conn
        -- ^ The connection to the remote vat.
        , answerId  :: !AnswerId
        -- ^ The answer to target.
        , transform :: DList Word16
        -- ^ A series of pointer indexes to follow from the result
        -- struct to the capability. This corresponds to MessageTarget's
        -- transform field in rpc.capnp.
        }
    -- | Send call messages to a remote vat, targeting an entry in our
    -- imports table.
    | ImportDest ImportRef

-- | A reference to a capability in our import table/a remote vat's export
-- table.
data ImportRef = ImportRef
    { conn     :: Conn
    -- ^ The connection to the remote vat.
    , importId :: !ImportId
    -- ^ The import id for this capability.
    , proxies  :: ConnRefs
    -- ^ Export ids to use when this client is passed to a vat other than
    -- the one identified by 'conn'. See Note [proxies]
    }

-- Ideally we could just derive these, but stm-containers doesn't have Eq
-- instances, so neither does ConnRefs. not all of the fields are actually
-- necessary to check equality though. See also
-- https://github.com/nikita-volkov/stm-hamt/pull/1
instance Eq ImportRef where
    ImportRef { conn=cx, importId=ix } == ImportRef { conn=cy, importId=iy } =
        cx == cy && ix == iy
instance Eq Client where
    NullClient == NullClient =
        True
    LocalClient { refCount = x } == LocalClient { refCount = y } =
        x == y
    PromiseClient { pState = x } == PromiseClient { pState = y } =
        x == y
    ImportClient x == ImportClient y =
        x == y
    _ == _ =
        False


-- | a 'ConnRefs' tracks a mapping from connections to export IDs; it is used
-- to ensure that we re-use export IDs for capabilities when passing them
-- to remote vats. This used for locally hosted capabilities, but also by
-- proxied imports (see Note [proxies]).
newtype ConnRefs = ConnRefs (M.Map Conn ExportId)

data MsgTarget
    = ImportTgt !ImportId
    | AnswerTgt
        { answerId  :: !AnswerId
        , transform :: DList Word16
        }

-- Note [proxies]
-- ==============
--
-- It is possible to have multiple connections open at once, and pass around
-- clients freely between them. Without level 3 support, this means that when
-- we pass a capability pointing into Vat A to another Vat B, we must proxy it.
--
-- To achieve this, capabilities pointing into a remote vat hold a 'ConnRefs',
-- which tracks which export IDs we should be using to proxy the client on each
-- connection.

-- | Queue a call on a client.
call :: Server.CallInfo -> Client -> STM ()
call info@Server.CallInfo { response } = \case
    NullClient ->
        breakPromise response eMethodUnimplemented

    LocalClient { opQ, refCount } -> do
        refs <- readTVar refCount
        if refs > 0
            then writeTQueue opQ (Server.Call info)
            else breakPromise response eDisconnected

    PromiseClient stVar -> readTVar stVar >>= \case
        Ready { target }  ->
            call info target

        Embargo { callBuffer } ->
            writeTQueue callBuffer info

        Pending { tmpDest } -> case tmpDest of
            LocalBuffer { callBuffer } ->
                writeTQueue callBuffer info

            AnswerDest { conn, answerId, transform } ->
                callRemote conn info AnswerTgt { answerId, transform }

            ImportDest ImportRef { conn, importId } ->
                callRemote conn info (ImportTgt importId)

        Error exn ->
            breakPromise response exn

    ImportClient ImportRef { conn, importId } ->
        callRemote conn info (ImportTgt importId)

-- | Send a call to a remote capability.
callRemote :: Conn -> Server.CallInfo -> MsgTarget -> STM ()
callRemote
        conn@Conn{ questions }
        Server.CallInfo{ interfaceId, methodId, arguments, response }
        target = do
    qid <- newQuestion conn
    capTable <- genSendableCapTableRaw conn arguments
    content <- evalLimitT defaultLimit (decerialize arguments)
    sendPureMsg conn $ RpcGen.Message'call def
        { RpcGen.questionId = qid
        , RpcGen.target = case target of
            ImportTgt importId ->
                RpcGen.MessageTarget'importedCap importId
            AnswerTgt { answerId, transform } ->
                RpcGen.MessageTarget'promisedAnswer
                    RpcGen.PromisedAnswer
                        { RpcGen.questionId = answerId
                        , RpcGen.transform = V.fromList $
                            map RpcGen.PromisedAnswer'Op'getPointerField $
                            DList.toList transform
                        }
        , RpcGen.params = RpcGen.Payload { content, capTable }
        , RpcGen.interfaceId = interfaceId
        , RpcGen.methodId = methodId
        }
    M.insert
        Question
            { onReturn = \RpcGen.Return{ union' } -> do
                case union' of
                    RpcGen.Return'exception exn ->
                        breakPromise response exn
                    RpcGen.Return'results RpcGen.Payload{ content } -> do
                        rawPtr <- createPure defaultLimit $ do
                            msg <- Message.newMessage Nothing
                            cerialize msg content
                        fulfill response rawPtr
                    _ ->
                        error "TODO: handle other variants."
                finishQuestion conn def
                    { RpcGen.questionId = qid
                    , RpcGen.releaseResultCaps = False
                    }
            }
        qid
        questions


-- | A null client. This is the only client value that can be represented
-- statically. Throws exceptions in response to all method calls.
nullClient :: Client
nullClient = NullClient

-- | Increment the reference count on a client.
incRef :: Client -> STM ()
incRef NullClient            = pure ()
incRef LocalClient{refCount} = modifyTVar' refCount (+1)
incRef _                     = error "TODO"


-- | Decrement the reference count on a client. If the count reaches zero,
-- the object is destroyed.
decRef :: Client -> STM ()
decRef NullClient = pure ()
decRef LocalClient{refCount, opQ} = do
    count <- readTVar refCount
    case count of
        0 -> pure ()
        1 -> do
            writeTVar refCount 0
            writeTQueue opQ Server.Stop
        n ->
            writeTVar refCount $! n - 1
decRef _ = error "TODO"

-- | Spawn a local server with its lifetime bound to the supervisor,
-- and return a client for it. When the client is garbage collected,
-- the server will be stopped (if it is still running).
export :: Supervisor -> Server.ServerOps IO -> STM Client
export sup ops = do
    opQ <- newTQueue
    refCount <- newTVar 1
    connRefs <- ConnRefs <$> M.new
    let client = LocalClient
            { refCount
            , opQ
            , connRefs
            }
    superviseSTM sup $ do
        addFinalizer client $
            atomically $ writeTQueue opQ Server.Stop
        Server.runServer opQ ops
    pure client

clientMethodHandler :: Word64 -> Word16 -> Client -> Server.MethodHandler IO p r
clientMethodHandler interfaceId methodId client =
    Server.fromUntypedHandler $ Server.untypedHandler $
        \arguments response -> atomically $ call Server.CallInfo{..} client

-- | 'sendLoop' shunts messages from the send queue into the transport.
sendLoop :: Transport -> Conn -> IO ()
sendLoop transport Conn{sendQ} =
    forever $ atomically (readTBQueue sendQ) >>= sendMsg transport

-- | 'recvLoop' shunts messages from the transport into the receive queue.
recvLoop :: Transport -> Conn -> IO ()
recvLoop transport Conn{recvQ} =
    forever $ recvMsg transport >>= atomically . writeTBQueue recvQ

-- | The coordinator processes incoming messages.
coordinator :: Conn -> IO ()
-- The logic here mostly routes messages to other parts of the code that know
-- more about the objects in question; See Note [Organization] for more info.
coordinator conn@Conn{recvQ,debugMode} = go
  where
    go = try (atomically handleMsg) >>= \case
        Right () ->
            go
        Left (SentAbort e) -> do
            atomically $ sendPureMsg conn $ RpcGen.Message'abort e
            -- Give the message a bit of time to reach the remote vat:
            threadDelay 1000000
            throwIO (SentAbort e)
        Left e ->
            throwIO e
    handleMsg = do
        msg <- readTBQueue recvQ
        pureMsg <- msgToValue msg
            `catchSTM`
            (abortConn conn . wrapException debugMode)
        case pureMsg of
            RpcGen.Message'abort exn ->
                handleAbortMsg conn exn
            RpcGen.Message'unimplemented msg ->
                handleUnimplementedMsg conn msg
            RpcGen.Message'bootstrap bs ->
                handleBootstrapMsg conn bs
            RpcGen.Message'call call ->
                handleCallMsg conn call msg
            RpcGen.Message'return ret ->
                handleReturnMsg conn ret msg
            RpcGen.Message'finish finish ->
                handleFinishMsg conn finish
            RpcGen.Message'release release ->
                handleReleaseMsg conn release
            _ ->
                sendPureMsg conn $ RpcGen.Message'unimplemented pureMsg

-- Each function handle*Msg handles a message of a particular type;
-- 'coordinator' dispatches to these.

handleAbortMsg :: Conn -> RpcGen.Exception -> STM ()
handleAbortMsg _ exn =
    throwSTM (ReceivedAbort exn)

handleUnimplementedMsg :: Conn -> RpcGen.Message -> STM ()
handleUnimplementedMsg conn = \case
    RpcGen.Message'unimplemented _ ->
        -- If the client itself doesn't handle unimplemented messages, that's
        -- weird, but ultimately their problem.
        pure ()
    RpcGen.Message'abort _ ->
        abortConn conn def
            { RpcGen.type_ = RpcGen.Exception'Type'failed
            , RpcGen.reason =
                "Your vat sent an 'unimplemented' message for an abort message " <>
                "that its remote peer never sent. This is likely a bug in your " <>
                "capnproto library."
            }
    _ ->
        abortConn conn def
            { RpcGen.type_ = RpcGen.Exception'Type'failed
            , RpcGen.reason = "Received unimplemented response for required message."
            }

handleBootstrapMsg :: Conn -> RpcGen.Bootstrap -> STM ()
handleBootstrapMsg conn RpcGen.Bootstrap{questionId} = do
    ret <- case bootstrap conn of
        Nothing ->
            pure $ RpcGen.Return
                { RpcGen.answerId = questionId
                , RpcGen.releaseParamCaps = True -- Not really meaningful for bootstrap, but...
                , RpcGen.union' =
                    RpcGen.Return'exception def
                        { RpcGen.type_ = RpcGen.Exception'Type'failed
                        , RpcGen.reason = "No bootstrap interface for this connection."
                        }
                }
        Just client -> do
            capDesc <- marshalCap conn client
            pure $ RpcGen.Return
                { RpcGen.answerId = questionId
                , RpcGen.releaseParamCaps = True -- Not really meaningful for bootstrap, but...
                , RpcGen.union' =
                    RpcGen.Return'results RpcGen.Payload
                            -- XXX: this is a bit fragile; we're relying on
                            -- the encode step to pick the right index for
                            -- our capability.
                        { content = Just (Untyped.PtrCap client)
                        , capTable = V.singleton capDesc
                        }
                }
    M.focus
        (Focus.alterM $ insertBootstrap ret)
        questionId
        (answers conn)
    sendPureMsg conn $ RpcGen.Message'return ret
  where
    insertBootstrap ret Nothing =
        pure $ Just HaveReturn
            { returnMsg = ret
            , onFinish = \_ -> pure ()
            }
    insertBootstrap _ (Just _) =
        abortConn conn def
            { RpcGen.type_ = RpcGen.Exception'Type'failed
            , RpcGen.reason = "Duplicate question ID"
            }

handleCallMsg :: Conn -> RpcGen.Call -> ConstMsg -> STM ()
handleCallMsg
        conn@Conn{exports, answers}
        RpcGen.Call
            { questionId
            , target
            , interfaceId
            , methodId
            , sendResultsTo
            , params=RpcGen.Payload{capTable}
            }
        msg = do
    -- First, add an entry in our answers table:
    insertNewAbort
        "answer"
        conn
        questionId
        NewAnswer
            { onReturn = \_ -> pure ()
            , onFinish = \_ -> pure ()
            }
        answers
    -- Next, fish out the parameters to the call, and make sure
    -- the capability table is set up:
    msgWithCaps <- fixCapTable capTable conn msg
    callParams <- evalLimitT defaultLimit $
        msgToValue msgWithCaps >>= \case
            RawRpc.Message'call rawCall ->
                RawRpc.get_Call'params rawCall
                    >>= RawRpc.get_Payload'content
            _ ->
                error "BUG: handleCallMsg was passed a non-call message!"

    -- Set up a callback for when the call is finished, to
    -- send the return message:
    fulfiller <- newCallback $ \case
        Left e ->
            returnAnswer conn def
                { RpcGen.answerId = questionId
                , RpcGen.releaseParamCaps = False
                , RpcGen.union' = RpcGen.Return'exception e
                }
        Right v -> do
            content <- evalLimitT defaultLimit (decerialize v)
            capTable <- genSendableCapTable conn content
            returnAnswer conn def
                { RpcGen.answerId = questionId
                , RpcGen.releaseParamCaps = False
                , RpcGen.union'   = RpcGen.Return'results def
                    { RpcGen.content  = content
                    , RpcGen.capTable = capTable
                    }
                }
    -- Package up the info for the call:
    let callInfo = Server.CallInfo
            { interfaceId
            , methodId
            , arguments = callParams
            , response = fulfiller
            }
    -- Finally, figure out where to send it:
    case target of
        RpcGen.MessageTarget'importedCap exportId ->
            lookupAbort "export" conn exports exportId $
                \Export{client} -> call callInfo client
        RpcGen.MessageTarget'promisedAnswer RpcGen.PromisedAnswer { questionId = targetQid, transform } ->
            lookupAbort "answer" conn answers targetQid $ \ans -> do
                ans' <- subscribeReturn ans $ \ret@RpcGen.Return{union'} ->
                    case union' of
                        RpcGen.Return'exception _ ->
                            returnAnswer conn ret { RpcGen.answerId = questionId }
                        RpcGen.Return'results RpcGen.Payload{content} ->
                            transformClient transform content conn >>= call callInfo
                        _ ->
                            error "TODO"
                M.insert ans' targetQid answers
        _ ->
            error "TODO"

transformClient
    :: V.Vector RpcGen.PromisedAnswer'Op
    -> Maybe Untyped.PtrType
    -> Conn
    -> STM Client
transformClient transform ptr conn =
    case followTransform transform ptr of
        Left e ->
            abortConn conn e
        Right Nothing ->
            pure nullClient
        Right (Just (Untyped.PtrCap client)) ->
            pure client
        Right (Just _) ->
            abortConn conn def
                { RpcGen.type_ = RpcGen.Exception'Type'failed
                , RpcGen.reason = "Tried to call method on non-capability."
                }

followTransform
    :: V.Vector RpcGen.PromisedAnswer'Op
    -> Maybe Untyped.PtrType
    -> Either RpcGen.Exception (Maybe Untyped.PtrType)
followTransform ops = go (V.toList ops)
  where
    go [] ptr = Right ptr
    go (RpcGen.PromisedAnswer'Op'noop:cs) ptr = go cs ptr
    go (RpcGen.PromisedAnswer'Op'getPointerField idx:cs) ptr = case ptr of
        Nothing -> go cs Nothing
        Just (Untyped.PtrStruct (Untyped.Struct _ ptrs)) ->
            go cs (Untyped.sliceIndex (fromIntegral idx) ptrs)
        Just _ ->
            Left def
                { RpcGen.type_ = RpcGen.Exception'Type'failed
                , RpcGen.reason = "Tried to access pointer field of non-struct."
                }
    go (RpcGen.PromisedAnswer'Op'unknown' op:_) ptr =
        Left def
            { RpcGen.type_ = RpcGen.Exception'Type'failed
            , RpcGen.reason = "Unknown PromisedAnswer.Op: " <> fromString (show op)
            }

-- | Follow a series of pointer indicies, returning the final value, or 'Left'
-- with an error if any of the pointers in the chain (except the last one) is
-- a non-null non struct.
--
-- TODO: use this in place of followTransform.
followPtrs
    :: [Word16]
    -> Maybe Untyped.PtrType
    -> Either RpcGen.Exception (Maybe Untyped.PtrType)
followPtrs [] ptr =
    Right ptr
followPtrs (_:_) Nothing =
    Right Nothing
followPtrs (i:is) (Just (Untyped.PtrStruct (Untyped.Struct _ ptrs))) =
    followPtrs is (Untyped.sliceIndex (fromIntegral i) ptrs)
followPtrs (_:_) (Just _) =
    Left (eFailed "Tried to access pointer field of non-struct.")

handleReturnMsg :: Conn -> RpcGen.Return -> ConstMsg -> STM ()
handleReturnMsg conn@Conn{questions} ret@RpcGen.Return{answerId, union'} msg = do
    ret <- case union' of
        RpcGen.Return'results RpcGen.Payload{capTable} -> do
            msgWithCaps <- fixCapTable capTable conn msg
            evalLimitT defaultLimit $
                msgToValue msgWithCaps >>= \case
                    RawRpc.Message'return rawRet ->
                        decerialize rawRet
                    _ ->
                        error "BUG: handleReturnMsg was passed a non-return message!"
        _ ->
            -- there's no payload, so we can just leave this as-is.
            pure ret
    lookupAbort "question" conn questions answerId $
        \Question{onReturn} -> onReturn ret

handleFinishMsg :: Conn -> RpcGen.Finish -> STM ()
handleFinishMsg conn@Conn{answers} finish@RpcGen.Finish{questionId} =
    lookupAbort "answer" conn answers questionId $ \case
        ans@NewAnswer{onFinish, onReturn} -> do
            onFinish finish
            M.insert
                HaveFinish
                    { finishMsg = finish
                    , onReturn
                    }
                questionId
                answers
        ans@HaveReturn{onFinish} -> do
            onFinish finish
            M.delete questionId answers
        HaveFinish{} ->
            abortConn conn def
                { RpcGen.type_ = RpcGen.Exception'Type'failed
                , RpcGen.reason =
                    "Duplicate finish message for question #"
                    <> fromString (show questionId)
                }

handleReleaseMsg :: Conn -> RpcGen.Release -> STM ()
handleReleaseMsg conn@Conn{exports} RpcGen.Release{id, referenceCount} =
    M.focus
        (Focus.alterM $ \case
            Nothing ->
                abortConn conn def
                    { RpcGen.type_ = RpcGen.Exception'Type'failed
                    , RpcGen.reason =
                        "No such export: " <> fromString (show id)
                    }
            Just Export{client, refCount} ->
                case compare refCount referenceCount of
                    LT ->
                        abortConn conn def
                            { RpcGen.type_ = RpcGen.Exception'Type'failed
                            , RpcGen.reason =
                                "Received release for export with referenceCount " <>
                                "greater than our recorded total ref count."
                            }
                    EQ -> do
                        decRef client
                        pure Nothing
                    GT ->
                        pure $ Just Export
                            { client
                            , refCount = refCount - referenceCount
                            }
        )
        id
        exports

-- | Interpret the list of cap descriptors, and replace the message's capability
-- table with the result.
fixCapTable :: V.Vector RpcGen.CapDescriptor -> Conn -> ConstMsg -> STM ConstMsg
fixCapTable capDescs conn msg = do
    clients <- traverse (unmarshalCap conn) capDescs
    pure $ Message.withCapTable clients msg

lookupAbort keyTypeName conn m key f = do
    result <- M.lookup key m
    case result of
        Just val ->
            f val
        Nothing ->
            abortConn conn def
                { RpcGen.type_ = RpcGen.Exception'Type'failed
                , RpcGen.reason = mconcat
                    [ "No such "
                    , keyTypeName
                    ,  ": "
                    , fromString (show key)
                    ]
                }

-- | @'insertNewAbort' keyTypeName conn key value stmMap@ inserts a key into a
-- map, aborting the connection if it is already present. @keyTypeName@ will be
-- used in the error message sent to the remote vat.
insertNewAbort :: (Eq k, Hashable k) => Text -> Conn -> k -> v -> M.Map k v -> STM ()
insertNewAbort keyTypeName conn key value =
    M.focus
        (Focus.alterM $ \case
            Just _ ->
                abortConn conn def
                    { RpcGen.type_ = RpcGen.Exception'Type'failed
                    , RpcGen.reason = "duplicate entry in " <> keyTypeName <> " table."
                    }
            Nothing ->
                pure (Just value)
        )
        key

-- | Generate a cap table describing the capabilities reachable from the given
-- pointer. The capability table will be correct for any message where all of
-- the capabilities are within the subtree under the pointer.
--
-- XXX: it's kinda gross that we're serializing the pointer just to collect
-- this, then decerializing to put it in the larger adt, then reserializing
-- again... at some point we'll probably want to overhaul much of this module
-- for performance. This kind of thing is the motivation for #52.
genSendableCapTable :: Conn -> Maybe Untyped.PtrType -> STM (V.Vector RpcGen.CapDescriptor)
genSendableCapTable conn ptr = do
    rawPtr <- createPure defaultLimit $ do
        msg <- Message.newMessage Nothing
        cerialize msg ptr
    genSendableCapTableRaw conn rawPtr

genSendableCapTableRaw
    :: Conn
    -> Maybe (UntypedRaw.Ptr ConstMsg)
    -> STM (V.Vector RpcGen.CapDescriptor)
genSendableCapTableRaw _ Nothing = pure V.empty
genSendableCapTableRaw conn (Just ptr) =
    traverse
        (marshalCap conn)
        (Message.getCapTable (UntypedRaw.message ptr))

sendPureMsg :: Conn -> RpcGen.Message -> STM ()
sendPureMsg Conn{sendQ} msg =
    createPure maxBound (valueToMsg msg) >>= writeTBQueue sendQ

-- | Send a finish message, and then remove the corresponding
-- question from our table, and return the question id to the
-- pool of available ids.
finishQuestion :: Conn -> RpcGen.Finish -> STM ()
finishQuestion conn@Conn{questions} finish@RpcGen.Finish{questionId} = do
    sendPureMsg conn $ RpcGen.Message'finish finish
    freeQuestion conn questionId
    M.delete questionId questions

-- | Send a return message, and update the corresponding entry in our
-- answers table, invoking any registered callbacks. Calls 'error' if
-- the answerId is not in the table, or if we've already sent a return
-- for this answer.
returnAnswer :: Conn -> RpcGen.Return -> STM ()
returnAnswer conn@Conn{answers} ret@RpcGen.Return{answerId} = do
    sendPureMsg conn $ RpcGen.Message'return ret
    M.focus
        (Focus.alterM $ \case
            Just NewAnswer{onFinish, onReturn} -> do
                onReturn ret
                pure $ Just HaveReturn
                    { returnMsg = ret
                    , onFinish
                    }
            Just HaveFinish{onReturn} -> do
                onReturn ret
                pure Nothing

            Just HaveReturn{} ->
                error "BUG: sent a second return"

            Nothing ->
                error "BUG: sent a return for non-existent answer"
        )
        answerId
        answers

-- | Update an entry in the answers table to run the given callback when
-- the return message for that answer comes in. If the return has already
-- arrived, the callback is run immediately.
--
-- If the answer already has other callbacks registered, this callback is
-- run *after* the others. Note that this is an important property, as it
-- is necessary to preserve E-order if the callbacks are successive method
-- calls on the returned object.
subscribeReturn :: Answer -> (RpcGen.Return -> STM ()) -> STM Answer
subscribeReturn ans onRet = case ans of
    NewAnswer{onFinish, onReturn} ->
        pure NewAnswer
            { onFinish
            , onReturn = \ret -> onReturn ret *> onRet ret
            }

    HaveFinish{finishMsg, onReturn} ->
        pure HaveFinish
            { finishMsg
            , onReturn = \ret -> onReturn ret *> onRet ret
            }

    val@HaveReturn{returnMsg} -> do
        onRet returnMsg
        pure val

abortConn :: Conn -> RpcGen.Exception -> STM a
abortConn _ e = throwSTM (SentAbort e)

{-
-- | Get a CapDescriptor for this client, suitable for sending to the remote
-- vat. If the client points to our own vat, this will increment the refcount
-- in the exports table, and will allocate a new export ID if needed. Returns
-- CapDescriptor'none if the client is 'nullClient'.
sendableCapDesc :: Conn -> Client -> STM RpcGen.CapDescriptor
sendableCapDesc _ (Client Nothing)  = pure RpcGen.CapDescriptor'none
sendableCapDesc conn@Conn{exports} client@(Client (Just clientVar)) =
    readTVar clientVar >>= \case
        LocalExportClient{exportIds} ->
            M.lookup conn exportIds >>= \case
                Just exportId -> do
                    -- This client is already exported on the connection; bump the
                    -- refcount and use the existing export id.
                    M.focus
                        (Focus.adjust $ \e@Export{refCount} ->
                            e { refCount = refCount + 1 } :: Export
                        )
                        exportId
                        exports
                    pure $ RpcGen.CapDescriptor'senderHosted exportId
                Nothing -> do
                    -- This client is not yet exported on this connection; allocate
                    -- a new export ID and insert it into the exports table. Also,
                    -- increment the client's refcount, to indicate that this
                    -- connection now holds a reference.
                    incRef client
                    exportId <- newExport conn
                    M.insert exportId conn exportIds
                    M.insert Export { client, refCount = 1 } exportId exports
                    pure $ RpcGen.CapDescriptor'senderHosted exportId

        -- TODO: other client types

-- The dual of sendableCapDesc; takes a cap descriptor and creates/fetches a client
-- from it. Bumps reference counts/modifies tables etc. as needed. CapDescriptor'none
-- returns 'nullClient'.
interpretCapDesc :: Conn -> RpcGen.CapDescriptor -> STM Client
interpretCapDesc conn@Conn{imports, exports, answers, maxAnswerCalls} = \case
    RpcGen.CapDescriptor'none ->
        pure nullClient
    RpcGen.CapDescriptor'senderHosted importId ->
        senderHostedOrPromise True importId
    RpcGen.CapDescriptor'senderPromise importId ->
        senderHostedOrPromise False importId
    RpcGen.CapDescriptor'receiverHosted exportId ->
        lookupAbort "export" conn exports exportId $
            \Export{client} ->
                -- TODO: we probably need to do some bookkeeping re: refcounts.
                pure client
    RpcGen.CapDescriptor'receiverAnswer RpcGen.PromisedAnswer{transform, questionId} -> do
        calls <- newTBQueue (fromIntegral maxAnswerCalls)
        client' <- newTVar LocalAnswerClient{transform, calls}
        lookupAbort "answer" conn answers questionId $ \ans ->
            subscribeReturn ans $ \ret@RpcGen.Return{union'} ->
                case union' of
                    RpcGen.Return'exception exn -> do
                        writeTVar client' (ExnClient exn)
                        flushTBQueue calls >>= traverse_
                            (\Server.CallInfo{response} -> breakPromise response exn)
                    RpcGen.Return'results RpcGen.Payload{content} -> do
                        finalClient <- transformClient transform content conn
                        getClient' finalClient >>= writeTVar client'
                        flushTBQueue calls >>= traverse_ (`call` finalClient)
                    -- TODO: other variants
        pure $ Client (Just client')
    other ->
        error $ "TODO: unsupported cap descriptor: " ++ show other
  where
    senderHostedOrPromise isResolved importId = do
        M.focus
            (Focus.alterM $ \case
                Nothing -> do
                    client <- Client . Just <$> newTVar RemoteClient
                        { remoteConn = conn
                        , msgTarget = ImportTgt{importId, isResolved}
                        }
                    -- TODO: set up a finalizer somehow.
                    pure $ Just Import
                        { client
                        , refCount = 1
                        }
                Just Import{refCount, client} ->
                    pure $ Just Import
                        { client
                        , refCount = refCount + 1
                        }
            )
            importId
            imports
        ret <- M.lookup importId imports
        case ret of
            Just Import{client} -> pure client
            Nothing             -> error "Impossible"
-}


-- | Request the remote vat's bootstrap interface.
requestBootstrap :: Conn -> STM Client
requestBootstrap conn@Conn{questions} = do
    qid <- newQuestion conn
    let tmpDest = AnswerDest
            { conn
            , answerId = qid
            , transform = mempty
            }
    pState <- newTVar Pending { tmpDest }
    sendPureMsg conn $
        RpcGen.Message'bootstrap def { RpcGen.questionId = qid }
    M.insert
        Question { onReturn = resolveClientReturn tmpDest (writeTVar pState) }
        qid
        questions
    pure PromiseClient { pState }

-- Note [resolveClient]
-- ====================
--
-- There are several functions resolveClient*, each of which resolves a
-- 'PromiseClient', which will previously have been in the 'Pending' state.
-- Each function accepts three parameters: the 'TmpDest' that the
-- pending promise had been targeting, a function for setting the new state,
-- and a thing to resolve the promise to. The type of the latter is specific
-- to each function.

-- | Resolve a promised client to an exception. See Note [resolveClient]
resolveClientExn :: TmpDest -> (PromiseState -> STM ()) -> RpcGen.Exception -> STM ()
resolveClientExn tmpDest resolve exn = do
    case tmpDest of
        LocalBuffer { callBuffer } -> do
            calls <- flushTQueue callBuffer
            traverse_
                (\Server.CallInfo{response} ->
                    breakPromise response exn)
                calls
        AnswerDest {} ->
            pure ()
        ImportDest imp ->
            pure () -- FIXME TODO: decrement the refcount for the import?
    resolve $ Error exn

-- Resolve a promised client to a pointer. If it is a non-null non-capability
-- pointer, it resolves to an exception. See Note [resolveClient]
resolveClientPtr :: TmpDest -> (PromiseState -> STM ()) -> Maybe Untyped.PtrType -> STM ()
resolveClientPtr tmpDest resolve ptr = case ptr of
    Nothing ->
        resolveClientClient tmpDest resolve nullClient
    Just (Untyped.PtrCap c) ->
        resolveClientClient tmpDest resolve c
    Just _ ->
        resolveClientExn tmpDest resolve $
            eFailed "Promise resolved to non-capability pointer"

-- | Resolve a promised client to another client. See Note [resolveClient]
resolveClientClient :: TmpDest -> (PromiseState -> STM ()) -> Client -> STM ()
resolveClientClient tmpDest resolve client =
    -- NB: make sure to handle embargos.
    error "TODO"

-- | Resolve a promised client to the result of a return. See Note [resolveClient]
resolveClientReturn :: TmpDest -> (PromiseState -> STM ()) -> RpcGen.Return -> STM ()
resolveClientReturn tmpDest resolve RpcGen.Return { union' } = case union' of
    RpcGen.Return'exception exn ->
        resolveClientExn tmpDest resolve exn
    RpcGen.Return'results RpcGen.Payload{ content } ->
        resolveClientPtr tmpDest resolve content
    _ ->
        error $
            "TODO: handle other return variants:\n" ++
            "   - canceled\n" ++
            "   - resultsSentElseWhere\n" ++
            "   - acceptFromThirdParty\n" ++
            "   - unknown\n"

getConnExport :: Conn -> ConnRefs -> Client -> STM ExportId
getConnExport = error "TODO"

marshalCap :: Conn -> Client -> STM RpcGen.CapDescriptor
marshalCap _conn NullClient =
    pure RpcGen.CapDescriptor'none
marshalCap conn client@LocalClient { connRefs } =
    RpcGen.CapDescriptor'senderHosted <$> getConnExport conn connRefs client
marshalCap conn PromiseClient { pState } =
    marshalPromiseCap conn pState
marshalCap remoteConn client@(ImportClient ImportRef { conn, proxies, importId })
    | conn == remoteConn =
        pure (RpcGen.CapDescriptor'receiverHosted importId)
    | otherwise =
        RpcGen.CapDescriptor'senderHosted <$> getConnExport conn proxies client

marshalPromiseCap :: Conn -> TVar PromiseState -> STM RpcGen.CapDescriptor
marshalPromiseCap = error "TODO"

unmarshalCap :: Conn -> RpcGen.CapDescriptor -> STM Client
unmarshalCap = error "TODO"


-- Note [Limiting resource usage]
-- =============================
--
-- N.B. much of this Note is future tense; the library is not yet robust against
-- resource useage attacks.
--
-- We employ various strategies to prevent remote vats from causing excessive
-- resource usage. In particular:
--
-- * We set a maximum size for incoming messages; this is in keeping with how
--   we mitigate these concerns when dealing with plain capnp data (i.e. not
--   rpc).
-- * We set a limit on the total *size* of all messages from the remote vat that
--   are currently being serviced. For example, if a Call message comes in,
--   we note its size, and deduct it from the quota. Once we have sent a return
--   and received a finish for this call, and thus can safely forget about it,
--   we remove it from our answers table, and add its size back to the available
--   quota.
--
-- Still TBD:
--
-- * We should come up with some way of guarding against too many intra-vat calls;
--   depending on the object graph, it may be possible for an attacker to get us
--   to "eat our own tail" so to speak.
--
--   Ideas:
--     * Per-object bounded queues for messages
--     * Global limit on intra-vat calls.
--
--   Right now I(zenhack) am more fond of the former.
--
-- * What should we actually do when limits are exceeded?
--
--   Possible strategies:
--     * Block
--     * Throw an 'overloaded' exception
--     * Some combination of the two; block with a timeout, then throw.
--
--   If we just block, we need to make sure this doesn't hang the vat;
--   we probably need a timeout at some level.

#ifdef SELF_TESTS
selfTests :: Spec
selfTests = pure ()
#endif
