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
    ) where

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
import Control.Concurrent.STM (catchSTM, throwSTM)
import Control.Monad          (forever, when)
import Data.Default           (Default(def))
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

import qualified Data.Vector       as V
import qualified Focus
import qualified StmContainers.Map as M

import Capnp.Classes        (cerialize, decerialize)
import Capnp.Convert        (msgToValue, valueToMsg)
import Capnp.Message        (ConstMsg)
import Capnp.Promise        (breakPromise, fulfill, newCallback)
import Capnp.Rpc.Transport  (Transport(recvMsg, sendMsg))
import Capnp.Rpc.Util       (wrapException)
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
    deriving(Show, Generic)

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

    , questions      :: M.Map QuestionId Question
    , answers        :: M.Map AnswerId Answer
    , exports        :: M.Map ExportId Export
    , imports        :: M.Map ImportId Import

    , embargos       :: M.Map EmbargoId (STM ())
    -- Outstanding embargos. When we receive a 'Disembargo' message with its
    -- context field set to receiverLoopback, we look up the embargo id in
    -- this table, and execute the STM we have registered.

    , bootstrap      :: Client
    -- The capability which should be served as this connection's bootstrap
    -- interface.

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
    { maxQuestions  :: !Word32
    -- ^ The maximum number of simultanious outstanding requests to the peer
    -- vat. Once this limit is reached, further questsions will block until
    -- some of the existing questions have been answered.
    --
    -- Defaults to 32.

    , maxExports    :: !Word32
    -- ^ The maximum number of objects which may be exported on this connection.
    --
    -- Defaults to 32.

    , debugMode     :: !Bool
    -- ^ In debug mode, errors reported by the RPC system to its peers will
    -- contain extra information. This should not be used in production, as
    -- it is possible for these messages to contain sensitive information,
    -- but it can be useful for debugging.
    --
    -- Defaults to 'False'.

    , getBootstrap  :: Supervisor -> STM Client
    -- ^ Get the bootstrap interface we should serve for this connection.
    -- the argument is a supervisor whose lifetime is bound to the
    -- connection. If 'getBootstrap' returns 'nullClient', we will respond
    -- to bootstrap messages with an exception.
    --
    -- The default always returns 'nullClient'.

    , withBootstrap :: Maybe (Supervisor -> Client -> IO ())
    -- ^ An action to perform with access to the remote vat's bootstrap
    -- interface. The supervisor argument is bound to the lifetime of the
    -- connection. If this is 'Nothing' (the default), the bootstrap
    -- interface will not be requested.
    }

instance Default ConnConfig where
    def = ConnConfig
        { maxQuestions  = 32
        , maxExports    = 32
        , debugMode     = False
        , getBootstrap  = \_ -> pure nullClient
        , withBootstrap = Nothing
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
        }
    = withSupervisor $ \sup -> do
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
    stopConn conn =
        atomically $ decRef (bootstrap conn)
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


-- Note [Client representation]
-- ============================
--
-- A client is a reference to a capability, which can be used to
-- call methods on an object. The implementation is composed of two
-- types, Client and Client'. Only the former is exposed by the API.
-- Client contains a @TVar Client'@ (or Nothing if it is a null
-- client).
--
-- The reason for the indirection is so that we can swap out the
-- implementation. Some examples of when this is useful include:
--
-- * When a promise resolves, we want to redirect it to the thing it
--   resolved to.
-- * When a connection is dropped, we replace the relevant clients
--   with ones that always throw disconnected exceptions.
--
-- The reason for not using the TVar to represent null clients is so
-- that we can define the top-level definition 'nullClient', which
-- can be used statically. If the value 'nullClient' included a 'TVar',
-- we would have to create it at runtime.


-- | An untyped capability on which methods may be called.
newtype Client =
    -- See Note [Client representation]
    Client (Maybe (TVar Client'))
    deriving(Eq)

-- | Types which may be converted to and from 'Client's. Typically these
-- will be simple type wrappers for capabilities.
class IsClient a where
    -- | Convert a value to a client.
    toClient :: a -> Client
    -- | Convert a client to a value.
    fromClient :: Client -> a

instance Show Client where
    show (Client Nothing) = "nullClient"
    show (Client (Just _)) = "({- capability; not statically representable -})"

-- See Note [Client representation]
data Client'
    -- | A client which always throws an exception in response
    -- to calls.
    = ExnClient RpcGen.Exception
    -- | A client which lives in the same vat/process as us.
    | LocalClient
        { refCount  :: TVar Word32
        -- ^ The number of live references to this object. When this
        -- reaches zero, we will tell the server to stop.
        , opQueue   :: TQueue Server.ServerMsg
        -- ^ A queue for submitting commands to the server thread managing
        -- the object.
        , exportIds :: M.Map Conn ExportId
        -- ^ A the ids under which this is exported on each connection.
        }
    -- | A client for an object that lives in a remote vat.
    | RemoteClient
        { remoteConn :: Conn
        -- ^ The connection to the vat where the object lives.
        , msgTarget  :: MsgTarget
        -- ^ The address of the object in the remote vat.
        }

-- | The destination of a remote method call. This is closely related to
-- the 'MessageTarget' type defined in rpc.capnp, but has a couple
-- differences:
--
-- * It does not have an unknown' variant, which is more convienent to work
--   with. See also issue #60.
-- * In the case of an imported capability, it records whether the capability
--   is an unresolved promise (answers are always unresolved by definition).
data MsgTarget
    = AnswerTgt !AnswerId
    -- ^ Targets an entry in the remote vat's answers table/local vat's
    -- questions table.
    | ImportTgt
        { importId   :: !ImportId
        -- ^ Targets an entry in the remote vat's export table/local vat's
        -- imports table.
        , isResolved :: !Bool
        -- ^ Records whether the capability has resolved to its final value.
        -- This is True iff the target is not a promise. If it is an unresolved
        -- promise, this will be false. When the promise resolves, clients using
        -- this message target will have their target replaced with the target
        -- to which the promise resolved, so a client should never actually point
        -- at a promise which has already resolved.
        }

-- | A null client. This is the only client value that can be represented
-- statically. Throws exceptions in response to all method calls.
nullClient :: Client
nullClient = Client Nothing

nullClient' :: Client'
nullClient' = ExnClient def
    { RpcGen.type_ = RpcGen.Exception'Type'failed
    , RpcGen.reason = "Client is null"
    }

-- | A client that is disconnected; always throws disconnected exceptions.
disconnectedClient' :: Client'
disconnectedClient' = ExnClient def
    { RpcGen.type_ = RpcGen.Exception'Type'disconnected
    , RpcGen.reason = "disconnected"
    }

-- | Increment the reference count on a client.
incRef :: Client -> STM ()
incRef (Client Nothing) = pure ()
incRef (Client (Just clientVar)) = readTVar clientVar >>= \case
    ExnClient _ ->
        pure ()

    LocalClient{refCount} ->
        modifyTVar' refCount succ

    -- TODO: RemoteClient


-- | Decrement the reference count on a client. If the count reaches zero,
-- the object is destroyed.
decRef :: Client -> STM ()
decRef (Client Nothing) = pure ()
decRef (Client (Just clientVar)) = readTVar clientVar >>= \case
    ExnClient _ ->
        pure ()

    LocalClient{refCount, opQueue} -> do
        modifyTVar' refCount pred
        cnt <- readTVar refCount
        when (cnt == 0) $ do
            -- Refcount is zero. Tell the server to stop:
            writeTQueue opQueue Server.Stop
            -- ...and then replace ourselves with a disconnected client:
            writeTVar clientVar disconnectedClient'

    -- TODO: RemoteClient


-- | Call a method on the object pointed to by this client.
call :: Server.CallInfo -> Client -> STM ()
call info (Client Nothing) =
    breakPromise (Server.response info) def
        { RpcGen.type_ = RpcGen.Exception'Type'failed
        , RpcGen.reason = "Client is null"
        }
call info@Server.CallInfo{interfaceId, methodId} (Client (Just clientVar)) =
    readTVar clientVar >>= \case
        ExnClient e ->
            breakPromise (Server.response info) e

        LocalClient{opQueue} ->
            writeTQueue opQueue (Server.Call info)

        RemoteClient{remoteConn, msgTarget} -> do
            questionId <- newQuestion remoteConn
            let caps = case Server.arguments info of
                    Nothing  -> V.empty
                    Just ptr -> Message.getCapTable (UntypedRaw.message ptr)
            capTable <- traverse (sendableCapDesc remoteConn) caps
            content <- evalLimitT defaultLimit $ decerialize (Server.arguments info)
            sendPureMsg remoteConn $ RpcGen.Message'call def
                { RpcGen.questionId = questionId
                , RpcGen.target = case msgTarget of
                    AnswerTgt answerId ->
                        RpcGen.MessageTarget'promisedAnswer
                            RpcGen.PromisedAnswer
                                { RpcGen.questionId = answerId
                                , RpcGen.transform = V.empty
                                }
                    ImportTgt {importId} ->
                        RpcGen.MessageTarget'importedCap importId
                , RpcGen.params = RpcGen.Payload
                    { RpcGen.content = content
                    , RpcGen.capTable = capTable
                    }
                , RpcGen.interfaceId = interfaceId
                , RpcGen.methodId = methodId
                }
            M.insert
                Question
                    { onReturn = \RpcGen.Return{union'} -> case union' of
                        RpcGen.Return'exception exn -> do
                            breakPromise (Server.response info) exn
                            writeTVar clientVar (ExnClient exn)
                            finishQuestion remoteConn def { RpcGen.questionId = questionId }
                        RpcGen.Return'results RpcGen.Payload{content} -> do
                            -- TODO: we need to initialize the cap table.
                            rawPtr <-  createPure defaultLimit $ do
                                msg <- Message.newMessage Nothing
                                cerialize msg content
                            fulfill (Server.response info) rawPtr
                        _ ->
                            error "TODO: handle other variants."
                    }
                questionId
                (questions remoteConn)

-- | Spawn a local server with its lifetime bound to the supervisor,
-- and return a client for it. When the client is garbage collected,
-- the server will be stopped (if it is still running).
export :: Supervisor -> Server.ServerOps IO -> STM Client
export sup ops = do
    q <- newTQueue
    refCount <- newTVar 1
    exportIds <- M.new
    let client' = LocalClient
            { refCount = refCount
            , opQueue = q
            , exportIds
            }
    superviseSTM sup $ do
        addFinalizer client' $
            atomically $ writeTQueue q Server.Stop
        Server.runServer q ops
    Client . Just <$> newTVar client'

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
    go = do
        try (atomically handleMsg) >>= \case
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
            _ ->
                error "TODO"

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
    _ ->
        error "TODO"

handleBootstrapMsg :: Conn -> RpcGen.Bootstrap -> STM ()
handleBootstrapMsg conn RpcGen.Bootstrap{questionId} = do
    capDesc <- sendableCapDesc conn (bootstrap conn)
    let ret = RpcGen.Return
            { RpcGen.answerId = questionId
            , RpcGen.releaseParamCaps = True -- Not really meaningful for bootstrap, but...
            , RpcGen.union' = case capDesc of
                RpcGen.CapDescriptor'none ->
                    RpcGen.Return'exception def
                        { RpcGen.type_ = RpcGen.Exception'Type'failed
                        , RpcGen.reason = "No bootstrap interface for this connection."
                        }
                _ ->
                    RpcGen.Return'results RpcGen.Payload
                            -- XXX: this is a bit fragile; we're relying on
                            -- the encode step to pick the right index for
                            -- our capability.
                        { content = Just $ Untyped.PtrCap (bootstrap conn)
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
                , RpcGen.union' = RpcGen.Return'exception e
                }
        Right v -> do
            content <- evalLimitT defaultLimit (decerialize v)
            capTable <- genSendableCapTable conn content
            returnAnswer conn def
                { RpcGen.answerId = questionId
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
            M.lookup exportId exports >>= \case
                Nothing ->
                    abortConn conn def
                        { RpcGen.type_ = RpcGen.Exception'Type'failed
                        , RpcGen.reason = "No such export ID."
                        }
                Just Export{client} ->
                    call callInfo client
        RpcGen.MessageTarget'promisedAnswer
            RpcGen.PromisedAnswer { questionId = targetQid, transform }
                | V.length transform /= 0 ->
                    error "TODO: handle transforms"
                -- Set up a callback to run once the answer is available:
                | otherwise -> M.focus
                    (Focus.alterM $ subscribeReturn $ \ret@RpcGen.Return{union'} ->
                        case union' of
                            RpcGen.Return'exception _ ->
                                returnAnswer conn ret { RpcGen.answerId = questionId }
                            RpcGen.Return'results RpcGen.Payload{content=Nothing} ->
                                call callInfo nullClient
                            RpcGen.Return'results RpcGen.Payload
                                { content=Just (Untyped.PtrCap client) } ->
                                    call callInfo client
                            RpcGen.Return'results RpcGen.Payload{content=Just _} ->
                                abortConn conn def
                                    { RpcGen.type_ = RpcGen.Exception'Type'failed
                                    , RpcGen.reason = "Tried to call method on non-capability."
                                    }
                            -- TODO: other variants
                    )
                    targetQid
                    answers
        _ ->
            error "TODO"

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

-- | Interpret the list of cap descriptors, and replace the message's capability
-- table with the result.
fixCapTable :: V.Vector RpcGen.CapDescriptor -> Conn -> ConstMsg -> STM ConstMsg
fixCapTable capDescs conn msg = do
    clients <- traverse (interpretCapDesc conn) capDescs
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
insertNewAbort keyTypeName conn key value stmMap =
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
        stmMap

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
    msg <- createPure defaultLimit $ valueToMsg $ Untyped.Struct
        (Untyped.Slice V.empty)
        (Untyped.Slice $ V.singleton ptr)
    traverse (sendableCapDesc conn) (Message.getCapTable msg)

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
subscribeReturn :: (RpcGen.Return -> STM ()) -> Maybe Answer -> STM (Maybe Answer)
subscribeReturn onRet = \case
    Just NewAnswer{onFinish, onReturn} ->
        pure $ Just NewAnswer
            { onFinish
            , onReturn = \ret -> onReturn ret *> onRet ret
            }

    Just HaveFinish{finishMsg, onReturn} ->
        pure $ Just HaveFinish
            { finishMsg
            , onReturn = \ret -> onReturn ret *> onRet ret
            }

    val@(Just HaveReturn{returnMsg}) -> do
        onRet returnMsg
        pure val

    Nothing ->
        error "BUG: tried to subscribe to return for non-existent answer."

abortConn :: Conn -> RpcGen.Exception -> STM a
abortConn _ e = throwSTM (SentAbort e)

-- | Get a CapDescriptor for this client, suitable for sending to the remote
-- vat. If the client points to our own vat, this will increment the refcount
-- in the exports table, and will allocate a new export ID if needed. Returns
-- CapDescriptor'none if the client is 'nullClient'.
sendableCapDesc :: Conn -> Client -> STM RpcGen.CapDescriptor
sendableCapDesc _ (Client Nothing)  = pure RpcGen.CapDescriptor'none
sendableCapDesc conn@Conn{exports} client@(Client (Just clientVar)) =
    readTVar clientVar >>= \case
        LocalClient{exportIds} ->
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
                    -- a new export ID and insert it into the exports table.
                    exportId <- newExport conn
                    M.insert exportId conn exportIds
                    M.insert Export { client, refCount = 1 } exportId exports
                    pure $ RpcGen.CapDescriptor'senderHosted exportId

        -- TODO: other client types

-- The dual of sendableCapDesc; takes a cap descriptor and creates/fetches a client
-- from it. Bumps reference counts/modifies tables etc. as needed. CapDescriptor'none
-- returns 'nullClient'.
interpretCapDesc :: Conn -> RpcGen.CapDescriptor -> STM Client
interpretCapDesc conn@Conn{imports} = \case
    RpcGen.CapDescriptor'none ->
        pure nullClient

    RpcGen.CapDescriptor'senderHosted importId -> do
        M.focus
            (Focus.alterM $ \case
                Nothing -> do
                    client <- Client . Just <$> newTVar RemoteClient
                        { remoteConn = conn
                        , msgTarget = ImportTgt
                            { importId
                            , isResolved = True
                            }
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
            Just Import{client} ->  pure client
            Nothing             -> error "Impossible"

    -- TODO: other variants

-- | Request the remote vat's bootstrap interface.
requestBootstrap :: Conn -> STM Client
requestBootstrap conn = do
    qid <- newQuestion conn
    client' <- newTVar RemoteClient
            { remoteConn = conn
            , msgTarget = AnswerTgt qid
            }
    sendPureMsg conn $
        RpcGen.Message'bootstrap def { RpcGen.questionId = qid }
    M.insert
        Question
            { onReturn = \ret@RpcGen.Return{union'} -> do
                case union' of
                    RpcGen.Return'exception exn ->
                        writeTVar client' (ExnClient exn)
                    RpcGen.Return'results RpcGen.Payload{content} ->
                        case content of
                            Nothing ->
                                -- XXX: this really should be nullClient, but
                                -- we can't swap that in. This is not pretty
                                -- but close enough:
                                writeTVar client' nullClient'
                            Just (Untyped.PtrCap (Client Nothing)) ->
                                writeTVar client' nullClient'
                            Just (Untyped.PtrCap (Client (Just newClientVar))) ->
                                -- TODO FIXME: deal with embargos as necessary.
                                -- Also, when dealing with finalization of clients,
                                -- we need to think about how moving the underlying
                                -- Client' will affect things.
                                readTVar newClientVar >>= writeTVar client'
                            Just _ ->
                                abortConn conn (def
                                    { RpcGen.type_ = RpcGen.Exception'Type'failed
                                    , RpcGen.reason =
                                        "Bootstrap message resolved to a non-capability pointer."
                                    } :: RpcGen.Exception)
                    _ ->
                        error "TODO"
                finishQuestion conn def { RpcGen.questionId = qid }
            }
        qid
        (questions conn)
    pure $ Client (Just client')


-- Note [Limiting resource usage]
-- =============================
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
