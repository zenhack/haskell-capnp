{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
-- Module: Capnp.Rpc.Untyped
-- Description: Core of the RPC subsystem.
--
-- This module does not deal with schema-level concepts; all capabilities,
-- methods etc. as used here are untyped.
module Capnp.Rpc.Untyped
    (
    -- * Connections to other vats
      ConnConfig(..)
    , handleConn

    -- * Clients for capabilities
    , Client
    , call
    , nullClient
    , newPromiseClient
    , newPromiseClientSTM

    , IsClient(..)

    -- * Exporting local objects
    , export
    , clientMethodHandler

    -- * Errors
    , RpcError(..)
    , R.Exception(..)
    , R.Exception'Type(..)

    -- * Shutting down the connection
    ) where

import Control.Concurrent.STM
import Data.Word

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (concurrently_, race_)
import Control.Concurrent.MVar  (MVar, newEmptyMVar)
import Control.Exception.Safe   (Exception, bracket, throwIO, try)
import Control.Monad            (forever, void, when)
import Data.Default             (Default(def))
import Data.Foldable            (for_, toList, traverse_)
import Data.Hashable            (Hashable, hash, hashWithSalt)
import Data.Maybe               (catMaybes)
import Data.String              (fromString)
import Data.Text                (Text)
import GHC.Generics             (Generic)
import Supervisors              (Supervisor, superviseSTM, withSupervisor)
import System.Mem.StableName    (StableName, hashStableName, makeStableName)
import System.Timeout           (timeout)

import qualified Data.Vector       as V
import qualified Focus
import qualified ListT
import qualified StmContainers.Map as M

import Capnp.Classes        (cerialize, decerialize)
import Capnp.Convert        (msgToValue, valueToMsg)
import Capnp.Message        (ConstMsg)
import Capnp.Rpc.Errors
    ( eDisconnected
    , eFailed
    , eMethodUnimplemented
    , eUnimplemented
    , wrapException
    )
import Capnp.Rpc.Promise
    (Fulfiller, breakPromiseSTM, fulfillSTM, newCallbackSTM)
import Capnp.Rpc.Transport  (Transport(recvMsg, sendMsg))
import Capnp.TraversalLimit (defaultLimit, evalLimitT)
import Internal.BuildPure   (createPure)
import Internal.Rc          (Rc)
import Internal.SnocList    (SnocList)

import qualified Capnp.Gen.Capnp.Rpc.Pure as R
import qualified Capnp.Message            as Message
import qualified Capnp.Rpc.Server         as Server
import qualified Capnp.Untyped            as UntypedRaw
import qualified Capnp.Untyped.Pure       as Untyped
import qualified Internal.Finalizer       as Fin
import qualified Internal.Rc              as Rc
import qualified Internal.SnocList        as SnocList
import qualified Internal.TCloseQ         as TCloseQ

-- Note [Organization]
-- ===================
--
-- As much as possible, the logic in this module is centralized according to
-- type types of objects it concerns.
--
-- As an example, consider how we handle embargos: The 'Conn' type's 'embargos'
-- table has values that are just 'Fulfiller's. This allows the code which triggers
-- sending embargoes to have full control over what happens when they return,
-- while the code that routes incoming messages (in 'coordinator') doesn't need
-- to concern itself with the details of embargos -- it just needs to route them
-- to the right place.
--
-- This approach generally results in better separation of concerns.

-- Note [Level 3]
--
-- This is currently a level 1 implementation, so use of most level 3 features
-- results in sending abort messages. However, to make adding this support
-- easier later, we mark such places with a cross-reference back to this note.
--
-- In addition to filling in those spots, the following will need to be dealt
-- with:
--
-- * The "Tribble 4-way Race Condition" as documented in rpc.capnp. This
--   doesn't affect level 1 implementations, but right now we shorten N-hop
--   paths of promises to 1-hop, (calls on Ready PromiseClients just
--   immediately call the target), which is unsafe in a level 3
--   implementation. See the protocol documentation for more info.

-- | We use this type often enough that the types get noisy without a shorthand:
type MPtr = Maybe Untyped.Ptr
-- | Less often, but still helpful:
type RawMPtr = Maybe (UntypedRaw.Ptr ConstMsg)


-- | Errors which can be thrown by the rpc system.
data RpcError
    = ReceivedAbort R.Exception
    -- ^ The remote vat sent us an abort message.
    | SentAbort R.Exception
    -- ^ We sent an abort to the remote vat.
    deriving(Show, Eq, Generic)

instance Exception RpcError

newtype EmbargoId = EmbargoId { embargoWord :: Word32 } deriving(Eq, Hashable)
newtype QAId = QAId { qaWord :: Word32 } deriving(Eq, Hashable)
newtype IEId = IEId { ieWord :: Word32 } deriving(Eq, Hashable)

-- We define these to just show the number; the derived instances would include
-- data constructors, which is a bit weird since these show up in output that
-- is sometimes show to users.
instance Show QAId where
    show = show . qaWord
instance Show IEId where
    show = show . ieWord

-- | A connection to a remote vat
data Conn = Conn
    { stableName :: StableName (MVar ())
    -- So we can use the connection as a map key. The MVar used to create
    -- this is just an arbitrary value; the only property we care about
    -- is that it is distinct for each 'Conn', so we use something with
    -- reference semantics to guarantee this.

    , debugMode  :: !Bool
    -- whether to include extra (possibly sensitive) info in error messages.

    , liveState  :: TVar LiveState
    }

data LiveState
    = Live Conn'
    | Dead

data Conn' = Conn'
    { sendQ            :: TBQueue ConstMsg
    , recvQ            :: TBQueue ConstMsg
    -- queues of messages to send and receive; each of these has a dedicated
    -- thread doing the IO (see 'sendLoop' and 'recvLoop'):

    , supervisor       :: Supervisor
    -- Supervisor managing the lifetimes of threads bound to this connection.

    , questionIdPool   :: IdPool
    , exportIdPool     :: IdPool
    -- Pools of identifiers for new questions and exports

    , questions        :: M.Map QAId EntryQA
    , answers          :: M.Map QAId EntryQA
    , exports          :: M.Map IEId EntryE
    , imports          :: M.Map IEId EntryI

    , embargos         :: M.Map EmbargoId (Fulfiller ())
    -- Outstanding embargos. When we receive a 'Disembargo' message with its
    -- context field set to receiverLoopback, we look up the embargo id in
    -- this table, and fulfill the promise.

    , pendingCallbacks :: TQueue (IO ())
    -- See Note [callbacks]

    , bootstrap        :: Maybe Client
    -- The capability which should be served as this connection's bootstrap
    -- interface (if any).
    }

instance Eq Conn where
    x == y = stableName x == stableName y

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
    -- Defaults to 128.

    , maxExports    :: !Word32
    -- ^ The maximum number of objects which may be exported on this connection.
    --
    -- Defaults to 8192.

    , debugMode     :: !Bool
    -- ^ In debug mode, errors reported by the RPC system to its peers will
    -- contain extra information. This should not be used in production, as
    -- it is possible for these messages to contain sensitive information,
    -- but it can be useful for debugging.
    --
    -- Defaults to 'False'.

    , getBootstrap  :: Supervisor -> STM (Maybe Client)
    -- ^ Get the bootstrap interface we should serve for this connection.
    -- the argument is a supervisor whose lifetime is bound to the
    -- connection. If 'getBootstrap' returns 'Nothing', we will respond
    -- to bootstrap messages with an exception.
    --
    -- The default always returns 'Nothing'.
    --
    -- 'getBootstrap' MUST NOT block; the connection will not be serviced
    -- and 'withBootstrap' will not be run until this returns. If you need
    -- to supply the bootstrap interface later, use 'newPromiseClient'.

    , withBootstrap :: Maybe (Supervisor -> Client -> IO ())
    -- ^ An action to perform with access to the remote vat's bootstrap
    -- interface. The supervisor argument is bound to the lifetime of the
    -- connection. If this is 'Nothing' (the default), the bootstrap
    -- interface will not be requested.
    }

instance Default ConnConfig where
    def = ConnConfig
        { maxQuestions   = 128
        , maxExports     = 8192
        , debugMode      = False
        , getBootstrap   = \_ -> pure Nothing
        , withBootstrap  = Nothing
        }

-- | Queue an IO action to be run some time after this transaction commits.
-- See Note [callbacks].
queueIO :: Conn' -> IO () -> STM ()
queueIO Conn'{pendingCallbacks} = writeTQueue pendingCallbacks

-- | Queue another transaction to be run some time after this transaction
-- commits, in a thread bound to the lifetime of the connection. If this is
-- called multiple times within the same transaction, each of the
-- transactions will be run separately, in the order they were queued.
--
-- See Note [callbacks]
queueSTM :: Conn' -> STM () -> STM ()
queueSTM conn = queueIO conn . atomically

-- | @'mapQueueSTM' conn fs val@ queues the list of transactions obtained
-- by applying each element of @fs@ to @val@.
mapQueueSTM :: Conn' -> SnocList (a -> STM ()) -> a -> STM ()
mapQueueSTM conn fs x = traverse_ (\f -> queueSTM conn (f x)) fs

-- Note [callbacks]
-- ================
--
-- There are many places where we want to register some code to run after
-- some later event has happened -- for exmaple:
--
-- * We send a Call to the remote vat, and when a corresponding Return message
--   is received, we want to fulfill (or break) the local promise for the
--   result.
-- * We send a Disembargo (with senderLoopback set), and want to actually lift
--   the embargo when the corresponding (receiverLoopback) message arrives.
--
-- Keeping the two parts of these patterns together tends to result in better
-- separation of concerns, and is easier to maintain.
--
-- To achieve this, the four tables and other connection state have fields in
-- which callbacks can be registered -- for example, an outstanding question has
-- fields containing transactions to run when the return and/or finish messages
-- arrive.
--
-- When it is time to actually run these, we want to make sure that each of them
-- runs as their own transaction. If, for example, when registering a callback to
-- run when a return message is received, we find that the return message is
-- already available, it might be tempting to just run the transaction immediately.
-- But this means that the synchronization semantics are totally different from the
-- case where the callback really does get run later!
--
-- In addition, we sometimes want to register a finalizer inside a transaction,
-- but this can only be done in IO.
--
-- To solve these issues, the connection maintains a queue of all callback actions
-- that are ready to run, and when the event a callback is waiting for occurs, we
-- simply move the callback to the queue, using 'queueIO' or 'queueSTM'. When the
-- connection starts up, it creates a thread running 'callbacksLoop', which just
-- continually flushes the queue, running the actions in the queue.

-- | Get a new question id. retries if we are out of available question ids.
newQuestion :: Conn' -> STM QAId
newQuestion = fmap QAId . newId . questionIdPool

-- | Return a question id to the pool of available ids.
freeQuestion :: Conn' -> QAId -> STM ()
freeQuestion conn = freeId (questionIdPool conn) . qaWord

-- | Get a new export id. retries if we are out of available export ids.
newExport :: Conn' -> STM IEId
newExport = fmap IEId . newId . exportIdPool

-- | Return a export id to the pool of available ids.
freeExport :: Conn' -> IEId -> STM ()
freeExport conn = freeId (exportIdPool conn) . ieWord

-- | Get a new embargo id. This shares the same pool as questions.
newEmbargo :: Conn' -> STM EmbargoId
newEmbargo = fmap EmbargoId . newId . questionIdPool

-- | Return an embargo id. to the available pool.
freeEmbargo :: Conn' -> EmbargoId -> STM ()
freeEmbargo conn = freeId (exportIdPool conn) . embargoWord

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
    = withSupervisor $ \sup ->
        bracket
            (newConn sup)
            stopConn
            runConn
  where
    newConn sup = do
        stableName <- makeStableName =<< newEmptyMVar
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
            pendingCallbacks <- newTQueue

            let conn' = Conn'
                    { supervisor = sup
                    , questionIdPool
                    , exportIdPool
                    , recvQ
                    , sendQ
                    , questions
                    , answers
                    , exports
                    , imports
                    , embargos
                    , pendingCallbacks
                    , bootstrap
                    }
            liveState <- newTVar (Live conn')
            let conn = Conn
                    { stableName
                    , debugMode
                    , liveState
                    }
            pure (conn, conn')
    runConn (conn, conn') = do
        result <- try $
            ( coordinator conn
                `concurrently_` sendLoop transport conn'
                `concurrently_` recvLoop transport conn'
                `concurrently_` callbacksLoop conn'
            ) `race_`
                useBootstrap conn conn'
        case result of
            Left (SentAbort e) -> do
                -- We need to actually send it:
                rawMsg <- createPure maxBound $ valueToMsg $ R.Message'abort e
                void $ timeout 1000000 $ sendMsg transport rawMsg
                throwIO $ SentAbort e
            Left e ->
                throwIO e
            Right _ ->
                pure ()
    stopConn
            ( conn@Conn{liveState}
            , conn'@Conn'{questions, exports, embargos}
            ) = do
        atomically $ do
            let walk table = flip ListT.traverse_ (M.listT table)
            -- drop the bootstrap interface:
            case bootstrap conn' of
                Just (Client (Just client')) -> dropConnExport conn client'
                _                            -> pure ()
            -- Remove everything from the exports table:
            walk exports $ \(_, EntryE{client}) ->
                dropConnExport conn client
            -- Outstanding questions should all throw disconnected:
            walk questions $ \(QAId qid, entry) ->
                let raiseDisconnected onReturn =
                        mapQueueSTM conn' onReturn $ R.Return
                            { answerId = qid
                            , releaseParamCaps = False
                            , union' = R.Return'exception eDisconnected
                            }
                in case entry of
                    NewQA{onReturn}      -> raiseDisconnected onReturn
                    HaveFinish{onReturn} -> raiseDisconnected onReturn
                    _                    -> pure ()
            -- same thing with embargos:
            walk embargos $ \(_, fulfiller) ->
                breakPromiseSTM fulfiller eDisconnected
            -- mark the connection as dead, making the live state inaccessible:
            writeTVar liveState Dead
        -- Make sure any pending callbacks get run. This is important, since
        -- some of these do things like raise disconnected exceptions.
        --
        -- FIXME: there's a race condition that we're not dealing with:
        -- if the callbacks loop is killed between dequeuing an action and
        -- performing it that action will be lost.
        flushCallbacks conn'
    useBootstrap conn conn' = case withBootstrap of
        Nothing ->
            forever $ threadDelay maxBound
        Just f  ->
            atomically (requestBootstrap conn) >>= f (supervisor conn')


-- | A pool of ids; used when choosing identifiers for questions and exports.
newtype IdPool = IdPool (TVar [Word32])

-- | @'newIdPool' size@ creates a new pool of ids, with @size@ available ids.
newIdPool :: Word32 -> STM IdPool
newIdPool size = IdPool <$> newTVar [0..size-1]

-- | Get a new id from the pool. Retries if the pool is empty.
newId :: IdPool -> STM Word32
newId (IdPool pool) = readTVar pool >>= \case
    [] -> retry
    (id:ids) -> do
        writeTVar pool $! ids
        pure id

-- | Return an id to the pool.
freeId :: IdPool -> Word32 -> STM ()
freeId (IdPool pool) id = modifyTVar' pool (id:)

-- | An entry in our questions or answers table.
data EntryQA
    -- | An entry for which we have neither sent/received a finish, nor
    -- a return. Contains two sets of callbacks, to invoke on each type
    -- of message.
    = NewQA
        { onFinish :: SnocList (R.Finish -> STM ())
        , onReturn :: SnocList (R.Return -> STM ())
        }
    -- | An entry for which we've sent/received a return, but not a finish.
    -- Contains the return message, and a set of callbacks to invoke on the
    -- finish.
    | HaveReturn
        { returnMsg :: R.Return
        , onFinish  :: SnocList (R.Finish -> STM ())
        }
    -- | An entry for which we've sent/received a finish, but not a return.
    -- Contains the finish message, and a set of callbacks to invoke on the
    -- return.
    | HaveFinish
        { finishMsg :: R.Finish
        , onReturn  :: SnocList (R.Return -> STM ())
        }


-- | An entry in our imports table.
data EntryI = EntryI
    { localRc      :: Rc ()
    -- ^ A refcount cell with a finalizer attached to it; when the finalizer
    -- runs it will remove this entry from the table and send a release
    -- message to the remote vat.
    , remoteRc     :: !Word32
    -- ^ The reference count for this object as understood by the remote
    -- vat. This tells us what to send in the release message's count field.
    , proxies      :: ExportMap
    -- ^ See Note [proxies]
    --
    , promiseState :: Maybe
        ( TVar PromiseState
        , TmpDest -- origTarget field. TODO(cleanup): clean this up a bit.
        )
    -- ^ If this entry is a promise, this will contain the state of that
    -- promise, so that it may be used to create PromiseClients and
    -- update the promise when it resolves.
    }

-- | An entry in our exports table.
data EntryE = EntryE
    { client   :: Client'
    -- ^ The client. We cache it in the table so there's only one object
    -- floating around, which lets us attach a finalizer without worrying
    -- about it being run more than once.
    , refCount :: !Word32
    -- ^ The refcount for this entry. This lets us know when we can drop
    -- the entry from the table.
    }

-- | Types which may be converted to and from 'Client's. Typically these
-- will be simple type wrappers for capabilities.
class IsClient a where
    -- | Convert a value to a client.
    toClient :: a -> Client
    -- | Convert a client to a value.
    fromClient :: Client -> a

instance IsClient Client where
    toClient = id
    fromClient = id

instance Show Client where
    show (Client Nothing) = "nullClient"
    show _                = "({- capability; not statically representable -})"

-- | A reference to a capability, which may be live either in the current vat
-- or elsewhere. Holding a client affords making method calls on a capability
-- or modifying the local vat's reference count to it.
newtype Client =
    -- We wrap the real client in a Maybe, with Nothing representing a 'null'
    -- capability.
    Client (Maybe Client')
    deriving(Eq)

-- | A non-null client.
data Client'
    -- | A client pointing at a capability local to our own vat.
    = LocalClient
        { exportMap    :: ExportMap
        -- ^ Record of what export IDs this client has on different remote
        -- connections.
        , qCall        :: Rc (Server.CallInfo -> STM ())
        -- ^ Queue a call for the local capability to handle. This is wrapped
        -- in a reference counted cell, whose finalizer stops the server.
        , finalizerKey :: Fin.Cell ()
        -- ^ Finalizer key; when this is collected, qCall will be released.
        }
    -- | A client which will resolve to some other capability at
    -- some point.
    | PromiseClient
        { pState     :: TVar PromiseState
        -- ^ The current state of the promise; the indirection allows
        -- the promise to be updated.
        , exportMap  :: ExportMap

        , origTarget :: TmpDest
        -- ^ The original target of this promise, before it was resolved.
        -- (if it is still in the pending state, it will match the TmpDest
        -- stored there).
        --
        -- FIXME: if this is an ImportDest, by holding on to this we actually
        -- leak the cap.
        }
    -- | A client which points to a (resolved) capability in a remote vat.
    | ImportClient (Fin.Cell ImportRef)

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
    | Error R.Exception

-- | A temporary destination for calls on an unresolved promise.
data TmpDest
    -- | A destination that is local to this vat.
    = LocalDest LocalDest
    -- | A destination in another vat.
    | RemoteDest RemoteDest

newtype LocalDest
    -- | Queue the calls in a buffer.
    = LocalBuffer { callBuffer :: TQueue Server.CallInfo }

data RemoteDest
    -- | Send call messages to a remote vat, targeting the results
    -- of an outstanding question.
    = AnswerDest
        { conn   :: Conn
        -- ^ The connection to the remote vat.
        , answer :: PromisedAnswer
        -- ^ The answer to target.
        }
    -- | Send call messages to a remote vat, targeting an entry in our
    -- imports table.
    | ImportDest (Fin.Cell ImportRef)

-- | A reference to a capability in our import table/a remote vat's export
-- table.
data ImportRef = ImportRef
    { conn     :: Conn
    -- ^ The connection to the remote vat.
    , importId :: !IEId
    -- ^ The import id for this capability.
    , proxies  :: ExportMap
    -- ^ Export ids to use when this client is passed to a vat other than
    -- the one identified by 'conn'. See Note [proxies]
    }

-- Ideally we could just derive these, but stm-containers doesn't have Eq
-- instances, so neither does ExportMap. not all of the fields are actually
-- necessary to check equality though. See also
-- https://github.com/nikita-volkov/stm-hamt/pull/1
instance Eq ImportRef where
    ImportRef { conn=cx, importId=ix } == ImportRef { conn=cy, importId=iy } =
        cx == cy && ix == iy
instance Eq Client' where
    LocalClient { qCall = x } == LocalClient { qCall = y } =
        x == y
    PromiseClient { pState = x } == PromiseClient { pState = y } =
        x == y
    ImportClient x == ImportClient y =
        x == y
    _ == _ =
        False


-- | an 'ExportMap' tracks a mapping from connections to export IDs; it is
-- used to ensure that we re-use export IDs for capabilities when passing
-- them to remote vats. This used for locally hosted capabilities, but also
-- by proxied imports (see Note [proxies]).
newtype ExportMap = ExportMap (M.Map Conn IEId)

-- MsgTarget and PromisedAnswer correspond to the similarly named types in
-- rpc.capnp, except:
--
-- * They use our newtype wrappers for ids
-- * They don't have unknown variants
-- * PromisedAnswer's transform field is just a list of pointer offsets,
--   rather than a union with no other actually-useful variants.
-- * PromisedAnswer's transform field is a SnocList, efficient appending.
data MsgTarget
    = ImportTgt !IEId
    | AnswerTgt PromisedAnswer
data PromisedAnswer = PromisedAnswer
    { answerId  :: !QAId
    , transform :: SnocList Word16
    }

-- Note [proxies]
-- ==============
--
-- It is possible to have multiple connections open at once, and pass around
-- clients freely between them. Without level 3 support, this means that when
-- we pass a capability pointing into Vat A to another Vat B, we must proxy it.
--
-- To achieve this, capabilities pointing into a remote vat hold an 'ExportMap',
-- which tracks which export IDs we should be using to proxy the client on each
-- connection.

-- | Queue a call on a client.
call :: Server.CallInfo -> Client -> STM ()
call Server.CallInfo { response } (Client Nothing) =
    breakPromiseSTM response eMethodUnimplemented
call info@Server.CallInfo { response } (Client (Just client')) = case client' of
    LocalClient { qCall } -> Rc.get qCall >>= \case
        Just q ->
            q info
        Nothing ->
            breakPromiseSTM response eDisconnected

    PromiseClient { pState } -> readTVar pState >>= \case
        Ready { target }  ->
            call info target

        Embargo { callBuffer } ->
            writeTQueue callBuffer info

        Pending { tmpDest } -> case tmpDest of
            LocalDest LocalBuffer { callBuffer } ->
                writeTQueue callBuffer info

            RemoteDest AnswerDest { conn, answer } ->
                callRemote conn info $ AnswerTgt answer

            RemoteDest (ImportDest (Fin.get -> ImportRef { conn, importId })) ->
                callRemote conn info (ImportTgt importId)

        Error exn ->
            breakPromiseSTM response exn

    ImportClient (Fin.get -> ImportRef { conn, importId }) ->
        callRemote conn info (ImportTgt importId)

-- | Send a call to a remote capability.
callRemote :: Conn -> Server.CallInfo -> MsgTarget -> STM ()
callRemote
        conn
        Server.CallInfo{ interfaceId, methodId, arguments, response }
        target = do
    conn'@Conn'{questions} <- getLive conn
    qid <- newQuestion conn'
    payload@R.Payload{capTable} <- makeOutgoingPayload conn arguments
    sendPureMsg conn' $ R.Message'call def
        { R.questionId = qaWord qid
        , R.target = marshalMsgTarget target
        , R.params = payload
        , R.interfaceId = interfaceId
        , R.methodId = methodId
        }
    -- save these in case the callee sends back releaseParamCaps = True in the return
    -- message:
    let paramCaps = catMaybes $ flip map (V.toList capTable) $ \case
            R.CapDescriptor'senderHosted  eid -> Just (IEId eid)
            R.CapDescriptor'senderPromise eid -> Just (IEId eid)
            _ -> Nothing
    M.insert
        NewQA
            { onReturn = SnocList.singleton $ cbCallReturn paramCaps conn response
            , onFinish = SnocList.empty
            }
        qid
        questions

-- | Callback to run when a return comes in that corresponds to a call
-- we sent. Registered in callRemote. The first argument is a list of
-- export IDs to release if the return message has releaseParamCaps = true.
cbCallReturn :: [IEId] -> Conn -> Fulfiller RawMPtr -> R.Return -> STM ()
cbCallReturn
        paramCaps
        conn
        response
        R.Return{ answerId, union', releaseParamCaps } = do
    conn'@Conn'{answers} <- getLive conn
    when releaseParamCaps $
        traverse_ (releaseExport conn 1) paramCaps
    case union' of
        R.Return'exception exn ->
            breakPromiseSTM response exn
        R.Return'results R.Payload{ content } -> do
            rawPtr <- createPure defaultLimit $ do
                msg <- Message.newMessage Nothing
                cerialize msg content
            fulfillSTM response rawPtr
        R.Return'canceled ->
            breakPromiseSTM response $ eFailed "Canceled"

        R.Return'resultsSentElsewhere ->
            -- This should never happen, since we always set
            -- sendResultsTo = caller
            abortConn conn' $ eFailed $ mconcat
                [ "Received Return.resultsSentElswhere for a call "
                , "with sendResultsTo = caller."
                ]

        R.Return'takeFromOtherQuestion (QAId -> qid) ->
            -- TODO(cleanup): we should be a little stricter; the protocol
            -- requires that (1) each answer is only used this way once, and
            -- (2) The question was sent with sendResultsTo set to 'yourself',
            -- but we don't enforce either of these requirements.
            subscribeReturn "answer" conn' answers qid $
                cbCallReturn [] conn response

        R.Return'acceptFromThirdParty _ ->
            -- Note [Level 3]
            abortConn conn' $ eUnimplemented
                "This vat does not support level 3."
        R.Return'unknown' ordinal ->
            abortConn conn' $ eUnimplemented $
                "Unknown return variant #" <> fromString (show ordinal)
    finishQuestion conn' def
        { R.questionId = answerId
        , R.releaseResultCaps = False
        }


marshalMsgTarget :: MsgTarget -> R.MessageTarget
marshalMsgTarget = \case
    ImportTgt importId ->
        R.MessageTarget'importedCap (ieWord importId)
    AnswerTgt tgt ->
        R.MessageTarget'promisedAnswer $ marshalPromisedAnswer tgt

marshalPromisedAnswer :: PromisedAnswer -> R.PromisedAnswer
marshalPromisedAnswer PromisedAnswer{ answerId, transform } =
    R.PromisedAnswer
        { R.questionId = qaWord answerId
        , R.transform =
            V.fromList $
                map R.PromisedAnswer'Op'getPointerField $
                    toList transform
        }

unmarshalPromisedAnswer :: R.PromisedAnswer -> Either R.Exception PromisedAnswer
unmarshalPromisedAnswer R.PromisedAnswer { questionId, transform } = do
    idxes <- unmarshalOps (toList transform)
    pure PromisedAnswer
        { answerId = QAId questionId
        , transform = SnocList.fromList idxes
        }

unmarshalOps :: [R.PromisedAnswer'Op] -> Either R.Exception [Word16]
unmarshalOps [] = Right []
unmarshalOps (R.PromisedAnswer'Op'noop:ops) =
    unmarshalOps ops
unmarshalOps (R.PromisedAnswer'Op'getPointerField i:ops) =
    (i:) <$> unmarshalOps ops
unmarshalOps (R.PromisedAnswer'Op'unknown' tag:_) =
    Left $ eFailed $ "Unknown PromisedAnswer.Op: " <> fromString (show tag)


-- | A null client. This is the only client value that can be represented
-- statically. Throws exceptions in response to all method calls.
nullClient :: Client
nullClient = Client Nothing

-- | Create a new client based on a promise. The fulfiller can be used to
-- supply the final client.
newPromiseClient :: IsClient c => IO (c, Fulfiller c)
newPromiseClient = atomically newPromiseClientSTM

-- | Like 'newPromiseClient', but in 'STM'.
newPromiseClientSTM :: IsClient c => STM (c, Fulfiller c)
newPromiseClientSTM = do
    callBuffer <- newTQueue
    let tmpDest = LocalDest LocalBuffer { callBuffer }
    pState <- newTVar Pending { tmpDest }
    exportMap <- ExportMap <$> M.new
    f <- newCallbackSTM $ \case
        Left e -> writeTVar pState (Error e)
        Right v -> writeTVar pState Ready { target = toClient v }
    let p = Client $ Just $ PromiseClient
            { pState
            , exportMap
            , origTarget = tmpDest
            }
    pure (fromClient p, f)


-- | Spawn a local server with its lifetime bound to the supervisor,
-- and return a client for it. When the client is garbage collected,
-- the server will be stopped (if it is still running).
export :: Supervisor -> Server.ServerOps IO -> STM Client
export sup ops = do
    q <- TCloseQ.new
    qCall <- Rc.new (TCloseQ.write q) (TCloseQ.close q)
    exportMap <- ExportMap <$> M.new
    finalizerKey <- Fin.newCell ()
    let client' = LocalClient
            { qCall
            , exportMap
            , finalizerKey
            }
    superviseSTM sup $ do
        Fin.addFinalizer finalizerKey $ atomically $ Rc.release qCall
        Server.runServer q ops
    pure $ Client (Just client')

clientMethodHandler :: Word64 -> Word16 -> Client -> Server.MethodHandler IO p r
clientMethodHandler interfaceId methodId client =
    Server.fromUntypedHandler $ Server.untypedHandler $
        \arguments response -> atomically $ call Server.CallInfo{..} client

-- | See Note [callbacks]
callbacksLoop :: Conn' -> IO ()
callbacksLoop Conn'{pendingCallbacks} = forever $ do
    cbs <- atomically $ flushTQueue pendingCallbacks >>= \case
        -- We need to make sure to block if there weren't any jobs, since
        -- otherwise we'll busy loop, pegging the CPU.
        [] -> retry
        cbs -> pure cbs
    sequence_ cbs

-- Run the one iteration of the callbacks loop, without blocking.
flushCallbacks :: Conn' -> IO ()
flushCallbacks Conn'{pendingCallbacks} =
    atomically (flushTQueue pendingCallbacks) >>= sequence_

-- | 'sendLoop' shunts messages from the send queue into the transport.
sendLoop :: Transport -> Conn' -> IO ()
sendLoop transport Conn'{sendQ} =
    forever $ atomically (readTBQueue sendQ) >>= sendMsg transport

-- | 'recvLoop' shunts messages from the transport into the receive queue.
recvLoop :: Transport -> Conn' -> IO ()
recvLoop transport Conn'{recvQ} =
    forever $ recvMsg transport >>= atomically . writeTBQueue recvQ

-- | The coordinator processes incoming messages.
coordinator :: Conn -> IO ()
-- The logic here mostly routes messages to other parts of the code that know
-- more about the objects in question; See Note [Organization] for more info.
coordinator conn@Conn{debugMode} = forever $ atomically $ do
    conn'@Conn'{recvQ} <- getLive conn
    msg <- (readTBQueue recvQ >>= parseWithCaps conn)
        `catchSTM`
        (abortConn conn' . wrapException debugMode)
    case msg of
        R.Message'abort exn ->
            handleAbortMsg conn exn
        R.Message'unimplemented oldMsg ->
            handleUnimplementedMsg conn oldMsg
        R.Message'bootstrap bs ->
            handleBootstrapMsg conn bs
        R.Message'call call ->
            handleCallMsg conn call
        R.Message'return ret ->
            handleReturnMsg conn ret
        R.Message'finish finish ->
            handleFinishMsg conn finish
        R.Message'resolve res ->
            handleResolveMsg conn res
        R.Message'release release ->
            handleReleaseMsg conn release
        R.Message'disembargo disembargo ->
            handleDisembargoMsg conn disembargo
        _ ->
            sendPureMsg conn' $ R.Message'unimplemented msg

-- | 'parseWithCaps' parses a message, making sure to interpret its capability
-- table. The latter bit is the difference between this and just calling
-- 'msgToValue'; 'msgToValue' will leave all of the clients in the message
-- null.
parseWithCaps :: Conn -> ConstMsg -> STM R.Message
parseWithCaps conn msg = do
    pureMsg <- msgToValue msg
    case pureMsg of
        -- capabilities only appear in call and return messages, and in the
        -- latter only in the 'results' variant. In the other cases we can
        -- just leave the result alone.
        R.Message'call R.Call{params=R.Payload{capTable}} ->
            fixCapTable capTable conn msg >>= msgToValue
        R.Message'return R.Return{union'=R.Return'results R.Payload{capTable}} ->
            fixCapTable capTable conn msg >>= msgToValue
        _ ->
            pure pureMsg

-- Each function handle*Msg handles a message of a particular type;
-- 'coordinator' dispatches to these.

handleAbortMsg :: Conn -> R.Exception -> STM ()
handleAbortMsg _ exn =
    throwSTM (ReceivedAbort exn)

handleUnimplementedMsg :: Conn -> R.Message -> STM ()
handleUnimplementedMsg conn msg = getLive conn >>= \conn' -> case msg of
    R.Message'unimplemented _ ->
        -- If the client itself doesn't handle unimplemented messages, that's
        -- weird, but ultimately their problem.
        pure ()
    R.Message'abort _ ->
        abortConn conn' $ eFailed $
            "Your vat sent an 'unimplemented' message for an abort message " <>
            "that its remote peer never sent. This is likely a bug in your " <>
            "capnproto library."
    _ ->
        abortConn conn' $
            eFailed "Received unimplemented response for required message."

handleBootstrapMsg :: Conn -> R.Bootstrap -> STM ()
handleBootstrapMsg conn R.Bootstrap{ questionId } = getLive conn >>= \conn' -> do
    ret <- case bootstrap conn' of
        Nothing ->
            pure $ R.Return
                { R.answerId = questionId
                , R.releaseParamCaps = True -- Not really meaningful for bootstrap, but...
                , R.union' =
                    R.Return'exception $
                        eFailed "No bootstrap interface for this connection."
                }
        Just client -> do
            capDesc <- emitCap conn client
            pure $ R.Return
                { R.answerId = questionId
                , R.releaseParamCaps = True -- Not really meaningful for bootstrap, but...
                , R.union' =
                    R.Return'results R.Payload
                            -- XXX: this is a bit fragile; we're relying on
                            -- the encode step to pick the right index for
                            -- our capability.
                        { content = Just (Untyped.PtrCap client)
                        , capTable = V.singleton capDesc
                        }
                }
    M.focus
        (Focus.alterM $ insertBootstrap conn' ret)
        (QAId questionId)
        (answers conn')
    sendPureMsg conn' $ R.Message'return ret
  where
    insertBootstrap _ ret Nothing =
        pure $ Just HaveReturn
            { returnMsg = ret
            , onFinish = SnocList.fromList
                [ \R.Finish{releaseResultCaps} ->
                    case ret of
                        R.Return
                            { union' = R.Return'results R.Payload
                                { capTable = (V.toList -> [ R.CapDescriptor'receiverHosted (IEId -> eid)])
                                }
                            } ->
                                when releaseResultCaps $
                                    releaseExport conn 1 eid
                        _ ->
                            pure ()
                ]

            }
    insertBootstrap conn' _ (Just _) =
        abortConn conn' $ eFailed "Duplicate question ID"

handleCallMsg :: Conn -> R.Call -> STM ()
handleCallMsg
        conn
        R.Call
            { questionId
            , target
            , interfaceId
            , methodId
            , params=R.Payload{content, capTable}
            }
        = getLive conn >>= \conn'@Conn'{exports, answers} -> do
    -- First, add an entry in our answers table:
    insertNewAbort
        "answer"
        conn'
        (QAId questionId)
        NewQA
            { onReturn = SnocList.empty
            , onFinish = SnocList.fromList
                [ \R.Finish{releaseResultCaps} ->
                    when releaseResultCaps $
                        for_ capTable $ \case
                            R.CapDescriptor'receiverHosted (IEId -> importId) ->
                                releaseExport conn 1 importId
                            _ ->
                                pure ()
                ]
            }
        answers

    -- Marshal the parameters to the call back into the low-level form:
    callParams <- createPure defaultLimit $ do
        msg <- Message.newMessage Nothing
        cerialize msg content

    -- Set up a callback for when the call is finished, to
    -- send the return message:
    fulfiller <- newCallbackSTM $ \case
        Left e ->
            returnAnswer conn' def
                { R.answerId = questionId
                , R.releaseParamCaps = False
                , R.union' = R.Return'exception e
                }
        Right v -> do
            content <- evalLimitT defaultLimit (decerialize v)
            capTable <- genSendableCapTable conn content
            returnAnswer conn' def
                { R.answerId = questionId
                , R.releaseParamCaps = False
                , R.union'   = R.Return'results def
                    { R.content  = content
                    , R.capTable = capTable
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
        R.MessageTarget'importedCap exportId ->
            lookupAbort "export" conn' exports (IEId exportId) $
                \EntryE{client} -> call callInfo $ Client $ Just client
        R.MessageTarget'promisedAnswer R.PromisedAnswer { questionId = targetQid, transform } ->
            let onReturn ret@R.Return{union'} =
                    case union' of
                        R.Return'exception _ ->
                            returnAnswer conn' ret { R.answerId = questionId }
                        R.Return'canceled ->
                            returnAnswer conn' ret { R.answerId = questionId }
                        R.Return'results R.Payload{content} ->
                            transformClient transform content conn' >>= call callInfo
                        R.Return'resultsSentElsewhere ->
                            -- our implementation should never actually do this, but
                            -- this way we don't have to change this if/when we
                            -- support the feature:
                            abortConn conn' $ eFailed $
                                "Tried to call a method on a promised answer that " <>
                                "returned resultsSentElsewhere"
                        R.Return'takeFromOtherQuestion otherQid ->
                            subscribeReturn "answer" conn' answers (QAId otherQid) onReturn
                        R.Return'acceptFromThirdParty _ ->
                            -- Note [Level 3]
                            error "BUG: our implementation unexpectedly used a level 3 feature"
                        R.Return'unknown' tag ->
                            error $
                                "BUG: our implemented unexpectedly returned unknown " ++
                                "result variant #" ++ show tag
            in
            subscribeReturn "answer" conn' answers (QAId targetQid) onReturn
        R.MessageTarget'unknown' ordinal ->
            abortConn conn' $ eUnimplemented $
                "Unknown MessageTarget ordinal #" <> fromString (show ordinal)

transformClient :: V.Vector R.PromisedAnswer'Op -> MPtr -> Conn' -> STM Client
transformClient transform ptr conn =
    case unmarshalOps (V.toList transform) >>= flip followPtrs ptr of
        Left e ->
            abortConn conn e
        Right Nothing ->
            pure nullClient
        Right (Just (Untyped.PtrCap client)) ->
            pure client
        Right (Just _) ->
            abortConn conn $ eFailed "Tried to call method on non-capability."

-- | Follow a series of pointer indicies, returning the final value, or 'Left'
-- with an error if any of the pointers in the chain (except the last one) is
-- a non-null non struct.
followPtrs :: [Word16] -> MPtr -> Either R.Exception MPtr
followPtrs [] ptr =
    Right ptr
followPtrs (_:_) Nothing =
    Right Nothing
followPtrs (i:is) (Just (Untyped.PtrStruct (Untyped.Struct _ ptrs))) =
    followPtrs is (Untyped.sliceIndex (fromIntegral i) ptrs)
followPtrs (_:_) (Just _) =
    Left (eFailed "Tried to access pointer field of non-struct.")

handleReturnMsg :: Conn -> R.Return -> STM ()
handleReturnMsg conn ret = getLive conn >>= \conn'@Conn'{questions} ->
    updateQAReturn conn' questions "question" ret

handleFinishMsg :: Conn -> R.Finish -> STM ()
handleFinishMsg conn finish = getLive conn >>= \conn'@Conn'{answers} ->
    updateQAFinish conn' answers "answer" finish

handleResolveMsg :: Conn -> R.Resolve -> STM ()
handleResolveMsg conn R.Resolve{promiseId, union'} =
    getLive conn >>= \conn'@Conn'{imports} -> do
        entry <- M.lookup (IEId promiseId) imports
        case entry of
            Nothing ->
                -- This can happen if we dropped the promise, but the release
                -- message is still in flight when the resolve message is sent.
                case union' of
                    R.Resolve'cap (R.CapDescriptor'receiverHosted importId) ->
                        -- Send a release message for the resolved cap, since
                        -- we're not going to use it:
                        sendPureMsg conn' $ R.Message'release def
                            { R.id = importId
                            , R.referenceCount = 1
                            }
                    -- Note [Level 3]: do we need to do something with
                    -- thirdPartyHosted here?
                    _ -> pure ()
            Just EntryI{ promiseState = Nothing } ->
                -- This wasn't a promise! The remote vat has done something wrong;
                -- abort the connection.
                abortConn conn' $ eFailed $ mconcat
                    [ "Received a resolve message for export id #", fromString (show promiseId)
                    , ", but that capability is not a promise!"
                    ]
            Just EntryI { promiseState = Just (tvar, tmpDest) } ->
                case union' of
                    R.Resolve'cap cap -> do
                        client <- acceptCap conn cap
                        resolveClientClient tmpDest (writeTVar tvar) client
                    R.Resolve'exception exn ->
                        resolveClientExn tmpDest (writeTVar tvar) exn
                    R.Resolve'unknown' tag ->
                        abortConn conn' $ eUnimplemented $ mconcat
                            [ "Resolve variant #"
                            , fromString (show tag)
                            , " not understood"
                            ]

handleReleaseMsg :: Conn -> R.Release -> STM ()
handleReleaseMsg
        conn
        R.Release
            { id=(IEId -> eid)
            , referenceCount=refCountDiff
            } =
    releaseExport conn refCountDiff eid

releaseExport :: Conn -> Word32 -> IEId -> STM ()
releaseExport conn refCountDiff eid =
    getLive conn >>= \conn'@Conn'{exports} ->
        lookupAbort "export" conn' exports eid $
            \EntryE{client, refCount=oldRefCount} ->
                case compare oldRefCount refCountDiff of
                    LT ->
                        abortConn conn' $ eFailed $
                            "Received release for export with referenceCount " <>
                            "greater than our recorded total ref count."
                    EQ ->
                        dropConnExport conn client
                    GT ->
                        M.insert
                            EntryE
                                { client
                                , refCount = oldRefCount - refCountDiff
                                }
                            eid
                            exports

handleDisembargoMsg :: Conn -> R.Disembargo -> STM ()
handleDisembargoMsg conn d = getLive conn >>= go d
  where
    go
        R.Disembargo { context=R.Disembargo'context'receiverLoopback (EmbargoId -> eid) }
        conn'@Conn'{embargos}
        = do
            result <- M.lookup eid embargos
            case result of
                Nothing ->
                    abortConn conn' $ eFailed $
                        "No such embargo: " <> fromString (show $ embargoWord eid)
                Just fulfiller -> do
                    queueSTM conn' (fulfillSTM fulfiller ())
                    M.delete eid embargos
                    freeEmbargo conn' eid
    go
        R.Disembargo{ target, context=R.Disembargo'context'senderLoopback embargoId }
        conn'@Conn'{exports, answers}
        = case target of
            R.MessageTarget'importedCap exportId ->
                lookupAbort "export" conn' exports (IEId exportId) $ \EntryE{ client } ->
                    disembargoPromise client
            R.MessageTarget'promisedAnswer R.PromisedAnswer{ questionId, transform } ->
                lookupAbort "answer" conn' answers (QAId questionId) $ \case
                    HaveReturn { returnMsg=R.Return{union'=R.Return'results R.Payload{content} } } ->
                        transformClient transform content conn' >>= \case
                            Client (Just client') -> disembargoClient client'
                            Client Nothing -> abortDisembargo "targets a null capability"
                    _ ->
                        abortDisembargo $
                            "does not target an answer which has resolved to a value hosted by"
                            <> " the sender."
            R.MessageTarget'unknown' ordinal ->
                abortConn conn' $ eUnimplemented $
                    "Unknown MessageTarget ordinal #" <> fromString (show ordinal)
      where
        disembargoPromise PromiseClient{ pState } = readTVar pState >>= \case
            Ready (Client (Just client)) ->
                disembargoClient client
            Ready (Client Nothing) ->
                abortDisembargo "targets a promise which resolved to null."
            _ ->
                abortDisembargo "targets a promise which has not resolved."
        disembargoPromise _ =
            abortDisembargo "targets something that is not a promise."

        disembargoClient (ImportClient (Fin.get -> ImportRef {conn=targetConn, importId}))
            | conn == targetConn =
                sendPureMsg conn' $ R.Message'disembargo R.Disembargo
                    { context = R.Disembargo'context'receiverLoopback embargoId
                    , target = R.MessageTarget'importedCap (ieWord importId)
                    }
        disembargoClient _ =
                abortDisembargo $
                    "targets a promise which has not resolved to a capability"
                    <> " hosted by the sender."

        abortDisembargo info =
            abortConn conn' $ eFailed $ mconcat
                [ "Disembargo #"
                , fromString (show embargoId)
                , " with context = senderLoopback "
                , info
                ]
-- Note [Level 3]
    go d conn' =
        sendPureMsg conn' $ R.Message'unimplemented $ R.Message'disembargo d



-- | Interpret the list of cap descriptors, and replace the message's capability
-- table with the result.
fixCapTable :: V.Vector R.CapDescriptor -> Conn -> ConstMsg -> STM ConstMsg
fixCapTable capDescs conn msg = do
    clients <- traverse (acceptCap conn) capDescs
    pure $ Message.withCapTable clients msg

lookupAbort
    :: (Eq k, Hashable k, Show k)
    => Text -> Conn' -> M.Map k v -> k -> (v -> STM a) -> STM a
lookupAbort keyTypeName conn m key f = do
    result <- M.lookup key m
    case result of
        Just val ->
            f val
        Nothing ->
            abortConn conn $ eFailed $ mconcat
                [ "No such "
                , keyTypeName
                ,  ": "
                , fromString (show key)
                ]

-- | @'insertNewAbort' keyTypeName conn key value stmMap@ inserts a key into a
-- map, aborting the connection if it is already present. @keyTypeName@ will be
-- used in the error message sent to the remote vat.
insertNewAbort :: (Eq k, Hashable k) => Text -> Conn' -> k -> v -> M.Map k v -> STM ()
insertNewAbort keyTypeName conn key value =
    M.focus
        (Focus.alterM $ \case
            Just _ ->
                abortConn conn $ eFailed $
                    "duplicate entry in " <> keyTypeName <> " table."
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
genSendableCapTable :: Conn -> MPtr -> STM (V.Vector R.CapDescriptor)
genSendableCapTable conn ptr = do
    rawPtr <- createPure defaultLimit $ do
        msg <- Message.newMessage Nothing
        cerialize msg ptr
    genSendableCapTableRaw conn rawPtr

genSendableCapTableRaw
    :: Conn
    -> Maybe (UntypedRaw.Ptr ConstMsg)
    -> STM (V.Vector R.CapDescriptor)
genSendableCapTableRaw _ Nothing = pure V.empty
genSendableCapTableRaw conn (Just ptr) =
    traverse
        (emitCap conn)
        (Message.getCapTable (UntypedRaw.message ptr))

-- | Convert the pointer into a Payload, including a capability table for
-- the clients in the pointer's cap table.
makeOutgoingPayload :: Conn -> RawMPtr -> STM R.Payload
makeOutgoingPayload conn rawContent = do
    capTable <- genSendableCapTableRaw conn rawContent
    content <- evalLimitT defaultLimit (decerialize rawContent)
    pure R.Payload { content, capTable }

sendPureMsg :: Conn' -> R.Message -> STM ()
sendPureMsg Conn'{sendQ} msg =
    createPure maxBound (valueToMsg msg) >>= writeTBQueue sendQ

-- | Send a finish message, updating connection state and triggering
-- callbacks as necessary.
finishQuestion :: Conn' -> R.Finish -> STM ()
finishQuestion conn@Conn'{questions} finish@R.Finish{questionId} = do
    -- arrange for the question ID to be returned to the pool once
    -- the return has also been received:
    subscribeReturn "question" conn questions (QAId questionId) $ \_ ->
        freeQuestion conn (QAId questionId)
    sendPureMsg conn $ R.Message'finish finish
    updateQAFinish conn questions "question" finish

-- | Send a return message, update the corresponding entry in our
-- answers table, and queue any registered callbacks. Calls 'error'
-- if the answerId is not in the table, or if we've already sent a
-- return for this answer.
returnAnswer :: Conn' -> R.Return -> STM ()
returnAnswer conn@Conn'{answers} ret = do
    sendPureMsg conn $ R.Message'return ret
    updateQAReturn conn answers "answer" ret

-- TODO(cleanup): updateQAReturn/Finish have a lot in common; can we refactor?

updateQAReturn :: Conn' -> M.Map QAId EntryQA -> Text -> R.Return -> STM ()
updateQAReturn conn table tableName ret@R.Return{answerId} =
    lookupAbort tableName conn table (QAId answerId) $ \case
        NewQA{onFinish, onReturn} -> do
            mapQueueSTM conn onReturn ret
            M.insert
                HaveReturn
                    { returnMsg = ret
                    , onFinish
                    }
                (QAId answerId)
                table
        HaveFinish{onReturn} -> do
            mapQueueSTM conn onReturn ret
            M.delete (QAId answerId) table
        HaveReturn{} ->
            abortConn conn $ eFailed $
                "Duplicate return message for " <> tableName <> " #"
                <> fromString (show answerId)

updateQAFinish :: Conn' -> M.Map QAId EntryQA -> Text -> R.Finish -> STM ()
updateQAFinish conn table tableName finish@R.Finish{questionId} =
    lookupAbort tableName conn table (QAId questionId) $ \case
        NewQA{onFinish, onReturn} -> do
            mapQueueSTM conn onFinish finish
            M.insert
                HaveFinish
                    { finishMsg = finish
                    , onReturn
                    }
                (QAId questionId)
                table
        HaveReturn{onFinish} -> do
            mapQueueSTM conn onFinish finish
            M.delete (QAId questionId) table
        HaveFinish{} ->
            abortConn conn $ eFailed $
                "Duplicate finish message for " <> tableName <> " #"
                <> fromString (show questionId)

-- | Update an entry in the questions or answers table to queue the given
-- callback when the return message for that answer comes in. If the return
-- has already arrived, the callback is queued immediately.
--
-- If the entry already has other callbacks registered, this callback is
-- run *after* the others (see Note [callbacks]). Note that this is an
-- important property, as it is necessary to preserve E-order if the
-- callbacks are successive method calls on the returned object.
subscribeReturn :: Text -> Conn' -> M.Map QAId EntryQA -> QAId -> (R.Return -> STM ()) -> STM ()
subscribeReturn tableName conn table qaId onRet =
    lookupAbort tableName conn table qaId $ \qa -> do
        new <- go qa
        M.insert new qaId table
  where
    go = \case
        NewQA{onFinish, onReturn} ->
            pure NewQA
                { onFinish
                , onReturn = SnocList.snoc onReturn onRet
                }

        HaveFinish{finishMsg, onReturn} ->
            pure HaveFinish
                { finishMsg
                , onReturn = SnocList.snoc onReturn onRet
                }

        val@HaveReturn{returnMsg} -> do
            queueSTM conn (onRet returnMsg)
            pure val

-- | Abort the connection, sending an abort message. This is only safe to call
-- from within either the thread running the coordinator or the callback loop.
abortConn :: Conn' -> R.Exception -> STM a
abortConn _ e = throwSTM (SentAbort e)

-- | Gets the live connection state, or throws disconnected if it is not live.
getLive :: Conn -> STM Conn'
getLive Conn{liveState} = readTVar liveState >>= \case
    Live conn' -> pure conn'
    Dead -> throwSTM eDisconnected

-- | Performs an action with the live connection state. Does nothing if the
-- connection is dead.
whenLive :: Conn -> (Conn' -> STM ()) -> STM ()
whenLive Conn{liveState} f = readTVar liveState >>= \case
    Live conn' -> f conn'
    Dead -> pure ()

-- | Request the remote vat's bootstrap interface.
requestBootstrap :: Conn -> STM Client
requestBootstrap conn@Conn{liveState} = readTVar liveState >>= \case
    Dead ->
        pure nullClient
    Live conn'@Conn'{questions} -> do
        qid <- newQuestion conn'
        let tmpDest = RemoteDest AnswerDest
                { conn
                , answer = PromisedAnswer
                    { answerId = qid
                    , transform = SnocList.empty
                    }
                }
        pState <- newTVar Pending { tmpDest }
        sendPureMsg conn' $
            R.Message'bootstrap def { R.questionId = qaWord qid }
        M.insert
            NewQA
                { onReturn = SnocList.singleton $
                    resolveClientReturn tmpDest (writeTVar pState) conn' []
                , onFinish = SnocList.empty
                }
            qid
            questions
        exportMap <- ExportMap <$> M.new
        pure $ Client $ Just PromiseClient
            { pState
            , exportMap
            , origTarget = tmpDest
            }

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
resolveClientExn :: TmpDest -> (PromiseState -> STM ()) -> R.Exception -> STM ()
resolveClientExn tmpDest resolve exn = do
    case tmpDest of
        LocalDest LocalBuffer { callBuffer } -> do
            calls <- flushTQueue callBuffer
            traverse_
                (\Server.CallInfo{response} ->
                    breakPromiseSTM response exn)
                calls
        RemoteDest AnswerDest {} ->
            pure ()
        RemoteDest (ImportDest _) ->
            pure ()
    resolve $ Error exn

-- Resolve a promised client to a pointer. If it is a non-null non-capability
-- pointer, it resolves to an exception. See Note [resolveClient]
resolveClientPtr :: TmpDest -> (PromiseState -> STM ()) -> MPtr -> STM ()
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
resolveClientClient tmpDest resolve (Client client) =
    case (client, tmpDest) of
        -- Remote resolved to local; we need to embargo:
        ( Just LocalClient{}, RemoteDest dest ) ->
            disembargoAndResolve dest
        ( Just PromiseClient { origTarget=LocalDest _ }, RemoteDest dest) ->
            disembargoAndResolve dest
        ( Nothing, RemoteDest dest ) ->
            -- It's not clear to me what we should actually do if the promise
            -- resolves to nullClient, but this can be encoded at the protocol
            -- level, so we have to deal with it. Possible options:
            --
            -- 1. Perhaps this is simply illegal, and we should send an abort?
            -- 2. Treat it as resolving to a local promise, in which case we
            --    need to send a disembargo as above.
            -- 3. Treat is as resolving to a remote promise, in which case we
            --    can't send an embargo.
            --
            -- (3) doesn't seem possible to implement quite correctly, since
            -- if we just resolve to nullClient right away, further calls will
            -- start returning exceptions before outstanding calls return -- we
            -- really do want to send a disembargo, but we can't because the
            -- protocol insists that we don't if the promise resolves to a
            -- remote cap.
            --
            -- What we currently do is (2); I(zenhack) intend to ask for
            -- clarification on the mailing list.
            disembargoAndResolve dest

        -- These cases are slightly subtle; despite resolving to a
        -- client that points at a "remote" target, if it points into a
        -- _different_ connection, we must be proxying it, so we treat
        -- it as local and do a disembargo like above. We may need to
        -- change this when we implement level 3, since third-party
        -- handoff is a possibility; see Note [Level 3].
        --
        -- If it's pointing into the same connection, we don't need to
        -- do a disembargo.
        ( Just PromiseClient { origTarget=RemoteDest newDest }, RemoteDest oldDest )
            | destConn newDest /= destConn oldDest ->
                disembargoAndResolve oldDest
            | otherwise ->
                releaseAndResolve
        ( Just (ImportClient (Fin.get -> ImportRef { conn=newConn })), RemoteDest oldDest )
            | newConn /= destConn oldDest ->
                disembargoAndResolve oldDest
            | otherwise ->
                releaseAndResolve

        -- Local promises never need embargos; we can just forward:
        ( _, LocalDest LocalBuffer { callBuffer } ) ->
            flushAndResolve callBuffer
  where
    destConn AnswerDest { conn }                          = conn
    destConn (ImportDest (Fin.get -> ImportRef { conn })) = conn
    destTarget AnswerDest { answer } = AnswerTgt answer
    destTarget (ImportDest (Fin.get -> ImportRef { importId })) = ImportTgt importId

    releaseAndResolve = do
        releaseTmpDest tmpDest
        resolve $ Ready (Client client)

    -- Flush the call buffer into the client's queue, and then pass the client
    -- to resolve.
    flushAndResolve callBuffer = do
        flushTQueue callBuffer >>= traverse_ (`call` Client client)
        resolve $ Ready (Client client)
    flushAndRaise callBuffer e =
        flushTQueue callBuffer >>=
            traverse_ (\Server.CallInfo{response} -> breakPromiseSTM response e)
    disembargoAndResolve dest@(destConn -> Conn{liveState}) =
        readTVar liveState >>= \case
            Live conn' -> do
                callBuffer <- newTQueue
                disembargo conn' (destTarget dest) $ \case
                    Right () ->
                        flushAndResolve callBuffer
                    Left e ->
                        flushAndRaise callBuffer e
                resolve $ Embargo { callBuffer }
            Dead ->
                resolveClientExn tmpDest resolve eDisconnected

-- | Send a (senderLoopback) disembargo to the given message target, and
-- register the transaction to run when the corresponding receiverLoopback
-- message is received.
--
-- The callback may be handed a 'Left' with a disconnected exception if
-- the connection is dropped before the disembargo is echoed.
disembargo :: Conn' -> MsgTarget -> (Either R.Exception () -> STM ()) -> STM ()
disembargo conn@Conn'{embargos} tgt onEcho = do
    callback <- newCallbackSTM onEcho
    eid <- newEmbargo conn
    M.insert callback eid embargos
    sendPureMsg conn $ R.Message'disembargo R.Disembargo
        { target = marshalMsgTarget tgt
        , context = R.Disembargo'context'senderLoopback (embargoWord eid)
        }

-- Do any cleanup of a TmpDest; this should be called after resolving a
-- pending promise.
releaseTmpDest :: TmpDest -> STM ()
releaseTmpDest (LocalDest LocalBuffer{}) = pure ()
releaseTmpDest (RemoteDest AnswerDest { conn, answer=PromisedAnswer{ answerId } }) =
    whenLive conn $ \conn' ->
        finishQuestion conn' def
            { R.questionId = qaWord answerId
            , R.releaseResultCaps = False
            }
releaseTmpDest (RemoteDest (ImportDest _)) = pure ()

-- | Resolve a promised client to the result of a return. See Note [resolveClient]
--
-- The [Word16] is a list of pointer indexes to follow from the result.
resolveClientReturn :: TmpDest -> (PromiseState -> STM ()) -> Conn' -> [Word16] -> R.Return -> STM ()
resolveClientReturn tmpDest resolve conn@Conn'{answers} transform R.Return { union' } = case union' of
    -- TODO(cleanup) there is a lot of redundency betwen this and cbCallReturn; can
    -- we refactor?
    R.Return'exception exn ->
        resolveClientExn tmpDest resolve exn
    R.Return'results R.Payload{ content } ->
        case followPtrs transform content of
            Right v ->
                resolveClientPtr tmpDest resolve v
            Left e ->
                resolveClientExn tmpDest resolve e

    R.Return'canceled ->
        resolveClientExn tmpDest resolve $ eFailed "Canceled"

    R.Return'resultsSentElsewhere ->
        -- Should never happen; we don't set sendResultsTo to anything other than
        -- caller.
        abortConn conn $ eFailed $ mconcat
            [ "Received Return.resultsSentElsewhere for a call "
            , "with sendResultsTo = caller."
            ]

    R.Return'takeFromOtherQuestion (QAId -> qid) ->
        subscribeReturn "answer" conn answers qid $
            resolveClientReturn tmpDest resolve conn transform

    R.Return'acceptFromThirdParty _ ->
        -- Note [Level 3]
        abortConn conn $ eUnimplemented
            "This vat does not support level 3."

    R.Return'unknown' ordinal ->
        abortConn conn $ eUnimplemented $
            "Unknown return variant #" <> fromString (show ordinal)

-- | Get the client's export ID for this connection, or allocate a new one if needed.
-- If this is the first time this client has been exported on this connection,
-- bump the refcount.
getConnExport :: Conn -> Client' -> STM IEId
getConnExport conn client = getLive conn >>= \conn'@Conn'{exports} -> do
    let ExportMap m = clientExportMap client
    val <- M.lookup conn m
    case val of
        Just eid -> do
            addBumpExport eid client exports
            pure eid

        Nothing -> do
            eid <- newExport conn'
            addBumpExport eid client exports
            M.insert eid conn m
            pure eid

-- | Remove export of the client on the connection. This entails removing it
-- from the export id, removing the connection from the client's ExportMap,
-- freeing the export id, and dropping the client's refcount.
dropConnExport :: Conn -> Client' -> STM ()
dropConnExport conn client' = do
    let ExportMap eMap = clientExportMap client'
    val <- M.lookup conn eMap
    case val of
        Just eid -> do
            M.delete conn eMap
            whenLive conn $ \conn'@Conn'{exports} -> do
                M.delete eid exports
                freeExport conn' eid
        Nothing ->
            error "BUG: tried to drop an export that doesn't exist."

clientExportMap :: Client' -> ExportMap
clientExportMap LocalClient{exportMap}                         = exportMap
clientExportMap PromiseClient{exportMap}                       = exportMap
clientExportMap (ImportClient (Fin.get -> ImportRef{proxies})) = proxies

-- | insert the client into the exports table, bumping the refcount if it is
-- already there. If a different client is already in the table at the same
-- id, call 'error'.
addBumpExport :: IEId -> Client' -> M.Map IEId EntryE -> STM ()
addBumpExport exportId client =
    M.focus (Focus.alter go) exportId
  where
    go Nothing = Just EntryE { client, refCount = 1 }
    go (Just EntryE{ client = oldClient, refCount } )
        | client /= oldClient =
            error $
                "BUG: addExportRef called with a client that is different " ++
                "from what is already in our exports table."
        | otherwise =
            Just EntryE { client, refCount = refCount + 1 }

-- | Generate a CapDescriptor, which we can sent to the connection's remote
-- vat to identify client. In the process, this may allocate export ids, update
-- reference counts, and so forth.
emitCap :: Conn -> Client -> STM R.CapDescriptor
emitCap _targetConn (Client Nothing) =
    pure R.CapDescriptor'none
emitCap targetConn (Client (Just client')) = case client' of
    LocalClient{} ->
        R.CapDescriptor'senderHosted . ieWord <$> getConnExport targetConn client'
    PromiseClient{ pState } -> readTVar pState >>= \case
        Pending { tmpDest = RemoteDest AnswerDest { conn, answer } }
            | conn == targetConn ->
                pure $ R.CapDescriptor'receiverAnswer (marshalPromisedAnswer answer)
        Pending { tmpDest = RemoteDest (ImportDest (Fin.get -> ImportRef { conn, importId = IEId iid })) }
            | conn == targetConn ->
                pure $ R.CapDescriptor'receiverHosted iid
        _ ->
            R.CapDescriptor'senderPromise . ieWord <$> getConnExport targetConn client'
    ImportClient (Fin.get -> ImportRef { conn=hostConn, importId })
        | hostConn == targetConn ->
            pure (R.CapDescriptor'receiverHosted (ieWord importId))
        | otherwise ->
            R.CapDescriptor'senderHosted . ieWord <$> getConnExport targetConn client'

-- | 'acceptCap' is a dual of 'emitCap'; it derives a Client from a CapDescriptor
-- received via the connection. May update connection state as necessary.
acceptCap :: Conn -> R.CapDescriptor -> STM Client
acceptCap conn cap = getLive conn >>= \conn' -> go conn' cap
  where
    go _ R.CapDescriptor'none = pure (Client Nothing)
    go conn'@Conn'{imports} (R.CapDescriptor'senderHosted (IEId -> importId)) = do
        entry <- M.lookup importId imports
        case entry of
            Just EntryI{ promiseState=Just _ } ->
                let imp = fromString (show importId)
                in abortConn conn' $ eFailed $
                    "received senderHosted capability #" <> imp <>
                    ", but the imports table says #" <> imp <> " is senderPromise."
            Just ent@EntryI{ localRc, remoteRc, proxies } -> do
                Rc.incr localRc
                M.insert ent { localRc, remoteRc = remoteRc + 1 } importId imports
                cell <- Fin.newCell ImportRef
                    { conn
                    , importId
                    , proxies
                    }
                queueIO conn' $ Fin.addFinalizer cell $ atomically (Rc.decr localRc)
                pure $ Client $ Just $ ImportClient cell

            Nothing ->
                Client . Just . ImportClient <$> newImport importId conn Nothing
    go conn'@Conn'{imports} (R.CapDescriptor'senderPromise (IEId -> importId)) = do
        entry <- M.lookup importId imports
        case entry of
            Just EntryI { promiseState=Nothing } ->
                let imp = fromString (show importId)
                in abortConn conn' $ eFailed $
                    "received senderPromise capability #" <> imp <>
                    ", but the imports table says #" <> imp <> " is senderHosted."
            Just ent@EntryI { remoteRc, proxies, promiseState=Just (pState, origTarget) } -> do
                M.insert ent { remoteRc = remoteRc + 1 } importId imports
                pure $ Client $ Just PromiseClient
                    { pState
                    , exportMap = proxies
                    , origTarget
                    }
            Nothing -> do
                rec imp@(Fin.get -> ImportRef{proxies}) <- newImport importId conn (Just (pState, tmpDest))
                    let tmpDest = RemoteDest (ImportDest imp)
                    pState <- newTVar Pending { tmpDest }
                pure $ Client $ Just PromiseClient
                    { pState
                    , exportMap = proxies
                    , origTarget = tmpDest
                    }
    go conn'@Conn'{exports} (R.CapDescriptor'receiverHosted exportId) =
        lookupAbort "export" conn' exports (IEId exportId) $
            \EntryE{client} ->
                pure $ Client $ Just client
    go conn' (R.CapDescriptor'receiverAnswer pa) =
        case unmarshalPromisedAnswer pa of
            Left e ->
                abortConn conn' e
            Right pa ->
                newLocalAnswerClient conn' pa
    go conn' (R.CapDescriptor'thirdPartyHosted _) =
        -- Note [Level 3]
        abortConn conn' $ eUnimplemented
            "thirdPartyHosted unimplemented; level 3 is not supported."
    go conn' (R.CapDescriptor'unknown' tag) =
        abortConn conn' $ eUnimplemented $
            "Unimplemented CapDescriptor variant #" <> fromString (show tag)

-- | Create a new entry in the imports table, with the given import id and
-- 'promiseState', and return a corresponding ImportRef. When the ImportRef is
-- garbage collected, the refcount in the table will be decremented.
newImport :: IEId -> Conn -> Maybe (TVar PromiseState, TmpDest) -> STM (Fin.Cell ImportRef)
newImport importId conn promiseState = getLive conn >>= \conn'@Conn'{imports} -> do
    localRc <- Rc.new () $ releaseImport importId conn'
    proxies <- ExportMap <$> M.new
    let importRef = ImportRef
                { conn
                , importId
                , proxies
                }
    M.insert EntryI
        { localRc
        , remoteRc = 1
        , proxies
        , promiseState
        }
        importId
        imports
    cell <- Fin.newCell importRef
    queueIO conn' $ Fin.addFinalizer cell $ atomically (Rc.decr localRc)
    pure cell

-- | Release the identified import. Removes it from the table and sends a release
-- message with the correct count.
releaseImport :: IEId -> Conn' -> STM ()
releaseImport importId conn'@Conn'{imports} = do
    lookupAbort "imports" conn' imports importId $ \EntryI { remoteRc } ->
        sendPureMsg conn' $ R.Message'release
            R.Release
                { id = ieWord importId
                , referenceCount = remoteRc
                }
    M.delete importId imports

-- | Create a new client targeting an object in our answers table.
-- Important: in this case the 'PromisedAnswer' refers to a question we
-- have recevied, not sent.
newLocalAnswerClient :: Conn' -> PromisedAnswer -> STM Client
newLocalAnswerClient conn@Conn'{answers} PromisedAnswer{ answerId, transform } = do
    callBuffer <- newTQueue
    let tmpDest = LocalDest $ LocalBuffer { callBuffer }
    pState <- newTVar Pending { tmpDest }
    subscribeReturn "answer" conn answers answerId $
        resolveClientReturn
            tmpDest
            (writeTVar pState)
            conn
            (toList transform)
    exportMap <- ExportMap <$> M.new
    pure $ Client $ Just PromiseClient
        { pState
        , exportMap
        , origTarget = tmpDest
        }


-- Note [Limiting resource usage]
-- ==============================
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
