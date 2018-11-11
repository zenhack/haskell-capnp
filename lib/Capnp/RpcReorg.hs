{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Capnp.RpcReorg
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
    ) where

-- Note [Implementation checklist]
-- ===============================
--
-- While RPC support is still incomplete, we keep a checklist of what
-- is implemented here; this is a little easier than using the bug
-- tracker, just because it's closer at hand.
--
-- * [ ] Handle each message type
--   * [ ] Abort
--   * [ ] Unimplemented
--   * [ ] Level 0 messages
--     * [ ] Bootstrap
--     * [ ] Call
--     * [ ] Return
--     * [ ] Finish
--   * [ ] Level 1 messages
--     * [ ] Resolve
--     * [ ] Release
--     * [ ] Disembargo
--   * [x] Level 2 messages (there are none).
--   * [ ] Level 3 messages
--     * [ ] Provide
--     * [ ] Accept
--   * [ ] Level 4 messages
--     * [ ] Join
-- * [ ] Resource limits (see Note [Limiting resource usage])

import Data.Word
import UnliftIO.STM

import Control.Monad  (forever, when)
import Data.Default   (Default(def))
import Supervisors    (Supervisor, withSupervisor)
import UnliftIO.Async (concurrently_)

import qualified StmContainers.Map as M

import Capnp.Message       (ConstMsg)
import Capnp.Promise       (breakPromise)
import Capnp.Rpc.Transport (Transport(recvMsg, sendMsg))

import qualified Capnp.Gen.Capnp.Rpc.Pure as RpcGen
import qualified Capnp.Rpc.Object         as Object

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
    { sendQ          :: TBQueue ConstMsg
    , recvQ          :: TBQueue ConstMsg
    -- queues of messages to send and receive; each of these has a dedicated
    -- thread doing the IO (see 'sendLoop' and 'recvLoop'):

    , supervisor     :: Supervisor
    -- Supervisor managing the lifetimes of threads bound to this connection.

    , questionIdPool :: IdPool
    , exportIdPool   :: IdPool
    -- Pools of identifiers for new questions and exports

    -- TODO: the four tables.

    , embargos       :: M.Map EmbargoId (STM ())
    -- Outstanding embargos. When we receive a 'Disembargo' message with its
    -- context field set to receiverLoopback, we look up the embargo id in
    -- this table, and execute the STM we have registered.
    }

-- | Configuration information for a connection.
data ConnConfig = ConnConfig
    { maxQuestions :: !Word32
    -- ^ The maximum number of simultanious outstanding requests to the peer
    -- vat. Once this limit is reached, further questsions will block until
    -- some of the existing questions have been answered.
    --
    -- Defaults to 32.

    , maxExports   :: !Word32
    -- ^ The maximum number of objects which may be exported on this connection.
    --
    -- Defaults to 32.
    }

instance Default ConnConfig where
    def = ConnConfig
        { maxQuestions = 32
        , maxExports   = 32
        }

-- | Get a new question id. retries if we are out of available question ids.
newQuestion :: Conn -> STM QuestionId
newQuestion Conn{questionIdPool} = newId questionIdPool

-- | Return a question id to the pool of available ids.
freeQuestion :: Conn -> QuestionId -> STM ()
freeQuestion = freeId . questionIdPool

-- | Get a new export id. retries if we are out of available export ids.
newExport :: Conn -> STM ExportId
newExport Conn{exportIdPool} = newId exportIdPool

-- | Return a export id to the pool of available ids.
freeExport :: Conn -> ExportId -> STM ()
freeExport = freeId . exportIdPool

-- | Handle a connection to another vat. Returns when the connection is closed.
handleConn :: ConnConfig -> Transport -> IO ()
handleConn ConnConfig{maxQuestions, maxExports} transport =
    withSupervisor $ \sup -> do
        conn <- atomically $ do
            questionIdPool <- newIdPool maxQuestions
            exportIdPool <- newIdPool maxExports

            sendQ <- newTBQueue $ fromIntegral maxQuestions
            recvQ <- newTBQueue $ fromIntegral maxQuestions

            embargos <- M.new

            pure Conn
                { supervisor = sup
                , questionIdPool
                , exportIdPool
                , recvQ
                , sendQ
                , embargos
                }

        coordinator conn
            `concurrently_` sendLoop transport conn
            `concurrently_` recvLoop transport conn


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

-- See Note [Client representation]
data Client'
    -- | A client which always throws an exception in response
    -- to calls.
    = ExnClient RpcGen.Exception
    -- | A client which lives in the same vat/process as us.
    | LocalClient
        { refCount :: TVar Word32
        -- ^ The number of live references to this object. When this
        -- reaches zero, we will tell the receiver to stop.
        , opQueue  :: TQueue Object.ReceiverOp
        -- ^ A queue for submitting commands to the thread managing the
        -- object.
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
    = Answer !AnswerId
    -- ^ Targets an entry in the remote vat's answers table/local vat's
    -- questions table.
    | Import
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
            -- Refcount is zero. Tell the receiver to stop:
            writeTQueue opQueue Object.Stop
            -- ...and then replace ourselves with a disconnected client:
            writeTVar clientVar disconnectedClient'

    -- TODO: RemoteClient


-- | Call a method on the object pointed to by this client.
call :: Object.CallInfo -> Client -> STM ()
call info (Client Nothing) =
    breakPromise (Object.response info) def
        { RpcGen.type_ = RpcGen.Exception'Type'failed
        , RpcGen.reason = "Client is null"
        }
call info (Client (Just clientVar)) =
    readTVar clientVar >>= \case
        ExnClient e ->
            breakPromise (Object.response info) e

        LocalClient{opQueue} ->
            writeTQueue opQueue (Object.Call info)

    -- TODO: RemoteClient


-- | The coordinator processes incoming messages.
coordinator :: Conn -> IO ()
-- The logic here mostly routes messages to other parts of the code that know
-- more about the objects in question; See Note [Organization] for more info.
coordinator Conn{recvQ} = atomically $ do
    msg <- readTBQueue recvQ
    error "TODO"

-- | 'sendLoop' shunts messages from the send queue into the transport.
sendLoop :: Transport -> Conn -> IO ()
sendLoop transport Conn{sendQ} =
    forever $ atomically (readTBQueue sendQ) >>= sendMsg transport

-- | 'recvLoop' shunts messages from the transport into the receive queue.
recvLoop :: Transport -> Conn -> IO ()
recvLoop transport Conn{recvQ} =
    forever $ recvMsg transport >>= atomically . writeTBQueue recvQ


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
