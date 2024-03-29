{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Capnp.Rpc.Untyped
-- Description: Core of the RPC subsystem.
--
-- This module does not deal with schema-level concepts; all capabilities,
-- methods etc. as used here are untyped.
module Capnp.Rpc.Untyped
  ( -- * Connections to other vats
    Conn,
    ConnConfig (..),
    handleConn,
    withConn,
    acquireConn,
    requestBootstrap,

    -- * Clients for capabilities
    Client,
    call,
    nullClient,
    newPromiseClient,
    IsClient (..),

    -- * Promise pipelining
    Pipeline,
    walkPipelinePtr,
    pipelineClient,
    waitPipeline,

    -- * Exporting local objects
    export,
    clientMethodHandler,

    -- * Unwrapping local clients
    unwrapServer,

    -- * Waiting for resolution
    waitClient,

    -- * Errors
    RpcError (..),

    -- * Shutting down the connection
  )
where

import Capnp.Accessors
import qualified Capnp.Basics as B
import Capnp.Bits (WordCount, bytesToWordsFloor)
import Capnp.Classes (new, newRoot, parse)
import Capnp.Convert (msgToRaw, parsedToMsg)
import Capnp.Fields (Which)
import qualified Capnp.Gen.Capnp.Rpc as R
import Capnp.Message (Message)
import qualified Capnp.Message as Message
import Capnp.Mutability (Mutability (..), thaw)
import Capnp.Repr (Raw (..), ReprFor)
import Capnp.Rpc.Errors
  ( eDisconnected,
    eFailed,
    eMethodUnimplemented,
    eUnimplemented,
    wrapException,
  )
import Capnp.Rpc.Promise
  ( Fulfiller,
    Promise,
    breakOrFulfill,
    breakPromise,
    fulfill,
    newCallback,
    newPromise,
    newReadyPromise,
  )
import qualified Capnp.Rpc.Server as Server
import Capnp.Rpc.Transport (Transport (recvMsg, sendMsg))
import Capnp.TraversalLimit (LimitT, defaultLimit, evalLimitT)
import qualified Capnp.Untyped as UntypedRaw
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, cancel, concurrently_, race, race_, wait, withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Concurrent.STM
import Control.Exception.Safe
  ( Exception,
    MonadThrow,
    SomeException,
    bracket,
    finally,
    fromException,
    throwIO,
    throwM,
    try,
  )
import Control.Monad (forever, join, void, when)
import Control.Monad.STM.Class
import Control.Monad.Trans.Class
import Data.Default (Default (def))
import Data.Dynamic (fromDynamic)
import Data.Either (fromLeft)
import Data.Foldable (for_, toList, traverse_)
import Data.Function ((&))
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Data.Word
import qualified Focus
import GHC.Generics (Generic)
import GHC.Prim (coerce)
import Internal.BuildPure (createPure)
import Internal.Rc (Rc)
import qualified Internal.Rc as Rc
import Internal.Rpc.Breaker
import Internal.SnocList (SnocList)
import qualified Internal.SnocList as SnocList
import qualified Internal.TCloseQ as TCloseQ
import Lifetimes (Acquire, mkAcquire, withAcquire)
import Lifetimes.Async (acquireAsync)
import qualified Lifetimes.Gc as Fin
import qualified ListT
import qualified StmContainers.Map as M
import Supervisors (Supervisor, superviseSTM, withSupervisor)
import System.Mem.StableName (StableName, hashStableName, makeStableName)
import System.Timeout (timeout)

-- Note [Organization]
-- ===================
--
-- As much as possible, the logic in this module is centralized according to
-- type types of objects it concerns.
--
-- As an example, consider how we handle embargos: The 'Conn' type's 'embargos'
-- table has values that are just 'Fulfiller's. This allows the code which triggers
-- sending embargoes to have full control over what happens when they return,
-- while the code that routes incoming messages (in 'recvLoop') doesn't need
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

-- Note [Breaker]
-- ==============
--
-- Since capabilities can be stored in messages, it is somewhat challenging
-- to design a module structure that avoids low-level capnp serialization code
-- depending on the rpc system, simply because it needs to pass the 'Client'
-- type around, even if it doesn't do much else with it.
--
-- Earlier versions of this library capitulated and introduced a cyclic
-- dependency; there was a .hs-boot file for this module exposing 'Client'
-- and a couple other things, and "Capnp.Message" and a few other
-- serialization modules imported it.
--
-- This was a problem for a couple reasons:
--

-- * Not only was there a cyclic dependency, the path it took went through

--   a large fraction of the library, meaning whenever any of those modules
--   changes most of the library had to be rebuilt.

-- * It precluded doing things like splitting rpc support into a separate

--   package, for consumers who only want serialization and want a more
--   minimal dependency footprint.
--
-- Instead, the current solution is the "Internal.Rpc.Breaker" module; it
-- defines the few things needed by serialization code, but it does so
-- in a way that avoids depending on this module, sacrificing a small
-- amount of type safety by using "Data.Dynamic" instead of referencing
-- the types in this module directly. While in principle a caller could
-- supply some other type, we expect that:
--

-- * 'Client' will always wrap a @Maybe Client'@.

-- * 'Pipeline' will always wrap a @Pipeline'@.

--
-- we provide wrap/unwrap helper functions for each of these, to keep
-- the type-unsafety that comes with this as localized as possible.

-- | We use this type often enough that the types get noisy without a shorthand:
type RawMPtr = Maybe (UntypedRaw.Ptr 'Const)

-- | Errors which can be thrown by the rpc system.
data RpcError
  = -- | The remote vat sent us an abort message.
    ReceivedAbort (R.Parsed R.Exception)
  | -- | We sent an abort to the remote vat.
    SentAbort (R.Parsed R.Exception)
  deriving (Show, Eq, Generic)

makeAbortExn :: Bool -> SomeException -> RpcError
makeAbortExn debugMode e =
  fromMaybe
    (SentAbort (wrapException debugMode e))
    (fromException e)

instance Exception RpcError

newtype EmbargoId = EmbargoId {embargoWord :: Word32} deriving (Eq, Hashable)

newtype QAId = QAId {qaWord :: Word32} deriving (Eq, Hashable)

newtype IEId = IEId {ieWord :: Word32} deriving (Eq, Hashable)

-- We define these to just show the number; the derived instances would include
-- data constructors, which is a bit weird since these show up in output that
-- is sometimes shown to users.
instance Show QAId where
  show = show . qaWord

instance Show IEId where
  show = show . ieWord

-- | A connection to a remote vat
data Conn = Conn
  { stableName :: StableName (MVar ()),
    -- So we can use the connection as a map key. The MVar used to create
    -- this is just an arbitrary value; the only property we care about
    -- is that it is distinct for each 'Conn', so we use something with
    -- reference semantics to guarantee this.

    debugMode :: !Bool,
    -- whether to include extra (possibly sensitive) info in error messages.

    done :: Async (),
    -- finished when the connection is shut down. Note: this should not be used
    -- by most of the code within this module; see the implementation of acquireConn.
    liveState :: TVar LiveState
  }

data LiveState
  = Live Conn'
  | Dead

data Conn' = Conn'
  { sendQ :: TChan (Message 'Const, Fulfiller ()),
    -- queue of messages to send sent to the remote vat; these are actually
    -- sent by a dedicated thread (see 'sendLoop').
    --
    -- The fulfiller is fulfilled after the message actually hits the transport.
    --
    -- The queue mainly exists for the sake of messages that are sent *while
    -- processing incomming messages*, since we cannot block in those cases,
    -- but it is used for all message sends to enforce ordering. The fulfiller
    -- is used by parts of the code (basically just calls) that want to block
    -- until their message is actually written to the socket.

    availableCallWords :: TVar WordCount,
    -- Semaphore used to limit the memory that can be used by in-progress
    -- calls originating from this connection. We don't just use a TSem
    -- because waitTSem doesn't let us wait for more than one token with a
    -- single call.

    supervisor :: Supervisor,
    -- Supervisor managing the lifetimes of threads bound to this connection.

    questionIdPool :: IdPool,
    exportIdPool :: IdPool,
    -- Pools of identifiers for new questions and exports

    questions :: M.Map QAId EntryQA,
    answers :: M.Map QAId EntryQA,
    exports :: M.Map IEId EntryE,
    imports :: M.Map IEId EntryI,
    embargos :: M.Map EmbargoId (Fulfiller ()),
    -- Outstanding embargos. When we receive a 'Disembargo' message with its
    -- context field set to receiverLoopback, we look up the embargo id in
    -- this table, and fulfill the promise.

    pendingCallbacks :: TQueue (IO ()),
    -- See Note [callbacks]

    myBootstrap :: Maybe Client
    -- The capability which should be served as this connection's bootstrap
    -- interface (if any).
  }

instance Eq Conn where
  x == y = stableName x == stableName y

instance Hashable Conn where
  hash Conn {stableName} = hashStableName stableName
  hashWithSalt _ = hash

-- | Configuration information for a connection.
data ConnConfig = ConnConfig
  { -- | The maximum number of simultanious outstanding requests to the peer
    -- vat. Once this limit is reached, further questsions will block until
    -- some of the existing questions have been answered.
    --
    -- Defaults to 128.
    maxQuestions :: !Word32,
    -- | The maximum number of objects which may be exported on this connection.
    --
    -- Defaults to 8192.
    maxExports :: !Word32,
    -- | The maximum total size of outstanding call messages that will be
    -- accepted; if this limit is reached, the implementation will not read
    -- more messages from the connection until some calls have completed
    -- and freed up enough space.
    --
    -- Defaults to 32MiB in words.
    maxCallWords :: !WordCount,
    -- | In debug mode, errors reported by the RPC system to its peers will
    -- contain extra information. This should not be used in production, as
    -- it is possible for these messages to contain sensitive information,
    -- but it can be useful for debugging.
    --
    -- Defaults to 'False'.
    debugMode :: !Bool,
    -- | The bootstrap interface we should serve for this connection.
    -- If 'bootstrap' is 'Nothing' (the default), we will respond to
    -- bootstrap messages with an exception.
    bootstrap :: Maybe Client
  }

instance Default ConnConfig where
  def =
    ConnConfig
      { maxQuestions = 128,
        maxExports = 8192,
        maxCallWords = bytesToWordsFloor $ 32 * 1024 * 1024,
        debugMode = False,
        bootstrap = Nothing
      }

-- | Queue an IO action to be run some time after this transaction commits.
-- See Note [callbacks].
queueIO :: Conn' -> IO () -> STM ()
queueIO Conn' {pendingCallbacks} = writeTQueue pendingCallbacks

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

-- | Wait until a connection has shut down, then re-throw the
-- exception that killed it, if any.
waitConn :: Conn -> IO ()
waitConn Conn {done} = wait done

-- | Handle a connection to another vat. Returns when the connection is closed.
handleConn ::
  Transport ->
  ConnConfig ->
  IO ()
handleConn transport cfg =
  withAcquire (acquireConn transport cfg) waitConn

-- | Run the function with access to a connection. Shut down the connection
-- when it returns.
withConn :: Transport -> ConnConfig -> (Conn -> IO a) -> IO a
withConn transport cfg f =
  withAcquire (acquireConn transport cfg) $ \conn -> do
    result <- f conn `race` waitConn conn
    case result of
      Left v -> pure v
      Right () ->
        error "BUG: waitConn returned normally"

acquireConn :: Transport -> ConnConfig -> Acquire Conn
acquireConn transport cfg = do
  (conn, conn') <- mkAcquire (newConn cfg) stopConn
  done <- acquireAsync $ runConn cfg transport (conn, conn')
  pure conn {done = done}

newConn
  cfg@ConnConfig
    { maxQuestions,
      maxExports,
      maxCallWords,
      debugMode
    } = do
    stableName <- makeStableName =<< newEmptyMVar
    atomically $ do
      questionIdPool <- newIdPool maxQuestions
      exportIdPool <- newIdPool maxExports

      sendQ <- newTChan

      availableCallWords <- newTVar maxCallWords

      questions <- M.new
      answers <- M.new
      exports <- M.new
      imports <- M.new

      embargos <- M.new
      pendingCallbacks <- newTQueue

      let conn' =
            Conn'
              { questionIdPool,
                exportIdPool,
                sendQ,
                availableCallWords,
                questions,
                answers,
                exports,
                imports,
                embargos,
                pendingCallbacks,
                myBootstrap = bootstrap cfg
              }
      liveState <- newTVar (Live conn')
      let conn =
            Conn
              { stableName,
                debugMode,
                liveState
              }
      pure (conn, conn')

runConn cfg transport (conn, conn') = do
  result <-
    try $
      recvLoop transport conn
        `concurrently_` sendLoop transport conn'
        `concurrently_` callbacksLoop conn'
  case result of
    Left (SentAbort e) -> do
      -- We need to actually send it:
      rawMsg <- createPure maxBound $ parsedToMsg $ R.Message'abort e
      void $ timeout 1000000 $ sendMsg transport rawMsg
      throwIO $ SentAbort e
    Left e ->
      throwIO e
    Right _ ->
      pure ()

stopConn
  ( conn@Conn {liveState},
    conn'@Conn' {questions, exports, embargos}
    ) = do
    atomically $ do
      let walk table = flip ListT.traverse_ (M.listT table)
      -- drop the bootstrap interface:
      case myBootstrap conn' of
        Just (unwrapClient -> Just client') -> dropConnExport conn client'
        _ -> pure ()
      -- Remove everything from the exports table:
      walk exports $ \(_, EntryE {client}) ->
        dropConnExport conn client
      -- Outstanding questions should all throw disconnected:
      walk questions $ \(qid, entry) ->
        let raiseDisconnected onReturn =
              mapQueueSTM conn' onReturn $
                Return
                  { answerId = qid,
                    releaseParamCaps = False,
                    union' = Return'exception eDisconnected
                  }
         in case entry of
              NewQA {onReturn} -> raiseDisconnected onReturn
              HaveFinish {onReturn} -> raiseDisconnected onReturn
              _ -> pure ()
      -- same thing with embargos:
      walk embargos $ \(_, fulfiller) ->
        breakPromise fulfiller eDisconnected
      -- mark the connection as dead, making the live state inaccessible:
      writeTVar liveState Dead

-- | A pool of ids; used when choosing identifiers for questions and exports.
newtype IdPool = IdPool (TVar [Word32])

-- | @'newIdPool' size@ creates a new pool of ids, with @size@ available ids.
newIdPool :: Word32 -> STM IdPool
newIdPool size = IdPool <$> newTVar [0 .. size - 1]

-- | Get a new id from the pool. Retries if the pool is empty.
newId :: IdPool -> STM Word32
newId (IdPool pool) =
  readTVar pool >>= \case
    [] -> retry
    (id : ids) -> do
      writeTVar pool $! ids
      pure id

-- | Return an id to the pool.
freeId :: IdPool -> Word32 -> STM ()
freeId (IdPool pool) id = modifyTVar' pool (id :)

-- | An entry in our questions or answers table.
data EntryQA
  = -- | An entry for which we have neither sent/received a finish, nor
    -- a return. Contains two sets of callbacks, one to invoke on each
    -- type of message.
    NewQA
      { onFinish :: SnocList (R.Parsed R.Finish -> STM ()),
        onReturn :: SnocList (Return -> STM ())
      }
  | -- | An entry for which we've sent/received a return, but not a finish.
    -- Contains the return message, and a set of callbacks to invoke on the
    -- finish.
    HaveReturn
      { returnMsg :: Return,
        onFinish :: SnocList (R.Parsed R.Finish -> STM ())
      }
  | -- | An entry for which we've sent/received a finish, but not a return.
    -- Contains the finish message, and a set of callbacks to invoke on the
    -- return.
    HaveFinish
      { finishMsg :: R.Parsed R.Finish,
        onReturn :: SnocList (Return -> STM ())
      }

-- | An entry in our imports table.
data EntryI = EntryI
  { -- | A refcount cell with a finalizer attached to it; when the finalizer
    -- runs it will remove this entry from the table and send a release
    -- message to the remote vat.
    localRc :: Rc (),
    -- | The reference count for this object as understood by the remote
    -- vat. This tells us what to send in the release message's count field.
    remoteRc :: !Word32,
    -- | See Note [proxies]
    proxies :: ExportMap,
    -- | If this entry is a promise, this will contain the state of that
    -- promise, so that it may be used to create PromiseClients and
    -- update the promise when it resolves.
    promiseState ::
      Maybe
        ( TVar PromiseState,
          TmpDest -- origTarget field. TODO(cleanup): clean this up a bit.
        )
  }

-- | An entry in our exports table.
data EntryE = EntryE
  { -- | The client. We cache it in the table so there's only one object
    -- floating around, which lets us attach a finalizer without worrying
    -- about it being run more than once.
    client :: Client',
    -- | The refcount for this entry. This lets us know when we can drop
    -- the entry from the table.
    refCount :: !Word32
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

-- | See Note [Breaker]
wrapClient :: Maybe Client' -> Client
wrapClient = Client . makeOpaque

-- | See Note [Breaker]
unwrapClient :: Client -> Maybe Client'
unwrapClient (Client o) =
  join $ fromDynamic $ reflectOpaque o

data Client'
  = -- | A client pointing at a capability local to our own vat.
    LocalClient
      { -- | Record of what export IDs this client has on different remote
        -- connections.
        exportMap :: ExportMap,
        -- | Queue a call for the local capability to handle. This is wrapped
        -- in a reference counted cell, whose finalizer stops the server.
        qCall :: Rc (Server.CallInfo -> STM ()),
        -- | Finalizer key; when this is collected, qCall will be released.
        finalizerKey :: Fin.Cell (),
        unwrapper :: forall a. Typeable a => Maybe a
      }
  | -- | A client which will resolve to some other capability at
    -- some point.
    PromiseClient
      { -- | The current state of the promise; the indirection allows
        -- the promise to be updated.
        pState :: TVar PromiseState,
        exportMap :: ExportMap,
        -- | The original target of this promise, before it was resolved.
        -- (if it is still in the pending state, it will match the TmpDest
        -- stored there).
        --
        -- FIXME: if this is an ImportDest, by holding on to this we actually
        -- leak the cap.
        origTarget :: TmpDest
      }
  | -- | A client which points to a (resolved) capability in a remote vat.
    ImportClient (Fin.Cell ImportRef)

data Pipeline' = Pipeline'
  { state :: TVar PipelineState,
    steps :: SnocList Word16
  }
  deriving (Eq)

-- | See Note [Breaker]
wrapPipeline :: Pipeline' -> Pipeline
wrapPipeline = Pipeline . makeOpaque

-- | See Note [Breaker]
unwrapPipeline :: Pipeline -> Pipeline'
unwrapPipeline (Pipeline o) =
  case fromDynamic (reflectOpaque o) of
    Nothing -> error "invalid pipeline; dynamic unwrap failed"
    Just p -> p

data PipelineState
  = PendingRemotePipeline
      { answerId :: !QAId,
        clientMap :: M.Map (SnocList Word16) Client,
        conn :: Conn
      }
  | PendingLocalPipeline (SnocList (Fulfiller RawMPtr))
  | ReadyPipeline (Either (R.Parsed R.Exception) RawMPtr)

-- | 'walkPipleinePtr' follows a pointer starting from the object referred to by the
-- 'Pipeline'. The 'Pipeline' must refer to a struct, and the pointer is referred to
-- by its index into the struct's pointer section.
walkPipelinePtr :: Pipeline -> Word16 -> Pipeline
walkPipelinePtr (unwrapPipeline -> p@Pipeline' {steps}) step =
  wrapPipeline $ p {steps = SnocList.snoc steps step}

-- | Convert a 'Pipeline' into a 'Client', which can be used to send messages to the
-- referant of the 'Pipeline', using promise pipelining.
pipelineClient :: MonadSTM m => Pipeline -> m Client
pipelineClient (unwrapPipeline -> Pipeline' {state, steps}) = liftSTM $ do
  readTVar state >>= \case
    PendingRemotePipeline {answerId, clientMap, conn} -> do
      maybeClient <- M.lookup steps clientMap
      case maybeClient of
        Nothing -> do
          client <-
            promisedAnswerClient
              conn
              PromisedAnswer {answerId, transform = steps}
          M.insert client steps clientMap
          pure client
        Just client ->
          pure client
    PendingLocalPipeline subscribers -> do
      (ret, retFulfiller) <- newPromiseClient
      ptrFulfiller <- newCallback $ \r -> do
        writeTVar state (ReadyPipeline r)
        case r of
          Left e ->
            breakPromise retFulfiller e
          Right v ->
            (ptrPathClient (toList steps) v >>= fulfill retFulfiller)
              `catchSTM` (breakPromise retFulfiller . wrapException False)
      writeTVar state $ PendingLocalPipeline $ SnocList.snoc subscribers ptrFulfiller
      pure ret
    ReadyPipeline r -> do
      -- TODO(cleanup): factor out the commonalities between this and the above case.
      (p, f) <- newPromiseClient
      case r of
        Left e -> breakPromise f e >> pure p
        Right v ->
          ptrPathClient (toList steps) v
            `catchSTM` ( \e -> do
                           breakPromise f (wrapException False e)
                           pure p
                       )

-- | Wait for the pipeline's target to resolve, and return the corresponding
-- pointer.
waitPipeline :: MonadSTM m => Pipeline -> m RawMPtr
waitPipeline (unwrapPipeline -> Pipeline' {state, steps}) = liftSTM $ do
  s <- readTVar state
  case s of
    ReadyPipeline (Left e) ->
      throwM e
    ReadyPipeline (Right v) ->
      evalLimitT defaultLimit $ followPtrs (toList steps) v
    _ ->
      retry

promisedAnswerClient :: Conn -> PromisedAnswer -> STM Client
promisedAnswerClient conn answer@PromisedAnswer {answerId, transform} = do
  let tmpDest = RemoteDest AnswerDest {conn, answer}
  pState <- newTVar Pending {tmpDest}
  exportMap <- ExportMap <$> M.new
  let client =
        wrapClient $
          Just
            PromiseClient
              { pState,
                exportMap,
                origTarget = tmpDest
              }
  readTVar (liveState conn) >>= \case
    Dead ->
      resolveClientExn tmpDest (writeTVar pState) eDisconnected
    Live conn'@Conn' {questions} ->
      subscribeReturn "questions" conn' questions answerId $
        resolveClientReturn tmpDest (writeTVar pState) conn' (toList transform)
  pure client

-- | The current state of a 'PromiseClient'.
data PromiseState
  = -- | The promise is fully resolved.
    Ready
      { -- | Capability to which the promise resolved.
        target :: Client
      }
  | -- | The promise has resolved, but is waiting on a Disembargo message
    -- before it is safe to send it messages.
    Embargo
      { -- | A queue in which to buffer calls while waiting for the
        -- disembargo.
        callBuffer :: TQueue Server.CallInfo
      }
  | -- | The promise has not yet resolved.
    Pending
      { -- | A temporary destination to send calls, while we wait for the
        -- promise to resolve.
        tmpDest :: TmpDest
      }
  | -- | The promise resolved to an exception.
    Error (R.Parsed R.Exception)

-- | A temporary destination for calls on an unresolved promise.
data TmpDest
  = -- | A destination that is local to this vat.
    LocalDest LocalDest
  | -- | A destination in another vat.
    RemoteDest RemoteDest

newtype LocalDest
  = -- | Queue the calls in a buffer.
    LocalBuffer {callBuffer :: TQueue Server.CallInfo}

data RemoteDest
  = -- | Send call messages to a remote vat, targeting the results
    -- of an outstanding question.
    AnswerDest
      { -- | The connection to the remote vat.
        conn :: Conn,
        -- | The answer to target.
        answer :: PromisedAnswer
      }
  | -- | Send call messages to a remote vat, targeting an entry in our
    -- imports table.
    ImportDest (Fin.Cell ImportRef)

-- | A reference to a capability in our import table/a remote vat's export
-- table.
data ImportRef = ImportRef
  { -- | The connection to the remote vat.
    conn :: Conn,
    -- | The import id for this capability.
    importId :: !IEId,
    -- | Export ids to use when this client is passed to a vat other than
    -- the one identified by 'conn'. See Note [proxies]
    proxies :: ExportMap
  }

-- Ideally we could just derive these, but stm-containers doesn't have Eq
-- instances, so neither does ExportMap. not all of the fields are actually
-- necessary to check equality though. See also
-- https://github.com/nikita-volkov/stm-hamt/pull/1
instance Eq ImportRef where
  ImportRef {conn = cx, importId = ix} == ImportRef {conn = cy, importId = iy} =
    cx == cy && ix == iy

instance Eq Client' where
  LocalClient {qCall = x} == LocalClient {qCall = y} =
    x == y
  PromiseClient {pState = x} == PromiseClient {pState = y} =
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

-- The below correspond to the similarly named types in
-- rpc.capnp, except:
--

-- * They use our newtype wrappers for ids

-- * They don't have unknown variants

-- * AnyPointers are left un-parsed

-- * PromisedAnswer's transform field is just a list of pointer offsets,

--   rather than a union with no other actually-useful variants.

-- * PromisedAnswer's transform field is a SnocList, for efficient appending.

data MsgTarget
  = ImportTgt !IEId
  | AnswerTgt PromisedAnswer

data PromisedAnswer = PromisedAnswer
  { answerId :: !QAId,
    transform :: SnocList Word16
  }

data Call = Call
  { questionId :: !QAId,
    target :: !MsgTarget,
    interfaceId :: !Word64,
    methodId :: !Word16,
    params :: !Payload
  }

data Return = Return
  { answerId :: !QAId,
    releaseParamCaps :: !Bool,
    union' :: Return'
  }

data Return'
  = Return'results Payload
  | Return'exception (R.Parsed R.Exception)
  | Return'canceled
  | Return'resultsSentElsewhere
  | Return'takeFromOtherQuestion QAId
  | Return'acceptFromThirdParty RawMPtr

data Payload = Payload
  { content :: RawMPtr,
    capTable :: [R.Parsed R.CapDescriptor]
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
call :: MonadSTM m => Server.CallInfo -> Client -> m (Promise Pipeline)
call Server.CallInfo {response} (unwrapClient -> Nothing) = liftSTM $ do
  breakPromise response eMethodUnimplemented
  state <- newTVar $ ReadyPipeline (Left eMethodUnimplemented)
  newReadyPromise $ wrapPipeline Pipeline' {state, steps = mempty}
call info@Server.CallInfo {response} (unwrapClient -> Just client') = liftSTM $ do
  (localPipeline, response') <- makeLocalPipeline response
  let info' = info {Server.response = response'}
  case client' of
    LocalClient {qCall} -> do
      Rc.get qCall >>= \case
        Just q -> do
          q info'
        Nothing ->
          breakPromise response' eDisconnected
      newReadyPromise localPipeline
    PromiseClient {pState} ->
      readTVar pState >>= \case
        Ready {target} ->
          call info target
        Embargo {callBuffer} -> do
          writeTQueue callBuffer info'
          newReadyPromise localPipeline
        Pending {tmpDest} -> case tmpDest of
          LocalDest LocalBuffer {callBuffer} -> do
            writeTQueue callBuffer info'
            newReadyPromise localPipeline
          RemoteDest AnswerDest {conn, answer} ->
            callRemote conn info $ AnswerTgt answer
          RemoteDest (ImportDest cell) -> do
            ImportRef {conn, importId} <- Fin.readCell cell
            callRemote conn info $ ImportTgt importId
        Error exn -> do
          breakPromise response' exn
          newReadyPromise localPipeline
    ImportClient cell -> do
      ImportRef {conn, importId} <- Fin.readCell cell
      callRemote conn info (ImportTgt importId)

makeLocalPipeline :: Fulfiller RawMPtr -> STM (Pipeline, Fulfiller RawMPtr)
makeLocalPipeline f = do
  state <- newTVar $ PendingLocalPipeline mempty
  f' <- newCallback $ \r -> do
    s <- readTVar state
    case s of
      PendingLocalPipeline fs -> do
        writeTVar state (ReadyPipeline r)
        breakOrFulfill f r
        traverse_ (`breakOrFulfill` r) fs
      _ ->
        -- TODO(cleanup): refactor so we don't need this case.
        error "impossible"
  pure (wrapPipeline Pipeline' {state, steps = mempty}, f')

-- | Send a call to a remote capability.
callRemote :: Conn -> Server.CallInfo -> MsgTarget -> STM (Promise Pipeline)
callRemote
  conn
  Server.CallInfo {interfaceId, methodId, arguments, response}
  target = do
    conn'@Conn' {questions} <- getLive conn
    qid <- newQuestion conn'
    payload@Payload {capTable} <- makeOutgoingPayload conn arguments
    -- save these in case the callee sends back releaseParamCaps = True in the return
    -- message:
    let paramCaps = catMaybes $ flip map capTable $ \R.CapDescriptor {union'} -> case union' of
          R.CapDescriptor'senderHosted eid -> Just (IEId eid)
          R.CapDescriptor'senderPromise eid -> Just (IEId eid)
          _ -> Nothing

    clientMap <- M.new
    rp <-
      newTVar
        PendingRemotePipeline
          { answerId = qid,
            clientMap,
            conn
          }

    response' <- newCallback $ \r -> do
      breakOrFulfill response r
      case r of
        Left e -> writeTVar rp $ ReadyPipeline (Left e)
        Right v ->
          writeTVar rp $ ReadyPipeline (Right v)

    M.insert
      NewQA
        { onReturn = SnocList.singleton $ cbCallReturn paramCaps conn response',
          onFinish = SnocList.empty
        }
      qid
      questions
    (p, f) <- newPromise
    f <- newCallback $ \r ->
      breakOrFulfill f (wrapPipeline Pipeline' {state = rp, steps = mempty} <$ r)
    sendCall
      conn'
      Call
        { questionId = qid,
          target = target,
          params = payload,
          interfaceId,
          methodId
        }
      f
    pure p

-- | Callback to run when a return comes in that corresponds to a call
-- we sent. Registered in callRemote. The first argument is a list of
-- export IDs to release if the return message has releaseParamCaps = true.
cbCallReturn :: [IEId] -> Conn -> Fulfiller RawMPtr -> Return -> STM ()
cbCallReturn
  paramCaps
  conn
  response
  Return {answerId, union', releaseParamCaps} = do
    conn'@Conn' {answers} <- getLive conn
    when releaseParamCaps $
      traverse_ (releaseExport conn 1) paramCaps
    case union' of
      Return'exception exn ->
        breakPromise response exn
      Return'results Payload {content} ->
        fulfill response content
      Return'canceled ->
        breakPromise response $ eFailed "Canceled"
      Return'resultsSentElsewhere ->
        -- This should never happen, since we always set
        -- sendResultsTo = caller
        abortConn conn' $
          eFailed $
            mconcat
              [ "Received Return.resultsSentElswhere for a call ",
                "with sendResultsTo = caller."
              ]
      Return'takeFromOtherQuestion qid ->
        -- TODO(cleanup): we should be a little stricter; the protocol
        -- requires that (1) each answer is only used this way once, and
        -- (2) The question was sent with sendResultsTo set to 'yourself',
        -- but we don't enforce either of these requirements.
        subscribeReturn "answer" conn' answers qid $
          cbCallReturn [] conn response
      Return'acceptFromThirdParty _ ->
        -- Note [Level 3]
        abortConn conn' $
          eUnimplemented
            "This vat does not support level 3."
    -- Defer this until after any other callbacks run, in case disembargos
    -- need to be sent due to promise resolutions that we triggered:
    queueSTM conn' $
      finishQuestion
        conn'
        def
          { R.questionId = qaWord answerId,
            R.releaseResultCaps = False
          }

marshalMsgTarget :: MsgTarget -> R.Parsed R.MessageTarget
marshalMsgTarget = \case
  ImportTgt importId ->
    R.MessageTarget $ R.MessageTarget'importedCap (ieWord importId)
  AnswerTgt tgt ->
    R.MessageTarget $ R.MessageTarget'promisedAnswer $ marshalPromisedAnswer tgt

marshalPromisedAnswer :: PromisedAnswer -> R.Parsed R.PromisedAnswer
marshalPromisedAnswer PromisedAnswer {answerId, transform} =
  R.PromisedAnswer
    { R.questionId = qaWord answerId,
      R.transform =
        map
          (R.PromisedAnswer'Op . R.PromisedAnswer'Op'getPointerField)
          (toList transform)
    }

unmarshalPromisedAnswer :: MonadThrow m => R.Parsed R.PromisedAnswer -> m PromisedAnswer
unmarshalPromisedAnswer R.PromisedAnswer {questionId, transform} = do
  idxes <- unmarshalOps (toList transform)
  pure
    PromisedAnswer
      { answerId = QAId questionId,
        transform = SnocList.fromList idxes
      }

unmarshalOps :: MonadThrow m => [R.Parsed R.PromisedAnswer'Op] -> m [Word16]
unmarshalOps [] = pure []
unmarshalOps (R.PromisedAnswer'Op {union' = R.PromisedAnswer'Op'noop} : ops) =
  unmarshalOps ops
unmarshalOps (R.PromisedAnswer'Op {union' = R.PromisedAnswer'Op'getPointerField i} : ops) =
  (i :) <$> unmarshalOps ops
unmarshalOps (R.PromisedAnswer'Op {union' = R.PromisedAnswer'Op'unknown' tag} : _) =
  throwM $ eFailed $ "Unknown PromisedAnswer.Op: " <> fromString (show tag)

-- | Create a new client based on a promise. The fulfiller can be used to
-- supply the final client.
newPromiseClient :: (MonadSTM m, IsClient c) => m (c, Fulfiller c)
newPromiseClient = liftSTM $ do
  callBuffer <- newTQueue
  let tmpDest = LocalDest LocalBuffer {callBuffer}
  pState <- newTVar Pending {tmpDest}
  exportMap <- ExportMap <$> M.new
  f <- newCallback $ \case
    Left e -> resolveClientExn tmpDest (writeTVar pState) e
    Right v -> resolveClientClient tmpDest (writeTVar pState) (toClient v)
  let p =
        wrapClient $
          Just $
            PromiseClient
              { pState,
                exportMap,
                origTarget = tmpDest
              }
  pure (fromClient p, f)

-- | Attempt to unwrap a client, to get at an underlying value from the
-- server. Returns 'Nothing' on failure.
--
-- This shells out to the underlying server's implementation of
-- 'Server.unwrap'. It will fail with 'Nothing' if any of these are true:
--
-- * The client is a promise.
-- * The client points to an object in a remote vat.
-- * The underlying Server's 'unwrap' method returns 'Nothing' for type 'a'.
unwrapServer :: (IsClient c, Typeable a) => c -> Maybe a
unwrapServer c = case unwrapClient (toClient c) of
  Just LocalClient {unwrapper} -> unwrapper
  _ -> Nothing

-- | Wait for the client to be fully resolved, and then return a client
-- pointing directly to the destination.
--
-- If the argument is null, a local client, or a (permanent) remote client,
-- this returns the argument immediately. If the argument is a promise client,
-- then this waits for the promise to resolve and returns the result of
-- the resolution. If the promise resolves to *another* promise, then this waits
-- for that promise to also resolve.
--
-- If the promise is rejected, then this throws the corresponding exception.
waitClient :: (IsClient c, MonadSTM m) => c -> m c
waitClient client = liftSTM $ case unwrapClient (toClient client) of
  Nothing -> pure client
  Just LocalClient {} -> pure client
  Just ImportClient {} -> pure client
  Just PromiseClient {pState} -> do
    state <- readTVar pState
    case state of
      Ready {target} -> fromClient <$> waitClient target
      Error e -> throwSTM e
      Pending {} -> retry
      Embargo {} -> retry

-- | Spawn a local server with its lifetime bound to the supervisor,
-- and return a client for it. When the client is garbage collected,
-- the server will be stopped (if it is still running).
export :: MonadSTM m => Supervisor -> Server.ServerOps -> m Client
export sup ops = liftSTM $ do
  q <- TCloseQ.new
  qCall <- Rc.new (TCloseQ.write q) (TCloseQ.close q)
  exportMap <- ExportMap <$> M.new
  finalizerKey <- Fin.newCell ()
  let client' =
        LocalClient
          { qCall,
            exportMap,
            finalizerKey,
            unwrapper = Server.handleCast ops
          }
  superviseSTM
    sup
    ( ( do
          Fin.addFinalizer finalizerKey $ atomically $ Rc.release qCall
          Server.runServer q ops
      )
        `finally` Server.handleStop ops
    )
  pure $ wrapClient (Just client')

clientMethodHandler ::
  forall p r.
  ( ReprFor p ~ ReprFor (Maybe B.AnyPointer),
    ReprFor r ~ ReprFor (Maybe B.AnyPointer)
  ) =>
  Word64 ->
  Word16 ->
  Client ->
  Server.MethodHandler p r
clientMethodHandler interfaceId methodId client =
  Server.castHandler @(Maybe B.AnyPointer) @p @(Maybe B.AnyPointer) @r $
    \(Raw arguments) response ->
      atomically $ void $ call Server.CallInfo {response = coerce response, ..} client

-- | See Note [callbacks]
callbacksLoop :: Conn' -> IO ()
callbacksLoop Conn' {pendingCallbacks} =
  loop `finally` cleanup
  where
    loop =
      forever $
        doCallbacks $
          atomically $
            flushTQueue pendingCallbacks >>= \case
              -- We need to make sure to block if there weren't any jobs, since
              -- otherwise we'll busy loop, pegging the CPU.
              [] -> retry
              cbs -> pure cbs
    cleanup =
      -- Make sure any pending callbacks get run. This is important, since
      -- some of these do things like raise disconnected exceptions.
      doCallbacks $ atomically $ flushTQueue pendingCallbacks
    doCallbacks getCbs =
      -- We need to be careful not to lose any callbacks in the event
      -- of an exception (even an async one):
      bracket
        getCbs
        (foldr finally (pure ()))
        (\_ -> pure ())

-- | 'sendLoop' shunts messages from the send queue into the transport.
sendLoop :: Transport -> Conn' -> IO ()
sendLoop transport Conn' {sendQ} =
  forever $ do
    (msg, f) <- atomically $ readTChan sendQ
    sendMsg transport msg
    atomically $ fulfill f ()

-- | 'recvLoop' processes incoming messages.
recvLoop :: Transport -> Conn -> IO ()
-- The logic here mostly routes messages to other parts of the code that know
-- more about the objects in question; See Note [Organization] for more info.
recvLoop transport conn@Conn {debugMode} = forever $ do
  capnpMsg <- recvMsg transport
  atomically $ do
    flip catchSTM (throwSTM . makeAbortExn debugMode) $ do
      evalLimitT defaultLimit $ do
        rpcMsg <- msgToRaw capnpMsg
        which <- structWhich rpcMsg
        case which of
          R.RW_Message'abort exn ->
            parse exn >>= lift . handleAbortMsg conn
          R.RW_Message'unimplemented oldMsg ->
            parse oldMsg >>= lift . handleUnimplementedMsg conn
          R.RW_Message'bootstrap bs ->
            parse bs >>= lift . handleBootstrapMsg conn
          R.RW_Message'call call -> do
            handleCallMsg conn call
          R.RW_Message'return ret -> do
            ret' <- acceptReturn conn ret
            lift $ handleReturnMsg conn ret'
          R.RW_Message'finish finish ->
            parse finish >>= lift . handleFinishMsg conn
          R.RW_Message'resolve res ->
            parse res >>= lift . handleResolveMsg conn
          R.RW_Message'release release ->
            parse release >>= lift . handleReleaseMsg conn
          R.RW_Message'disembargo disembargo ->
            parse disembargo >>= lift . handleDisembargoMsg conn
          _ -> do
            msg <- parse rpcMsg
            lift $ do
              (_, onSent) <- newPromise
              conn' <- getLive conn
              sendPureMsg conn' (R.Message'unimplemented msg) onSent

-- Each function handle*Msg handles a message of a particular type;
-- 'recvLoop' dispatches to these.

handleAbortMsg :: Conn -> R.Parsed R.Exception -> STM ()
handleAbortMsg _ exn =
  throwSTM (ReceivedAbort exn)

handleUnimplementedMsg :: Conn -> R.Parsed R.Message -> STM ()
handleUnimplementedMsg conn (R.Message msg) =
  getLive conn >>= \conn' -> case msg of
    R.Message'unimplemented _ ->
      -- If the client itself doesn't handle unimplemented messages, that's
      -- weird, but ultimately their problem.
      pure ()
    R.Message'abort _ ->
      abortConn conn' $
        eFailed $
          "Your vat sent an 'unimplemented' message for an abort message "
            <> "that its remote peer never sent. This is likely a bug in your "
            <> "capnproto library."
    _ ->
      abortConn conn' $
        eFailed "Received unimplemented response for required message."

handleBootstrapMsg :: Conn -> R.Parsed R.Bootstrap -> STM ()
handleBootstrapMsg conn R.Bootstrap {questionId} =
  getLive conn >>= \conn' -> do
    ret <- case myBootstrap conn' of
      Nothing ->
        pure
          Return
            { answerId = QAId questionId,
              releaseParamCaps = True, -- Not really meaningful for bootstrap, but...
              union' =
                Return'exception $
                  eFailed "No bootstrap interface for this connection."
            }
      Just client -> do
        capDesc <- emitCap conn client
        content <- fmap Just $ createPure defaultLimit $ do
          msg <- Message.newMessage Nothing
          UntypedRaw.PtrCap <$> UntypedRaw.appendCap msg client
        pure
          Return
            { answerId = QAId questionId,
              releaseParamCaps = True, -- Not really meaningful for bootstrap, but...
              union' =
                Return'results
                  Payload
                    { content,
                      capTable =
                        [def {R.union' = capDesc} :: R.Parsed R.CapDescriptor]
                    }
            }
    M.focus
      (Focus.alterM $ insertBootstrap conn' ret)
      (QAId questionId)
      (answers conn')
    sendReturn conn' ret
  where
    insertBootstrap _ ret Nothing =
      pure $
        Just
          HaveReturn
            { returnMsg = ret,
              onFinish =
                SnocList.fromList
                  [ \R.Finish {releaseResultCaps} ->
                      case ret of
                        Return
                          { union' =
                              Return'results
                                Payload
                                  { capTable = [R.CapDescriptor {union' = R.CapDescriptor'receiverHosted (IEId -> eid)}]
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

handleCallMsg :: Conn -> Raw R.Call 'Const -> LimitT STM ()
handleCallMsg conn callMsg = do
  conn'@Conn' {exports, answers, availableCallWords} <- lift $ getLive conn
  let capnpMsg = UntypedRaw.message @(Raw R.Call) callMsg

  -- Apply backpressure, by limiting the memory usage of outstanding call
  -- messages.
  msgWords <- Message.totalNumWords capnpMsg
  lift $ do
    available <- readTVar availableCallWords
    when
      (msgWords > available)
      retry
    writeTVar availableCallWords $! available - msgWords

  questionId <- parseField #questionId callMsg
  R.MessageTarget target <- parseField #target callMsg
  interfaceId <- parseField #interfaceId callMsg
  methodId <- parseField #methodId callMsg
  payload <- readField #params callMsg

  Payload {content = callParams, capTable} <- acceptPayload conn payload

  lift $ do
    -- First, add an entry in our answers table:
    insertNewAbort
      "answer"
      conn'
      (QAId questionId)
      NewQA
        { onReturn =
            SnocList.fromList
              [ \_ ->
                  modifyTVar' availableCallWords (msgWords +)
              ],
          onFinish =
            SnocList.fromList
              [ \R.Finish {releaseResultCaps} ->
                  when releaseResultCaps $
                    for_ capTable $ \R.CapDescriptor {union'} -> case union' of
                      R.CapDescriptor'receiverHosted (IEId -> importId) ->
                        releaseExport conn 1 importId
                      _ ->
                        pure ()
              ]
        }
      answers

    -- Set up a callback for when the call is finished, to
    -- send the return message:
    fulfiller <- newCallback $ \case
      Left e ->
        returnAnswer
          conn'
          Return
            { answerId = QAId questionId,
              releaseParamCaps = False,
              union' = Return'exception e
            }
      Right content -> do
        capTable <- genSendableCapTableRaw conn content
        returnAnswer
          conn'
          Return
            { answerId = QAId questionId,
              releaseParamCaps = False,
              union' =
                Return'results
                  Payload
                    { content = content,
                      capTable = capTable
                    }
            }
    -- Package up the info for the call:
    let callInfo =
          Server.CallInfo
            { interfaceId,
              methodId,
              arguments = callParams,
              response = coerce fulfiller
            }
    -- Finally, figure out where to send it:
    case target of
      R.MessageTarget'importedCap exportId ->
        lookupAbort "export" conn' exports (IEId exportId) $
          \EntryE {client} -> void $ call callInfo $ wrapClient $ Just client
      R.MessageTarget'promisedAnswer R.PromisedAnswer {questionId = targetQid, transform} ->
        let onReturn ret@Return {union'} =
              case union' of
                Return'exception _ ->
                  returnAnswer conn' ret {answerId = QAId questionId}
                Return'canceled ->
                  returnAnswer conn' ret {answerId = QAId questionId}
                Return'results Payload {content} ->
                  void $ transformClient transform content conn' >>= call callInfo
                Return'resultsSentElsewhere ->
                  -- our implementation should never actually do this, but
                  -- this way we don't have to change this if/when we
                  -- support the feature:
                  abortConn conn' $
                    eFailed $
                      "Tried to call a method on a promised answer that "
                        <> "returned resultsSentElsewhere"
                Return'takeFromOtherQuestion otherQid ->
                  subscribeReturn "answer" conn' answers otherQid onReturn
                Return'acceptFromThirdParty _ ->
                  -- Note [Level 3]
                  error "BUG: our implementation unexpectedly used a level 3 feature"
         in subscribeReturn "answer" conn' answers (QAId targetQid) onReturn
      R.MessageTarget'unknown' ordinal ->
        abortConn conn' $
          eUnimplemented $
            "Unknown MessageTarget ordinal #" <> fromString (show ordinal)

ptrPathClient :: MonadThrow m => [Word16] -> RawMPtr -> m Client
ptrPathClient is ptr =
  evalLimitT defaultLimit $ followPtrs is ptr >>= ptrClient

transformClient :: [R.Parsed R.PromisedAnswer'Op] -> RawMPtr -> Conn' -> STM Client
transformClient transform ptr conn =
  (unmarshalOps transform >>= flip ptrPathClient ptr)
    `catchSTM` abortConn conn

ptrClient :: UntypedRaw.ReadCtx m 'Const => RawMPtr -> m Client
ptrClient Nothing = pure nullClient
ptrClient (Just (UntypedRaw.PtrCap cap)) = UntypedRaw.getClient cap
ptrClient (Just _) = throwM $ eFailed "Tried to call method on non-capability."

-- | Follow a series of pointer indicies, returning the final value, or 'Left'
-- with an error if any of the pointers in the chain (except the last one) is
-- a non-null non struct.
followPtrs :: UntypedRaw.ReadCtx m 'Const => [Word16] -> RawMPtr -> m RawMPtr
followPtrs [] ptr =
  pure ptr
followPtrs (_ : _) Nothing =
  pure Nothing
followPtrs (i : is) (Just (UntypedRaw.PtrStruct struct)) =
  UntypedRaw.getPtr (fromIntegral i) struct >>= followPtrs is
followPtrs (_ : _) (Just _) =
  throwM $ eFailed "Tried to access pointer field of non-struct."

sendRawMsg :: Conn' -> Message 'Const -> Fulfiller () -> STM ()
sendRawMsg conn' msg onSent = writeTChan (sendQ conn') (msg, onSent)

sendCall :: Conn' -> Call -> Fulfiller () -> STM ()
sendCall
  conn'
  Call {questionId, target, interfaceId, methodId, params = Payload {content, capTable}}
  onSent = do
    msg <- createPure defaultLimit $ do
      mcontent <- traverse thaw content
      msg <- case mcontent of
        Just v -> pure $ UntypedRaw.message @UntypedRaw.Ptr v
        Nothing -> Message.newMessage Nothing
      payload <- new @R.Payload () msg
      payload & setField #content (Raw mcontent)
      payload & encodeField #capTable capTable
      call <- new @R.Call () msg
      setField #params payload call
      call & encodeField #questionId (qaWord questionId)
      call & encodeField #interfaceId interfaceId
      call & encodeField #methodId methodId
      call & encodeField #target (marshalMsgTarget target)
      rpcMsg <- newRoot @R.Message () msg
      setVariant #call rpcMsg call
      pure msg
    sendRawMsg conn' msg onSent

sendReturn :: Conn' -> Return -> STM ()
sendReturn conn' Return {answerId, releaseParamCaps, union'} = do
  (_, onSent) <- newPromise
  case union' of
    Return'results Payload {content, capTable} -> do
      msg <- createPure defaultLimit $ do
        mcontent <- traverse thaw content
        msg <- case mcontent of
          Just v -> pure $ UntypedRaw.message @UntypedRaw.Ptr v
          Nothing -> Message.newMessage Nothing
        payload <- new @R.Payload () msg
        payload & setField #content (Raw mcontent)
        payload & encodeField #capTable capTable
        ret <- new @R.Return () msg
        setVariant #results ret payload
        ret & encodeField #answerId (qaWord answerId)
        ret & encodeField #releaseParamCaps releaseParamCaps
        rpcMsg <- newRoot @R.Message () msg
        setVariant #return rpcMsg ret
        pure msg
      sendRawMsg conn' msg onSent
    Return'exception exn ->
      sendPureMsg
        conn'
        ( R.Message'return
            R.Return
              { answerId = qaWord answerId,
                releaseParamCaps,
                union' = R.Return'exception exn
              }
        )
        onSent
    Return'canceled ->
      sendPureMsg
        conn'
        ( R.Message'return
            R.Return
              { answerId = qaWord answerId,
                releaseParamCaps,
                union' = R.Return'canceled
              }
        )
        onSent
    Return'resultsSentElsewhere ->
      sendPureMsg
        conn'
        ( R.Message'return
            R.Return
              { answerId = qaWord answerId,
                releaseParamCaps,
                union' = R.Return'resultsSentElsewhere
              }
        )
        onSent
    Return'takeFromOtherQuestion (QAId qid) ->
      sendPureMsg
        conn'
        ( R.Message'return
            R.Return
              { answerId = qaWord answerId,
                releaseParamCaps,
                union' = R.Return'takeFromOtherQuestion qid
              }
        )
        onSent
    Return'acceptFromThirdParty ptr -> do
      msg <- createPure defaultLimit $ do
        mptr <- traverse thaw ptr
        msg <- case mptr of
          Just v -> pure $ UntypedRaw.message @UntypedRaw.Ptr v
          Nothing -> Message.newMessage Nothing
        ret <- new @R.Return () msg
        ret & encodeField #answerId (qaWord answerId)
        ret & encodeField #releaseParamCaps releaseParamCaps
        setVariant #acceptFromThirdParty ret (Raw @(Maybe B.AnyPointer) mptr)
        rpcMsg <- newRoot @R.Message () msg
        setVariant #return rpcMsg ret
        pure msg
      sendRawMsg conn' msg onSent

acceptReturn :: Conn -> Raw R.Return 'Const -> LimitT STM Return
acceptReturn conn ret = do
  let answerId = QAId (getField #answerId ret)
      releaseParamCaps = getField #releaseParamCaps ret
  which <- structWhich ret
  union' <- case which of
    R.RW_Return'results payload ->
      Return'results <$> acceptPayload conn payload
    R.RW_Return'exception exn ->
      Return'exception <$> parse exn
    R.RW_Return'canceled _ ->
      pure Return'canceled
    R.RW_Return'resultsSentElsewhere _ ->
      pure Return'resultsSentElsewhere
    R.RW_Return'takeFromOtherQuestion id ->
      Return'takeFromOtherQuestion . QAId <$> parse id
    R.RW_Return'acceptFromThirdParty (Raw ptr) ->
      pure $ Return'acceptFromThirdParty ptr
    R.RW_Return'unknown' ordinal ->
      lift $ throwSTM $ eFailed $ "Unknown return variant #" <> fromString (show ordinal)
  pure Return {answerId, releaseParamCaps, union'}

handleReturnMsg :: Conn -> Return -> STM ()
handleReturnMsg conn ret =
  getLive conn >>= \conn'@Conn' {questions} ->
    updateQAReturn conn' questions "question" ret

handleFinishMsg :: Conn -> R.Parsed R.Finish -> STM ()
handleFinishMsg conn finish =
  getLive conn >>= \conn'@Conn' {answers} ->
    updateQAFinish conn' answers "answer" finish

handleResolveMsg :: Conn -> R.Parsed R.Resolve -> STM ()
handleResolveMsg conn R.Resolve {promiseId, union'} =
  getLive conn >>= \conn'@Conn' {imports} -> do
    entry <- M.lookup (IEId promiseId) imports
    case entry of
      Nothing ->
        -- This can happen if we dropped the promise, but the release
        -- message is still in flight when the resolve message is sent.
        case union' of
          R.Resolve'cap R.CapDescriptor {union' = R.CapDescriptor'receiverHosted importId} -> do
            (_, onSent) <- newPromise
            -- Send a release message for the resolved cap, since
            -- we're not going to use it:
            sendPureMsg
              conn'
              ( R.Message'release
                  def
                    { R.id = importId,
                      R.referenceCount = 1
                    }
              )
              onSent
          -- Note [Level 3]: do we need to do something with
          -- thirdPartyHosted here?
          _ -> pure ()
      Just EntryI {promiseState = Nothing} ->
        -- This wasn't a promise! The remote vat has done something wrong;
        -- abort the connection.
        abortConn conn' $
          eFailed $
            mconcat
              [ "Received a resolve message for export id #",
                fromString (show promiseId),
                ", but that capability is not a promise!"
              ]
      Just EntryI {promiseState = Just (tvar, tmpDest)} ->
        case union' of
          R.Resolve'cap R.CapDescriptor {union' = cap} -> do
            client <- acceptCap conn cap
            resolveClientClient tmpDest (writeTVar tvar) client
          R.Resolve'exception exn ->
            resolveClientExn tmpDest (writeTVar tvar) exn
          R.Resolve'unknown' tag ->
            abortConn conn' $
              eUnimplemented $
                mconcat
                  [ "Resolve variant #",
                    fromString (show tag),
                    " not understood"
                  ]

handleReleaseMsg :: Conn -> R.Parsed R.Release -> STM ()
handleReleaseMsg
  conn
  R.Release
    { id = (IEId -> eid),
      referenceCount = refCountDiff
    } =
    releaseExport conn refCountDiff eid

releaseExport :: Conn -> Word32 -> IEId -> STM ()
releaseExport conn refCountDiff eid =
  getLive conn >>= \conn'@Conn' {exports} ->
    lookupAbort "export" conn' exports eid $
      \EntryE {client, refCount = oldRefCount} ->
        case compare oldRefCount refCountDiff of
          LT ->
            abortConn conn' $
              eFailed $
                "Received release for export with referenceCount "
                  <> "greater than our recorded total ref count."
          EQ ->
            dropConnExport conn client
          GT ->
            M.insert
              EntryE
                { client,
                  refCount = oldRefCount - refCountDiff
                }
              eid
              exports

handleDisembargoMsg :: Conn -> R.Parsed R.Disembargo -> STM ()
handleDisembargoMsg conn d = getLive conn >>= go d
  where
    go
      R.Disembargo
        { context =
            R.Disembargo'context'
              (R.Disembargo'context'receiverLoopback (EmbargoId -> eid))
        }
      conn'@Conn' {embargos} =
        do
          result <- M.lookup eid embargos
          case result of
            Nothing ->
              abortConn conn' $
                eFailed $
                  "No such embargo: " <> fromString (show $ embargoWord eid)
            Just fulfiller -> do
              queueSTM conn' (fulfill fulfiller ())
              M.delete eid embargos
              freeEmbargo conn' eid
    go
      R.Disembargo
        { target = R.MessageTarget target,
          context = R.Disembargo'context' (R.Disembargo'context'senderLoopback embargoId)
        }
      conn'@Conn' {exports, answers} =
        case target of
          R.MessageTarget'importedCap exportId ->
            lookupAbort "export" conn' exports (IEId exportId) $ \EntryE {client} ->
              disembargoPromise client
          R.MessageTarget'promisedAnswer R.PromisedAnswer {questionId, transform} ->
            lookupAbort "answer" conn' answers (QAId questionId) $ \case
              HaveReturn {returnMsg = Return {union' = Return'results Payload {content}}} ->
                transformClient transform content conn' >>= \case
                  (unwrapClient -> Just client') -> disembargoClient client'
                  (unwrapClient -> Nothing) -> abortDisembargo "targets a null capability"
              _ ->
                abortDisembargo $
                  "does not target an answer which has resolved to a value hosted by"
                    <> " the sender."
          R.MessageTarget'unknown' ordinal ->
            abortConn conn' $
              eUnimplemented $
                "Unknown MessageTarget ordinal #" <> fromString (show ordinal)
        where
          disembargoPromise PromiseClient {pState} =
            readTVar pState >>= \case
              Ready (unwrapClient -> Just client) ->
                disembargoClient client
              Ready (unwrapClient -> Nothing) ->
                abortDisembargo "targets a promise which resolved to null."
              _ ->
                abortDisembargo "targets a promise which has not resolved."
          disembargoPromise _ =
            abortDisembargo "targets something that is not a promise."

          disembargoClient (ImportClient cell) = do
            client <- Fin.readCell cell
            case client of
              ImportRef {conn = targetConn, importId}
                | conn == targetConn -> do
                    (_, onSent) <- newPromise
                    sendPureMsg
                      conn'
                      ( R.Message'disembargo
                          R.Disembargo
                            { context =
                                R.Disembargo'context' $
                                  R.Disembargo'context'receiverLoopback embargoId,
                              target =
                                R.MessageTarget $
                                  R.MessageTarget'importedCap (ieWord importId)
                            }
                      )
                      onSent
              _ ->
                abortDisembargoClient
          disembargoClient _ = abortDisembargoClient

          abortDisembargoClient =
            abortDisembargo $
              "targets a promise which has not resolved to a capability"
                <> " hosted by the sender."

          abortDisembargo info =
            abortConn conn' $
              eFailed $
                mconcat
                  [ "Disembargo #",
                    fromString (show embargoId),
                    " with context = senderLoopback ",
                    info
                  ]
    -- Note [Level 3]
    go d conn' = do
      (_, onSent) <- newPromise
      sendPureMsg
        conn'
        (R.Message'unimplemented $ R.Message $ R.Message'disembargo d)
        onSent

lookupAbort ::
  (Eq k, Hashable k, Show k) =>
  Text ->
  Conn' ->
  M.Map k v ->
  k ->
  (v -> STM a) ->
  STM a
lookupAbort keyTypeName conn m key f = do
  result <- M.lookup key m
  case result of
    Just val ->
      f val
    Nothing ->
      abortConn conn $
        eFailed $
          mconcat
            [ "No such ",
              keyTypeName,
              ": ",
              fromString (show key)
            ]

-- | @'insertNewAbort' keyTypeName conn key value stmMap@ inserts a key into a
-- map, aborting the connection if it is already present. @keyTypeName@ will be
-- used in the error message sent to the remote vat.
insertNewAbort :: (Eq k, Hashable k) => Text -> Conn' -> k -> v -> M.Map k v -> STM ()
insertNewAbort keyTypeName conn key value =
  M.focus
    ( Focus.alterM $ \case
        Just _ ->
          abortConn conn $
            eFailed $
              "duplicate entry in " <> keyTypeName <> " table."
        Nothing ->
          pure (Just value)
    )
    key

-- | Generate a cap table describing the capabilities reachable from the given
-- pointer. The capability table will be correct for any message where all of
-- the capabilities are within the subtree under the pointer.
genSendableCapTableRaw ::
  Conn ->
  Maybe (UntypedRaw.Ptr 'Const) ->
  STM [R.Parsed R.CapDescriptor]
genSendableCapTableRaw _ Nothing = pure []
genSendableCapTableRaw conn (Just ptr) =
  traverse
    ( \c -> do
        union' <- emitCap conn c
        pure (def :: R.Parsed R.CapDescriptor) {R.union' = union'}
    )
    (V.toList $ Message.getCapTable (UntypedRaw.message @UntypedRaw.Ptr ptr))

-- | Convert the pointer into a Payload, including a capability table for
-- the clients in the pointer's cap table.
makeOutgoingPayload :: Conn -> RawMPtr -> STM Payload
makeOutgoingPayload conn content = do
  capTable <- genSendableCapTableRaw conn content
  pure Payload {content, capTable}

sendPureMsg :: Conn' -> R.Parsed (Which R.Message) -> Fulfiller () -> STM ()
sendPureMsg Conn' {sendQ} msg onSent = do
  msg <- createPure maxBound (parsedToMsg (R.Message msg))
  writeTChan sendQ (msg, onSent)

-- | Send a finish message, updating connection state and triggering
-- callbacks as necessary.
finishQuestion :: Conn' -> R.Parsed R.Finish -> STM ()
finishQuestion conn@Conn' {questions} finish@R.Finish {questionId} = do
  -- arrange for the question ID to be returned to the pool once
  -- the return has also been received:
  subscribeReturn "question" conn questions (QAId questionId) $ \_ ->
    freeQuestion conn (QAId questionId)
  (_, onSent) <- newPromise
  sendPureMsg conn (R.Message'finish finish) onSent
  updateQAFinish conn questions "question" finish

-- | Send a return message, update the corresponding entry in our
-- answers table, and queue any registered callbacks. Calls 'error'
-- if the answerId is not in the table, or if we've already sent a
-- return for this answer.
returnAnswer :: Conn' -> Return -> STM ()
returnAnswer conn@Conn' {answers} ret = do
  sendReturn conn ret
  updateQAReturn conn answers "answer" ret

-- TODO(cleanup): updateQAReturn/Finish have a lot in common; can we refactor?

updateQAReturn :: Conn' -> M.Map QAId EntryQA -> Text -> Return -> STM ()
updateQAReturn conn table tableName ret@Return {answerId} =
  lookupAbort tableName conn table answerId $ \case
    NewQA {onFinish, onReturn} -> do
      M.insert
        HaveReturn
          { returnMsg = ret,
            onFinish
          }
        answerId
        table
      traverse_ ($ ret) onReturn
    HaveFinish {onReturn} -> do
      M.delete answerId table
      traverse_ ($ ret) onReturn
    HaveReturn {} ->
      abortConn conn $
        eFailed $
          "Duplicate return message for "
            <> tableName
            <> " #"
            <> fromString (show answerId)

updateQAFinish :: Conn' -> M.Map QAId EntryQA -> Text -> R.Parsed R.Finish -> STM ()
updateQAFinish conn table tableName finish@R.Finish {questionId} =
  lookupAbort tableName conn table (QAId questionId) $ \case
    NewQA {onFinish, onReturn} -> do
      traverse_ ($ finish) onFinish
      M.insert
        HaveFinish
          { finishMsg = finish,
            onReturn
          }
        (QAId questionId)
        table
    HaveReturn {onFinish} -> do
      traverse_ ($ finish) onFinish
      M.delete (QAId questionId) table
    HaveFinish {} ->
      abortConn conn $
        eFailed $
          "Duplicate finish message for "
            <> tableName
            <> " #"
            <> fromString (show questionId)

-- | Update an entry in the questions or answers table to queue the given
-- callback when the return message for that answer comes in. If the return
-- has already arrived, the callback is queued immediately.
--
-- If the entry already has other callbacks registered, this callback is
-- run *after* the others (see Note [callbacks]). Note that this is an
-- important property, as it is necessary to preserve E-order if the
-- callbacks are successive method calls on the returned object.
subscribeReturn :: Text -> Conn' -> M.Map QAId EntryQA -> QAId -> (Return -> STM ()) -> STM ()
subscribeReturn tableName conn table qaId onRet =
  lookupAbort tableName conn table qaId $ \qa -> do
    new <- go qa
    M.insert new qaId table
  where
    go = \case
      NewQA {onFinish, onReturn} ->
        pure
          NewQA
            { onFinish,
              onReturn = SnocList.snoc onReturn onRet
            }
      HaveFinish {finishMsg, onReturn} ->
        pure
          HaveFinish
            { finishMsg,
              onReturn = SnocList.snoc onReturn onRet
            }
      val@HaveReturn {returnMsg} -> do
        onRet returnMsg
        pure val

-- | Abort the connection, sending an abort message. This is only safe to call
-- from within either the thread running the receieve loop or the callback loop.
abortConn :: Conn' -> R.Parsed R.Exception -> STM a
abortConn _ e = throwSTM (SentAbort e)

-- | Gets the live connection state, or throws disconnected if it is not live.
getLive :: Conn -> STM Conn'
getLive Conn {liveState} =
  readTVar liveState >>= \case
    Live conn' -> pure conn'
    Dead -> throwSTM eDisconnected

-- | Performs an action with the live connection state. Does nothing if the
-- connection is dead.
whenLive :: Conn -> (Conn' -> STM ()) -> STM ()
whenLive Conn {liveState} f =
  readTVar liveState >>= \case
    Live conn' -> f conn'
    Dead -> pure ()

-- | Request the remote vat's bootstrap interface.
requestBootstrap :: MonadSTM m => Conn -> m Client
requestBootstrap conn@Conn {liveState} =
  liftSTM $
    readTVar liveState >>= \case
      Dead ->
        pure nullClient
      Live conn'@Conn' {questions} -> do
        qid <- newQuestion conn'
        let tmpDest =
              RemoteDest
                AnswerDest
                  { conn,
                    answer =
                      PromisedAnswer
                        { answerId = qid,
                          transform = SnocList.empty
                        }
                  }
        pState <- newTVar Pending {tmpDest}

        -- Arguably, we should wait for this promise, since it's analagous
        -- to a call in terms of operation, but we only send one of these
        -- per connection, so whatever.
        (_, onSent) <- newPromise
        sendPureMsg
          conn'
          (R.Message'bootstrap (def {R.questionId = qaWord qid} :: R.Parsed R.Bootstrap))
          onSent

        M.insert
          NewQA
            { onReturn =
                SnocList.fromList
                  [ resolveClientReturn tmpDest (writeTVar pState) conn' [],
                    \_ ->
                      finishQuestion
                        conn'
                        R.Finish
                          { questionId = qaWord qid,
                            releaseResultCaps = False
                          }
                  ],
              onFinish = SnocList.empty
            }
          qid
          questions
        exportMap <- ExportMap <$> M.new
        pure $
          wrapClient $
            Just
              PromiseClient
                { pState,
                  exportMap,
                  origTarget = tmpDest
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
resolveClientExn :: TmpDest -> (PromiseState -> STM ()) -> R.Parsed R.Exception -> STM ()
resolveClientExn tmpDest resolve exn = do
  case tmpDest of
    LocalDest LocalBuffer {callBuffer} -> do
      calls <- flushTQueue callBuffer
      traverse_
        ( \Server.CallInfo {response} ->
            breakPromise response exn
        )
        calls
    RemoteDest AnswerDest {} ->
      pure ()
    RemoteDest (ImportDest _) ->
      pure ()
  resolve $ Error exn

-- Resolve a promised client to a pointer. If it is a non-null non-capability
-- pointer, it resolves to an exception. See Note [resolveClient]
resolveClientPtr :: TmpDest -> (PromiseState -> STM ()) -> RawMPtr -> STM ()
resolveClientPtr tmpDest resolve ptr = case ptr of
  Nothing ->
    resolveClientClient tmpDest resolve nullClient
  Just (UntypedRaw.PtrCap c) -> do
    c' <- evalLimitT defaultLimit $ UntypedRaw.getClient c
    resolveClientClient tmpDest resolve c'
  Just _ ->
    resolveClientExn tmpDest resolve $
      eFailed "Promise resolved to non-capability pointer"

-- | Resolve a promised client to another client. See Note [resolveClient]
resolveClientClient :: TmpDest -> (PromiseState -> STM ()) -> Client -> STM ()
resolveClientClient tmpDest resolve (unwrapClient -> client) =
  case (client, tmpDest) of
    -- Remote resolved to local; we need to embargo:
    (Just LocalClient {}, RemoteDest dest) ->
      disembargoAndResolve dest
    (Just PromiseClient {origTarget = LocalDest _}, RemoteDest dest) ->
      disembargoAndResolve dest
    (Nothing, RemoteDest _) ->
      -- If it resolves to a null client, then we can't send a disembargo.
      -- Note that this may result in futrther calls throwing exceptions
      -- /before/ the outstanding calls, which is a bit weird. But all
      -- calls will throw at some point, so it's probably fine.
      resolveNow
    -- Local promises never need embargos; we can just forward:
    (_, LocalDest LocalBuffer {callBuffer}) ->
      flushAndResolve callBuffer
    -- These cases are slightly subtle; despite resolving to a
    -- client that points at a "remote" target, if it points into a
    -- _different_ connection, we must be proxying it, so we treat
    -- it as local and do a disembargo like above. We may need to
    -- change this when we implement level 3, since third-party
    -- handoff is a possibility; see Note [Level 3].
    --
    -- If it's pointing into the same connection, we don't need to
    -- do a disembargo.
    (Just PromiseClient {origTarget = RemoteDest newDest}, RemoteDest oldDest) -> do
      newConn <- destConn newDest
      oldConn <- destConn oldDest
      if newConn == oldConn
        then resolveNow
        else disembargoAndResolve oldDest
    (Just (ImportClient cell), RemoteDest oldDest) -> do
      ImportRef {conn = newConn} <- Fin.readCell cell
      oldConn <- destConn oldDest
      if newConn == oldConn
        then resolveNow
        else disembargoAndResolve oldDest
  where
    destConn AnswerDest {conn} = pure conn
    destConn (ImportDest cell) = do
      ImportRef {conn} <- Fin.readCell cell
      pure conn
    destTarget AnswerDest {answer} = pure $ AnswerTgt answer
    destTarget (ImportDest cell) = do
      ImportRef {importId} <- Fin.readCell cell
      pure $ ImportTgt importId

    resolveNow = do
      resolve $ Ready (wrapClient client)

    -- Flush the call buffer into the client's queue, and then pass the client
    -- to resolve.
    flushAndResolve callBuffer = do
      flushTQueue callBuffer >>= traverse_ (`call` wrapClient client)
      resolve $ Ready (wrapClient client)
    flushAndRaise callBuffer e =
      flushTQueue callBuffer
        >>= traverse_ (\Server.CallInfo {response} -> breakPromise response e)
    disembargoAndResolve dest = do
      Conn {liveState} <- destConn dest
      readTVar liveState >>= \case
        Live conn' -> do
          callBuffer <- newTQueue
          target <- destTarget dest
          disembargo conn' target $ \case
            Right () ->
              flushAndResolve callBuffer
            Left e ->
              flushAndRaise callBuffer e
          resolve $ Embargo {callBuffer}
        Dead ->
          resolveClientExn tmpDest resolve eDisconnected

-- | Send a (senderLoopback) disembargo to the given message target, and
-- register the transaction to run when the corresponding receiverLoopback
-- message is received.
--
-- The callback may be handed a 'Left' with a disconnected exception if
-- the connection is dropped before the disembargo is echoed.
disembargo :: Conn' -> MsgTarget -> (Either (R.Parsed R.Exception) () -> STM ()) -> STM ()
disembargo conn@Conn' {embargos} tgt onEcho = do
  callback <- newCallback onEcho
  eid <- newEmbargo conn
  M.insert callback eid embargos
  (_, onSent) <- newPromise
  sendPureMsg
    conn
    ( R.Message'disembargo
        R.Disembargo
          { target = marshalMsgTarget tgt,
            context =
              R.Disembargo'context' $
                R.Disembargo'context'senderLoopback (embargoWord eid)
          }
    )
    onSent

-- | Resolve a promised client to the result of a return. See Note [resolveClient]
--
-- The [Word16] is a list of pointer indexes to follow from the result.
resolveClientReturn :: TmpDest -> (PromiseState -> STM ()) -> Conn' -> [Word16] -> Return -> STM ()
resolveClientReturn tmpDest resolve conn@Conn' {answers} transform Return {union'} = case union' of
  -- TODO(cleanup) there is a lot of redundency betwen this and cbCallReturn; can
  -- we refactor?
  Return'exception exn ->
    resolveClientExn tmpDest resolve exn
  Return'results Payload {content} -> do
    res <- try $ evalLimitT defaultLimit $ followPtrs transform content
    case res of
      Right v ->
        resolveClientPtr tmpDest resolve v
      Left e ->
        resolveClientExn tmpDest resolve e
  Return'canceled ->
    resolveClientExn tmpDest resolve $ eFailed "Canceled"
  Return'resultsSentElsewhere ->
    -- Should never happen; we don't set sendResultsTo to anything other than
    -- caller.
    abortConn conn $
      eFailed $
        mconcat
          [ "Received Return.resultsSentElsewhere for a call ",
            "with sendResultsTo = caller."
          ]
  Return'takeFromOtherQuestion qid ->
    subscribeReturn "answer" conn answers qid $
      resolveClientReturn tmpDest resolve conn transform
  Return'acceptFromThirdParty _ ->
    -- Note [Level 3]
    abortConn conn $
      eUnimplemented
        "This vat does not support level 3."

-- | Get the client's export ID for this connection, or allocate a new one if needed.
-- If this is the first time this client has been exported on this connection,
-- bump the refcount.
getConnExport :: Conn -> Client' -> STM IEId
getConnExport conn client =
  getLive conn >>= \conn'@Conn' {exports} -> do
    ExportMap m <- clientExportMap client
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
  ExportMap eMap <- clientExportMap client'
  val <- M.lookup conn eMap
  case val of
    Just eid -> do
      M.delete conn eMap
      whenLive conn $ \conn'@Conn' {exports} -> do
        M.delete eid exports
        freeExport conn' eid
    Nothing ->
      error "BUG: tried to drop an export that doesn't exist."

clientExportMap :: Client' -> STM ExportMap
clientExportMap LocalClient {exportMap} = pure exportMap
clientExportMap PromiseClient {exportMap} = pure exportMap
clientExportMap (ImportClient cell) = do
  ImportRef {proxies} <- Fin.readCell cell
  pure proxies

-- | insert the client into the exports table, bumping the refcount if it is
-- already there. If a different client is already in the table at the same
-- id, call 'error'.
addBumpExport :: IEId -> Client' -> M.Map IEId EntryE -> STM ()
addBumpExport exportId client =
  M.focus (Focus.alter go) exportId
  where
    go Nothing = Just EntryE {client, refCount = 1}
    go (Just EntryE {client = oldClient, refCount})
      | client /= oldClient =
          error $
            "BUG: addExportRef called with a client that is different "
              ++ "from what is already in our exports table."
      | otherwise =
          Just EntryE {client, refCount = refCount + 1}

-- | Generate a CapDescriptor', which we can send to the connection's remote
-- vat to identify client. In the process, this may allocate export ids, update
-- reference counts, and so forth.
emitCap :: Conn -> Client -> STM (R.Parsed (Which R.CapDescriptor))
emitCap _targetConn (unwrapClient -> Nothing) =
  pure R.CapDescriptor'none
emitCap targetConn (unwrapClient -> Just client') = case client' of
  LocalClient {} ->
    R.CapDescriptor'senderHosted . ieWord <$> getConnExport targetConn client'
  PromiseClient {pState} ->
    readTVar pState >>= \case
      Pending {tmpDest = RemoteDest AnswerDest {conn, answer}}
        | conn == targetConn ->
            pure $ R.CapDescriptor'receiverAnswer (marshalPromisedAnswer answer)
      Pending {tmpDest = RemoteDest (ImportDest cell)} -> do
        ImportRef {conn, importId = IEId iid} <- Fin.readCell cell
        if conn == targetConn
          then pure (R.CapDescriptor'receiverHosted iid)
          else newSenderPromise
      _ ->
        newSenderPromise
  ImportClient cell -> do
    ImportRef {conn = hostConn, importId} <- Fin.readCell cell
    if hostConn == targetConn
      then pure (R.CapDescriptor'receiverHosted (ieWord importId))
      else R.CapDescriptor'senderHosted . ieWord <$> getConnExport targetConn client'
  where
    newSenderPromise = R.CapDescriptor'senderPromise . ieWord <$> getConnExport targetConn client'

acceptPayload :: Conn -> Raw R.Payload 'Const -> LimitT STM Payload
acceptPayload conn payload = do
  capTable <- parseField #capTable payload
  clients <- lift $ V.fromList <$> traverse (\R.CapDescriptor {union'} -> acceptCap conn union') capTable
  Raw rawContent <- readField #content payload
  content <- traverse (UntypedRaw.tMsg (pure . Message.withCapTable clients)) rawContent
  pure Payload {content, capTable}

-- | 'acceptCap' is a dual of 'emitCap'; it derives a Client from a CapDescriptor'
-- received via the connection. May update connection state as necessary.
acceptCap :: Conn -> R.Parsed (Which R.CapDescriptor) -> STM Client
acceptCap conn cap = getLive conn >>= \conn' -> go conn' cap
  where
    go _ R.CapDescriptor'none = pure (wrapClient Nothing)
    go conn'@Conn' {imports} (R.CapDescriptor'senderHosted (IEId -> importId)) = do
      entry <- M.lookup importId imports
      case entry of
        Just EntryI {promiseState = Just _} ->
          let imp = fromString (show importId)
           in abortConn conn' $
                eFailed $
                  "received senderHosted capability #"
                    <> imp
                    <> ", but the imports table says #"
                    <> imp
                    <> " is senderPromise."
        Just ent@EntryI {localRc, remoteRc, proxies} -> do
          Rc.incr localRc
          M.insert ent {localRc, remoteRc = remoteRc + 1} importId imports
          cell <-
            Fin.newCell
              ImportRef
                { conn,
                  importId,
                  proxies
                }
          queueIO conn' $ Fin.addFinalizer cell $ atomically $ Rc.decr localRc
          pure $ wrapClient $ Just $ ImportClient cell
        Nothing ->
          wrapClient . Just . ImportClient <$> newImport importId conn Nothing
    go conn'@Conn' {imports} (R.CapDescriptor'senderPromise (IEId -> importId)) = do
      entry <- M.lookup importId imports
      case entry of
        Just EntryI {promiseState = Nothing} ->
          let imp = fromString (show importId)
           in abortConn conn' $
                eFailed $
                  "received senderPromise capability #"
                    <> imp
                    <> ", but the imports table says #"
                    <> imp
                    <> " is senderHosted."
        Just ent@EntryI {remoteRc, proxies, promiseState = Just (pState, origTarget)} -> do
          M.insert ent {remoteRc = remoteRc + 1} importId imports
          pure $
            wrapClient $
              Just
                PromiseClient
                  { pState,
                    exportMap = proxies,
                    origTarget
                  }
        Nothing -> do
          rec imp <- newImport importId conn (Just (pState, tmpDest))
              ImportRef {proxies} <- Fin.readCell imp
              let tmpDest = RemoteDest (ImportDest imp)
              pState <- newTVar Pending {tmpDest}
          pure $
            wrapClient $
              Just
                PromiseClient
                  { pState,
                    exportMap = proxies,
                    origTarget = tmpDest
                  }
    go conn'@Conn' {exports} (R.CapDescriptor'receiverHosted exportId) =
      lookupAbort "export" conn' exports (IEId exportId) $
        \EntryE {client} ->
          pure $ wrapClient $ Just client
    go conn' (R.CapDescriptor'receiverAnswer pa) = do
      pa <- unmarshalPromisedAnswer pa `catchSTM` abortConn conn'
      newLocalAnswerClient conn' pa
    go conn' (R.CapDescriptor'thirdPartyHosted _) =
      -- Note [Level 3]
      abortConn conn' $
        eUnimplemented
          "thirdPartyHosted unimplemented; level 3 is not supported."
    go conn' (R.CapDescriptor'unknown' tag) =
      abortConn conn' $
        eUnimplemented $
          "Unimplemented CapDescriptor variant #" <> fromString (show tag)

-- | Create a new entry in the imports table, with the given import id and
-- 'promiseState', and return a corresponding ImportRef. When the ImportRef is
-- garbage collected, the refcount in the table will be decremented.
newImport :: IEId -> Conn -> Maybe (TVar PromiseState, TmpDest) -> STM (Fin.Cell ImportRef)
newImport importId conn promiseState =
  getLive conn >>= \conn'@Conn' {imports} -> do
    localRc <- Rc.new () $ releaseImport importId conn'
    proxies <- ExportMap <$> M.new
    let importRef =
          ImportRef
            { conn,
              importId,
              proxies
            }
    M.insert
      EntryI
        { localRc,
          remoteRc = 1,
          proxies,
          promiseState
        }
      importId
      imports
    cell <- Fin.newCell importRef
    queueIO conn' $ Fin.addFinalizer cell $ atomically $ Rc.decr localRc
    pure cell

-- | Release the identified import. Removes it from the table and sends a release
-- message with the correct count.
releaseImport :: IEId -> Conn' -> STM ()
releaseImport importId conn'@Conn' {imports} = do
  (_, onSent) <- newPromise
  lookupAbort "imports" conn' imports importId $ \EntryI {remoteRc} ->
    sendPureMsg
      conn'
      ( R.Message'release
          R.Release
            { id = ieWord importId,
              referenceCount = remoteRc
            }
      )
      onSent
  M.delete importId imports

-- | Create a new client targeting an object in our answers table.
-- Important: in this case the 'PromisedAnswer' refers to a question we
-- have recevied, not sent.
newLocalAnswerClient :: Conn' -> PromisedAnswer -> STM Client
newLocalAnswerClient conn@Conn' {answers} PromisedAnswer {answerId, transform} = do
  callBuffer <- newTQueue
  let tmpDest = LocalDest $ LocalBuffer {callBuffer}
  pState <- newTVar Pending {tmpDest}
  subscribeReturn "answer" conn answers answerId $
    resolveClientReturn
      tmpDest
      (writeTVar pState)
      conn
      (toList transform)
  exportMap <- ExportMap <$> M.new
  pure $
    wrapClient $
      Just
        PromiseClient
          { pState,
            exportMap,
            origTarget = tmpDest
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
