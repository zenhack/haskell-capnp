{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Capnp.Rpc.Server
  ( CallHandler,
    MethodHandler,
    UntypedMethodHandler,
    CallInfo (..),
    ServerOps (..),
    Export (..),
    exportToServerOps,
    findMethod,
    SomeServer (..),
    runServer,
    castHandler,

    -- * Helpers for writing method handlers
    handleParsed,
    handleRaw,
    methodUnimplemented,
    toUntypedMethodHandler,

    -- * Internals; exposed only for use by generated code.
    MethodHandlerTree (..),
  )
where

import qualified Capnp.Basics as B
import qualified Capnp.Classes as C
import Capnp.Convert (parsedToRaw)
import Capnp.Message (Mutability (..))
import qualified Capnp.Repr as R
-- import Capnp.Repr.Methods (Client (..))
import Capnp.Rpc.Errors
  ( eFailed,
    eMethodUnimplemented,
    wrapException,
  )
import Capnp.Rpc.Promise
  ( Fulfiller,
    breakPromise,
    fulfill,
  )
import Capnp.TraversalLimit (defaultLimit, evalLimitT)
import qualified Capnp.Untyped as U
import Control.Concurrent.STM (atomically)
import Control.Exception.Safe (withException)
import Data.Function ((&))
import Data.Functor.Contravariant (contramap)
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Data.Word
import GHC.Prim (coerce)
import Internal.BuildPure (createPure)
import qualified Internal.TCloseQ as TCloseQ

-- | A 'CallInfo' contains information about a method call.
data CallInfo = CallInfo
  { -- | The id of the interface whose method is being called.
    interfaceId :: !Word64,
    -- | The method id of the method being called.
    methodId :: !Word16,
    -- | The arguments to the method call.
    arguments :: Maybe (U.Ptr 'Const),
    -- | A 'Fulfiller' which accepts the method's return value.
    response :: Fulfiller (Maybe (U.Ptr 'Const))
  }

-- | The operations necessary to receive and handle method calls, i.e.
-- to implement an object.
data ServerOps = ServerOps
  { -- | Handle a method call; takes the interface and method id and returns
    -- a handler for the specific method.
    handleCall :: Word64 -> Word16 -> UntypedMethodHandler,
    -- | Handle shutting-down the receiver; this is called when the last
    -- reference to the capability is dropped.
    handleStop :: IO (),
    -- | used to unwrap the server when reflecting on a local client.
    handleCast :: forall a. Typeable a => Maybe a
  }

-- | A handler for arbitrary RPC calls. Maps (interfaceId, methodId) pairs to
-- 'UntypedMethodHandler's.
type CallHandler = M.Map Word64 (V.Vector UntypedMethodHandler)

-- | Type alias for a handler for a particular rpc method.
type MethodHandler p r =
  R.Raw p 'Const ->
  Fulfiller (R.Raw r 'Const) ->
  IO ()

castHandler ::
  forall p q r s.
  (R.ReprFor p ~ R.ReprFor q, R.ReprFor r ~ R.ReprFor s) =>
  MethodHandler p r ->
  MethodHandler q s
castHandler = coerce

-- | Type alias for a handler for an untyped RPC method.
type UntypedMethodHandler = MethodHandler (Maybe B.AnyPointer) (Maybe B.AnyPointer)

-- | Base class for things that can act as capnproto servers.
class SomeServer a where
  -- | Called when the last live reference to a server is dropped.
  shutdown :: a -> IO ()
  shutdown _ = pure ()

  -- | Try to extract a value of a given type. The default implementation
  -- always fails (returns 'Nothing'). If an instance chooses to implement
  -- this, it will be possible to use "reflection" on clients that point
  -- at local servers to dynamically unwrap the server value. A typical
  -- implementation will just call Typeable's @cast@ method, but this
  -- needn't be the case -- a server may wish to allow local peers to
  -- unwrap some value that is not exactly the data the server has access
  -- to.
  unwrap :: Typeable b => a -> Maybe b
  unwrap _ = Nothing

-- | Generated interface types have instances of 'Export', which allows a server
-- for that interface to be exported as a 'Client'.
class (R.IsCap i, C.HasTypeId i) => Export i where
  -- | The constraint needed for a server to implement an interface;
  -- if @'Server' i s@ is satisfied, @s@ is a server for interface @i@.
  -- The code generator generates a type class for each interface, and
  -- this will aways be an alias for that type class.
  type Server i :: Type -> Constraint

  -- | Convert the server to a 'MethodHandlerTree' populated with appropriate
  -- 'MethodHandler's for the interface. This is really only exported for use
  -- by generated code; users of the library will generally prefer to use
  -- 'export'.
  methodHandlerTree :: Server i s => Proxy i -> s -> MethodHandlerTree

-- NB: the proxy helps disambiguate types; for some reason TypeApplications
-- doesn't seem to be enough in the face of a type alias of kind 'Constraint'.
-- the inconsistency is a bit ugly, but this method isn't intended to called
-- by users directly, only by generated code and our helper in this module,
-- so it's less of a big deal.

-- | Lazily computed tree of the method handlers exposed by an interface. Only
-- of interest to generated code.
data MethodHandlerTree = MethodHandlerTree
  { -- | type id for the primary interface
    mhtId :: Word64,
    -- | method handlers for methods of the primary interface.
    mhtHandlers :: [UntypedMethodHandler],
    -- | Trees for parent interfaces. In the case of diamond dependencies,
    -- there may be duplicates, which are eliminated by 'mhtToCallHandler'.
    mhtParents :: [MethodHandlerTree]
  }

mhtToCallHandler :: MethodHandlerTree -> CallHandler
mhtToCallHandler = go M.empty . pure
  where
    go accum [] = accum
    go accum (t : ts)
      | mhtId t `M.member` accum = go accum ts -- dedup diamond dependencies
      | otherwise =
          go (M.insert (mhtId t) (V.fromList (mhtHandlers t)) accum) (mhtParents t ++ ts)

{-
-- | Export the server as a client for interface @i@. Spawns a server thread
-- with its lifetime bound to the supervisor.
export :: forall i s m. (MonadSTM m, Export i, Server i s, SomeServer s) => Supervisor -> s -> m (Client i)
export sup srv =
  let h = mhtToCallHandler (methodHandlerTree (Proxy @i) srv)
   in liftSTM $ Client <$> URpc.export sup (toLegacyServerOps srv h)
-}

-- | Look up a particlar 'MethodHandler' in the 'CallHandler'.
findMethod :: Word64 -> Word16 -> CallHandler -> Maybe UntypedMethodHandler
findMethod interfaceId methodId handler = do
  iface <- M.lookup interfaceId handler
  iface V.!? fromIntegral methodId

-- | Convert a typed method handler to an untyped one. Mostly intended for
-- use by generated code.
toUntypedMethodHandler ::
  forall p r.
  (R.IsStruct p, R.IsStruct r) =>
  MethodHandler p r ->
  UntypedMethodHandler
toUntypedMethodHandler h =
  \case
    R.Raw (Just (U.PtrStruct param)) -> \ret ->
      h
        (R.Raw param)
        ( contramap
            (\(R.Raw s) -> R.Raw $ Just $ U.PtrStruct s)
            ret
        )
    _ ->
      \ret -> breakPromise ret (eFailed "Parameter was not a struct")

someServerToServerOps :: SomeServer a => a -> CallHandler -> ServerOps
someServerToServerOps srv callHandler =
  ServerOps
    { handleStop = shutdown srv,
      handleCast = unwrap srv,
      handleCall = \interfaceId methodId ->
        findMethod interfaceId methodId callHandler
          & fromMaybe methodUnimplemented
    }

exportToServerOps :: forall i s. (Export i, Server i s, SomeServer s) => Proxy i -> s -> ServerOps
exportToServerOps proxy srv =
  mhtToCallHandler (methodHandlerTree proxy srv)
    & someServerToServerOps srv

-- Helpers for writing method handlers

-- | Handle a method, working with the parsed form of parameters and
-- results.
handleParsed ::
  ( C.Parse p pp,
    R.IsStruct p,
    C.Parse r pr,
    R.IsStruct r
  ) =>
  (pp -> IO pr) ->
  MethodHandler p r
handleParsed handler param = propagateExceptions $ \f -> do
  p <- evalLimitT defaultLimit $ C.parse param
  r <- handler p
  -- TODO: Figure out how to add an instance of Thaw for
  -- Raw so we can skip the (un)wrapping here.
  struct <- createPure maxBound $ R.fromRaw <$> parsedToRaw r
  fulfill f (R.Raw struct)

-- | Handle a method, working with the raw (unparsed) form of
-- parameters and results.
handleRaw ::
  (R.IsStruct p, R.IsStruct r) =>
  (R.Raw p 'Const -> IO (R.Raw r 'Const)) ->
  MethodHandler p r
handleRaw handler param = propagateExceptions $ \f ->
  handler param >>= fulfill f

-- Helper for handle*; breaks the promise if the handler throws.
propagateExceptions :: (Fulfiller a -> IO b) -> Fulfiller a -> IO b
propagateExceptions h f =
  h f `withException` (breakPromise f . wrapException False)

-- | 'MethodHandler' that always throws unimplemented.
methodUnimplemented :: MethodHandler p r
methodUnimplemented _ f = breakPromise f eMethodUnimplemented

-- | Handle incoming messages for a given object.
--
-- Accepts a queue of messages to handle, and 'ServerOps' used to handle them.
-- returns when it receives a 'Stop' message.
runServer :: TCloseQ.Q CallInfo -> ServerOps -> IO ()
runServer q ops = go
  where
    go =
      atomically (TCloseQ.read q) >>= \case
        Nothing ->
          pure ()
        Just CallInfo {interfaceId, methodId, arguments, response} ->
          do
            handleCall ops interfaceId methodId (R.Raw arguments) (coerce response)
            go

{-
Sketch of future Async API, might take a bit of internals work to make
this possible:

-- | Handle a method call asynchronously.
--
-- When invoked, the handleer will be run synchronously, blocking further
-- method calls until the 'IO' returns. The method call does not return
-- until the 'Async' resolves, but further method calls can be serviced in
-- the meantime.
--
-- If a Finish message is received before the Async resolves, it will be
-- 'cancel'ed.
handleRawAsync
    :: (R.IsStruct p, R.IsStruct r)
    => (R.Raw 'Const p -> IO (Async (R.Raw 'Const r)))
    -> MethodHandler IO p r

-- | Like 'handleRawAsync', but accepts and returns parsed values.
handleParsedAsync  ::
    ( C.Parse p pp, R.IsStruct p
    , C.Parse r, rr, R.IsStruct r
    )
    => (pp -> IO (Async rr))
    -> MethodHandler IO p r
-}
