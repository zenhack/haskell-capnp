{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Capnp.New.Rpc.Server
    ( CallHandler
    , MethodHandler
    , UntypedMethodHandler
    , Export(..)
    , export
    , findMethod

    -- * Helpers for writing method handlers
    , handleParsed
    , handleRaw
    , methodUnimplemented

    , toUntypedMethodHandler
    ) where

import           Capnp.Convert           (parsedToRaw)
import           Capnp.Message           (Mutability(..))
import qualified Capnp.New.Basics        as B
import qualified Capnp.New.Classes       as C
import qualified Capnp.Repr              as R
import           Capnp.Repr.Methods      (Client(..))
import           Capnp.Rpc.Errors        (eFailed, eMethodUnimplemented)
import           Capnp.Rpc.Promise
    (Fulfiller, breakPromise, fulfill, newCallback)
import qualified Capnp.Rpc.Server        as Legacy
import qualified Capnp.Rpc.Untyped       as URpc
import           Capnp.TraversalLimit    (defaultLimit, evalLimitT)
import qualified Capnp.Untyped           as U
import           Control.Exception.Safe  (withException)
import           Control.Monad.STM.Class (MonadSTM(..))
import           Data.Function           ((&))
import           Data.Kind               (Constraint)
import qualified Data.Map.Strict         as M
import           Data.Maybe              (fromMaybe)
import           Data.Proxy              (Proxy(..))
import qualified Data.Vector             as V
import           Data.Word
import           GHC.Prim                (coerce)
import           Internal.BuildPure      (createPure)
import           Supervisors             (Supervisor)

-- | A handler for arbitrary RPC calls. Maps (interfaceId, methodId) pairs to
-- 'UntypedMethodHandler's.
type CallHandler m = M.Map Word64 (V.Vector (UntypedMethodHandler m))

-- | Type alias for a handler for a particular rpc method.
type MethodHandler m p r
    = R.Raw 'Const p
    -> Fulfiller (R.Raw 'Const r)
    -> m ()

-- | Type alias for a handler for an untyped RPC method.
type UntypedMethodHandler m = MethodHandler m B.AnyStruct B.AnyStruct

-- | Generated interface types have instances of 'Export', which allows a server
-- for that interface to be exported as a 'Client'.
class (R.IsCap i, C.HasTypeId i) => Export i where
    -- | The constraint needed for a server to implement an interface;
    -- if @'Server' i s@ is satisfied, @s@ is a server for interface @i@.
    -- The code generator generates a type class for each interface, and
    -- this will aways be an alias for that type class.
    type Server i :: * -> Constraint

    -- | Convert the server to a 'CallHandler' populated with appropriate
    -- 'MethodHandler's for the interface.
    serverToCallHandler :: Server i s => Proxy i -> s -> CallHandler IO
    -- NB: the proxy helps disambiguate types; for some reason TypeApplications
    -- doesn't seem to be enough in the face of a type alias of kind 'Constraint'.
    -- the inconsistency is a bit ugly, but this method isn't intended to called
    -- by users directly, only by generated code, so it's less of a big deal.

-- | Export the server as a client for interface @i@. Spawns a server thread
-- with its lifetime bound to the supervisor.
export :: forall i s m. (MonadSTM m, Export i, Server i s) => Supervisor -> s -> m (Client i)
export sup srv =
    let h = serverToCallHandler (Proxy :: Proxy i) srv in
    liftSTM $ Client <$> URpc.export sup (toLegacyServerOps h)

-- | Look up a particlar 'MethodHandler' in the 'CallHandler'.
findMethod :: Word64 -> Word16 -> CallHandler m -> Maybe (UntypedMethodHandler m)
findMethod interfaceId methodId handler = do
    iface <- M.lookup interfaceId handler
    iface V.!? fromIntegral methodId

toLegacyCallHandler
    :: (Monad m, MonadSTM m)
    => CallHandler m
    -> Word64
    -> Word16
    -> Legacy.MethodHandler m (Maybe (U.Ptr 'Const)) (Maybe (U.Ptr 'Const))
toLegacyCallHandler callHandler interfaceId methodId =
    findMethod interfaceId methodId callHandler
    & fromMaybe methodUnimplemented
    & toLegacyMethodHandler


-- | Convert a typed method handler to an untyped one. Mostly intended for
-- use by generated code.
toUntypedMethodHandler
    :: forall p r m. (R.IsStruct p, R.IsStruct r)
    => MethodHandler m p r
    -> UntypedMethodHandler m
toUntypedMethodHandler = coerce

toLegacyMethodHandler :: (Monad m, MonadSTM m) => UntypedMethodHandler m -> Legacy.MethodHandler m (Maybe (U.Ptr 'Const)) (Maybe (U.Ptr 'Const))
toLegacyMethodHandler handler =
    Legacy.untypedHandler $ \args respond -> do
        respond' <- newCallback $ \case
            Left e ->
                breakPromise respond e
            Right (R.Raw s) ->
                fulfill respond (Just (U.PtrStruct s))
        case args of
            Just (U.PtrStruct argStruct) ->
                handler (R.Raw argStruct) respond'
            _ ->
                breakPromise respond $ eFailed "Argument was not a struct"

toLegacyServerOps :: (Monad m, MonadSTM m) => CallHandler m -> Legacy.ServerOps m
toLegacyServerOps callHandler = Legacy.ServerOps
    { handleStop = liftSTM $ pure ()
    , handleCast = Nothing
    , handleCall = toLegacyCallHandler callHandler
    }

-- Helpers for writing method handlers

-- | Handle a method, working with the parsed form of parameters and
-- results.
handleParsed ::
    ( C.Parse p pp, R.IsStruct p
    , C.Parse r rr, R.IsStruct r
    ) => (pp -> IO rr) -> MethodHandler IO p r
handleParsed handler =
    handleRaw $ \param -> do
        p <- evalLimitT defaultLimit $ C.parse param
        r <- handler p
        -- TODO: Figure out how to add an instance of Thaw for
        -- Raw so we can skip the (un)wrapping here.
        struct <- createPure maxBound $ R.fromRaw <$> parsedToRaw r
        pure (R.Raw struct)

-- | Handle a method, working with the raw (unparsed) form of
-- parameters and results.
handleRaw
    :: (R.IsStruct p, R.IsStruct r)
    => (R.Raw 'Const p -> IO (R.Raw 'Const r)) -> MethodHandler IO p r
handleRaw handler param f = do
    res <- handler param `withException` breakPromise f
    fulfill f res


-- | 'MethodHandler' that always throws unimplemented.
methodUnimplemented :: MonadSTM m => MethodHandler m p r
methodUnimplemented _ f = breakPromise f eMethodUnimplemented

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
