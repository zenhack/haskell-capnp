{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Capnp.New.Rpc.Server
    ( CallHandler
    , MethodHandler
    , UntypedMethodHandler
    , Export(..)
    , export
    , unimplemented
    , findMethod

    , toUntypedMethodHandler
    ) where

import           Capnp.Message           (Mutability(..))
import qualified Capnp.New.Basics        as B
import qualified Capnp.Repr              as R
import           Capnp.Repr.Methods      (Client(..))
import           Capnp.Rpc.Errors        (eFailed, eMethodUnimplemented)
import           Capnp.Rpc.Promise
    (Fulfiller, breakPromise, fulfill, newCallback)
import qualified Capnp.Rpc.Server        as Legacy
import qualified Capnp.Rpc.Untyped       as URpc
import qualified Capnp.Untyped           as U
import           Control.Monad.STM.Class (MonadSTM(..))
import           Data.Function           ((&))
import           Data.Kind               (Constraint)
import qualified Data.Map.Strict         as M
import           Data.Maybe              (fromMaybe)
import           Data.Proxy              (Proxy(..))
import qualified Data.Vector             as V
import           Data.Word
import           GHC.Prim                (coerce)
import           Supervisors             (Supervisor)

-- | A handler for RPC calls. Maps (interfaceId, methodId) pairs to
-- 'MethodHandler's.
type CallHandler m = M.Map Word64 (V.Vector (UntypedMethodHandler m))

-- | Type alias for a handler for a particular rpc method.
type MethodHandler m p r
    = R.Raw 'Const p
    -> Fulfiller (R.Raw 'Const r)
    -> m ()

type UntypedMethodHandler m = MethodHandler m B.AnyStruct B.AnyStruct

-- | Generated interface types have instances of 'Export', which allows a server
-- for that interface to be exported as a 'Client'.
class R.IsCap i => Export i where
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

-- | Export the server as a client for interface @i@. Spawns a server thread
-- with its lifetime bound to the supervisor.
export :: forall i s m. (MonadSTM m, Export i, Server i s) => Supervisor -> s -> m (Client i)
export sup srv =
    let h = serverToCallHandler (Proxy :: Proxy i) srv in
    liftSTM $ Client <$> URpc.export sup (toLegacyServerOps h)

-- | 'MethodHandler' that always throws unimplemented.
unimplemented :: MonadSTM m => MethodHandler m p r
unimplemented _ f = breakPromise f eMethodUnimplemented

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
    & fromMaybe unimplemented
    & toLegacyMethodHandler


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
