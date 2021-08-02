{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Capnp.New.Rpc.Server
    ( CallHandler
    , MethodHandler
    , Export(..)
    , export
    , unimplemented
    , findMethod
    ) where

import           Capnp.Message           (Mutability(..))
import qualified Capnp.New.Basics        as B
import qualified Capnp.Repr              as R
import           Capnp.Repr.Methods      (Client(..))
import           Capnp.Rpc.Errors        (eMethodUnimplemented)
import           Capnp.Rpc.Promise       (Fulfiller, breakPromise)
import qualified Capnp.Rpc.Server        as Legacy
import qualified Capnp.Rpc.Untyped       as URpc
import qualified Capnp.Untyped           as U
import           Control.Monad.STM.Class (MonadSTM(..))
import           Data.Kind               (Constraint)
import qualified Data.Map.Strict         as M
import           Data.Proxy              (Proxy(..))
import qualified Data.Vector             as V
import           Data.Word
import           GHC.Prim                (coerce)
import           Supervisors             (Supervisor)

-- | A handler for RPC calls. Maps (interfaceId, methodId) pairs to
-- 'MethodHandler's.
type CallHandler m = M.Map Word64 (V.Vector (MethodHandler m))

-- | Type alias for a handler for a particular rpc method.
type MethodHandler m
    = R.Raw 'Const (Maybe B.AnyPointer)
    -> Fulfiller (R.Raw 'Const (Maybe B.AnyPointer))
    -> m ()

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
unimplemented :: MonadSTM m => MethodHandler m
unimplemented _ f = breakPromise f eMethodUnimplemented

-- | Look up a particlar 'MethodHandler' in the 'CallHandler'.
findMethod :: Word64 -> Word16 -> CallHandler m -> Maybe (MethodHandler m)
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
    toLegacyMethodHandler $ case findMethod interfaceId methodId callHandler of
        Nothing            -> unimplemented
        Just methodHandler -> methodHandler

toLegacyMethodHandler :: MethodHandler m -> Legacy.MethodHandler m (Maybe (U.Ptr 'Const)) (Maybe (U.Ptr 'Const))
toLegacyMethodHandler handler =
    Legacy.untypedHandler $ \args respond ->
        handler (coerce args) (coerce respond)

toLegacyServerOps :: (Monad m, MonadSTM m) => CallHandler m -> Legacy.ServerOps m
toLegacyServerOps callHandler = Legacy.ServerOps
    { handleStop = liftSTM $ pure ()
    , handleCast = Nothing
    , handleCall = toLegacyCallHandler callHandler
    }
