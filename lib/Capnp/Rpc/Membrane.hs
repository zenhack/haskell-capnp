{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Module: Capnp.Rpc.Membrane
-- Descritpion: Helpers for working with membranes.
--
-- Membranes are common in object-capability design. Think of it like a
-- proxy on steroids: a membrane inserts itself in front of another capability,
-- and can intercept and modify method calls. Unlike a simple proxy though,
-- the membrane will also be applied to any objects returned by method calls,
-- or passed in arguments, transitively, so it can sit in front of entire
-- object graphs.
module Capnp.Rpc.Membrane
  ( enclose,
    exclude,
    Policy,
    Action (..),
    Direction (..),
    Call (..),
  )
where

import qualified Capnp.Message as M
import Capnp.Mutability (Mutability (..))
import qualified Capnp.Repr as R
import Capnp.Rpc.Promise (breakOrFulfill, newCallback)
import qualified Capnp.Rpc.Server as Server
import qualified Capnp.Rpc.Untyped as URpc
import qualified Capnp.Untyped as U
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.STM.Class
import Data.Functor.Contravariant (contramap)
import Data.Typeable (Typeable, cast)
import Data.Word
import Supervisors (Supervisor)

-- | An action indicates what to do with an incoming method call.
data Action
  = -- | Handle the method using the provided method handler, instead of
    -- letting it through the membrane. Arguments and return values will not
    -- be wrapped/unwraped, so be careful when delegating to objects inside
    -- the membrane.
    Handle Server.UntypedMethodHandler
  | -- | Forward the method call on to its original destination, wrapping
    -- and unwrapping arguments & return values as normal.
    Forward

-- | A Direction indicates which direction a method call is traveling:
-- into or out of the membrane.
data Direction = In | Out
  deriving (Show, Read, Eq)

flipDir :: Direction -> Direction
flipDir In = Out
flipDir Out = In

-- | Alias for direction; somtimes it is convienent to think about capabilities
-- from the standpoint of which side they is _on_, rather than where it is going
-- as with methods.
type Side = Direction

-- | A 'Call' represents a method call that is crossing the membrane.
data Call = Call
  { -- | Which direction is the call going? if this is 'In', the call was made
    -- by something outside the membrane to something inside it. If it is 'Out',
    -- something inside the membrane is making a call to something outside the
    -- membrane.
    direction :: Direction,
    -- | The interface id of the method being called.
    interfaceId :: Word64,
    -- | The ordinal of the method being called.
    methodId :: Word16,
    -- | The target of the method call.
    target :: URpc.Client
  }

-- | A 'Policy' decides what to do when a call crosses the membrane.
type Policy = Call -> STM Action

-- | @'enclose' sup cap policy@ wraps @cap@ in a membrane whose behavior is
-- goverend by @policy@.
enclose :: (URpc.IsClient c, MonadSTM m) => Supervisor -> c -> Policy -> m c
enclose = newMembrane In

-- | 'exclude' is like 'enclose', except that the capability is treated as
-- being *outside* of a membrane that wraps the rest of the world.
exclude :: (URpc.IsClient c, MonadSTM m) => Supervisor -> c -> Policy -> m c
exclude = newMembrane Out

newMembrane :: (URpc.IsClient c, MonadSTM m) => Direction -> Supervisor -> c -> Policy -> m c
newMembrane dir sup toWrap policy = liftSTM $ do
  identity <- newTVar ()
  let mem = Membrane {policy, identity}
  URpc.fromClient <$> pass dir sup mem (URpc.toClient toWrap)

data MembraneWrapped = MembraneWrapped
  { client :: URpc.Client,
    membrane :: Membrane,
    side :: Direction
  }
  deriving (Typeable)

data Membrane = Membrane
  { policy :: Policy,
    -- | an object with identity, for comparison purposes:
    identity :: TVar ()
  }

instance Eq Membrane where
  x == y = identity x == identity y

wrapHandler :: Side -> Supervisor -> Membrane -> Server.UntypedMethodHandler -> Server.UntypedMethodHandler
wrapHandler receiverSide sup mem handler = \(R.Raw arguments) response -> do
  (args, resp) <- atomically $ do
    args' <- passPtr receiverSide sup mem arguments
    resp' <- newCallback $ \result ->
      traverse (passPtr (flipDir receiverSide) sup mem) result
        >>= breakOrFulfill (contramap R.Raw response)
    pure (args', resp')
  handler (R.Raw args) (contramap R.fromRaw resp)

passPtr :: MonadSTM m => Direction -> Supervisor -> Membrane -> Maybe (U.Ptr 'Const) -> m (Maybe (U.Ptr 'Const))
passPtr dir sup mem = liftSTM . traverse (U.tMsg $ passMessage dir sup mem)

passMessage :: MonadSTM m => Direction -> Supervisor -> Membrane -> M.Message 'Const -> m (M.Message 'Const)
passMessage dir sup mem msg = liftSTM $ do
  caps' <- traverse (pass dir sup mem) (M.getCapTable msg)
  pure $ M.withCapTable caps' msg

pass :: MonadSTM m => Direction -> Supervisor -> Membrane -> URpc.Client -> m URpc.Client
pass dir sup mem inClient = liftSTM $
  case URpc.unwrapServer inClient :: Maybe MembraneWrapped of
    Just mw | onSide dir mw mem -> pure $ client mw
    _ ->
      URpc.export
        sup
        Server.ServerOps
          { Server.handleCast =
              cast $
                MembraneWrapped
                  { client = inClient,
                    membrane = mem,
                    side = dir
                  },
            -- Once we're gc'd, the downstream client will be as well
            -- and then the relevant shutdown logic will still be called.
            -- This introduces latency unfortuantely, but nothing is broken.
            Server.handleStop = pure (),
            Server.handleCall = \interfaceId methodId arguments response ->
              do
                action <- atomically $ policy mem Call {interfaceId, methodId, direction = dir, target = inClient}
                case action of
                  Handle h -> h arguments response
                  Forward ->
                    ( wrapHandler dir sup mem $ \(R.Raw arguments) resp ->
                        let response = contramap R.Raw resp
                         in void $ URpc.call Server.CallInfo {..} inClient
                    )
                      arguments
                      response
          }

onSide :: Direction -> MembraneWrapped -> Membrane -> Bool
onSide dir mw mem =
  membrane mw == mem && dir == side mw
