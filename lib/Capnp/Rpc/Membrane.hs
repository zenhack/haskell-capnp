{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Capnp.Rpc.Membrane
  ( Action (..),
    Direction (..),
    Call (..),
    Policy,
    enclose,
    exclude,
  )
where

import qualified Capnp.Message as M
import Capnp.Mutability (Mutability (..))
import qualified Capnp.Rpc.Common as Rpc
import Capnp.Rpc.Promise (breakOrFulfill, newCallback)
import qualified Capnp.Rpc.Server as Server
import qualified Capnp.Rpc.Untyped as URpc
import qualified Capnp.Untyped as U
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.STM.Class
import Data.Typeable (Typeable, cast)
import Data.Word
import Supervisors (Supervisor)

data Action
  = Redirect URpc.Client
  | Forward
  deriving (Eq)

data Direction = In | Out
  deriving (Show, Read, Eq)

flipDir :: Direction -> Direction
flipDir In = Out
flipDir Out = In

type Side = Direction

data Call = Call
  { direction :: Direction,
    interfaceId :: Word64,
    methodId :: Word16,
    target :: URpc.Client
  }

type Policy = Call -> Action

enclose :: MonadSTM m => Supervisor -> Rpc.Client a -> Policy -> m (Rpc.Client a)
enclose = newMembrane In

exclude :: MonadSTM m => Supervisor -> Rpc.Client a -> Policy -> m (Rpc.Client a)
exclude = newMembrane Out

newMembrane :: MonadSTM m => Direction -> Supervisor -> Rpc.Client a -> Policy -> m (Rpc.Client a)
newMembrane dir sup (Rpc.Client toWrap) policy = liftSTM $ do
  identity <- newTVar ()
  let mem = Membrane {policy, identity}
  Rpc.Client <$> pass dir sup mem toWrap

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
wrapHandler receiverSide sup mem handler = Server.untypedHandler $ \arguments response -> do
  (args, resp) <- atomically $ do
    args' <- passPtr receiverSide sup mem arguments
    resp' <- newCallback $ \result ->
      traverse (passPtr (flipDir receiverSide) sup mem) result
        >>= breakOrFulfill response
    pure (args', resp')
  Server.handleUntypedMethod handler args resp

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
            Server.handleCall = \interfaceId methodId ->
              let handleWith client = Server.untypedHandler $
                    \arguments response -> void $ URpc.call Server.CallInfo {..} client
               in case policy mem Call {interfaceId, methodId, direction = dir, target = inClient} of
                    Forward -> wrapHandler dir sup mem (handleWith inClient)
                    Redirect target -> handleWith target
          }

onSide :: Direction -> MembraneWrapped -> Membrane -> Bool
onSide dir mw mem =
  membrane mw == mem && dir == side mw
