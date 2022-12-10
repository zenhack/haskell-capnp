{-# LANGUAGE NamedFieldPuns #-}

module Capnp.Rpc.Membrane
  ( Action (..),
    Direction (..),
    Call (..),
    Policy,
    enclose,
    exclude,
  )
where

import qualified Capnp.Rpc.Common as Rpc
import qualified Capnp.Rpc.Server as Server
import qualified Capnp.Rpc.Untyped as URpc
import Control.Concurrent.STM
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
            Server.handleCall = \interfaceId methodId -> error "TODO"
          }

onSide :: Direction -> MembraneWrapped -> Membrane -> Bool
onSide dir mw mem =
  membrane mw == mem && dir == side mw
