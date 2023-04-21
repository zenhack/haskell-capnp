{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Internal.Rpc.Export (export) where

import Capnp.New.Rpc.Server
import Capnp.Rpc.Common (Client (..))
import qualified Capnp.Rpc.Untyped as URpc
import Control.Monad.STM.Class (MonadSTM (liftSTM))
import Data.Proxy (Proxy (..))
import Supervisors (Supervisor)

-- | Export the server as a client for interface @i@. Spawns a server thread
-- with its lifetime bound to the supervisor.
export :: forall i s m. (MonadSTM m, Export i, Server i s, SomeServer s) => Supervisor -> s -> m (Client i)
export sup srv = liftSTM $ Client <$> URpc.export sup (exportToServerOps (Proxy @i) srv)
