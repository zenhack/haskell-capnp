{-# LANGUAGE OverloadedStrings #-}

module Capnp.Rpc.Revoke
  ( makeRevocable,
  )
where

import Capnp.Rpc.Errors (eFailed)
import qualified Capnp.Rpc.Membrane as Membrane
import Capnp.Rpc.Promise (breakPromise)
import qualified Capnp.Rpc.Server as Server
import Capnp.Rpc.Untyped (IsClient)
import Control.Concurrent.STM
import Control.Monad.STM.Class (MonadSTM, liftSTM)
import Supervisors (Supervisor)

makeRevocable :: (MonadSTM m, IsClient c) => Supervisor -> c -> m (c, STM ())
makeRevocable sup client = liftSTM $ do
  isRevoked <- newTVar False
  wrappedClient <- Membrane.enclose sup client (revokerPolicy isRevoked)
  pure (wrappedClient, writeTVar isRevoked True)

revokerPolicy :: TVar Bool -> Membrane.Policy
revokerPolicy isRevoked _call = do
  revoked <- readTVar isRevoked
  pure $
    if revoked
      then Membrane.Handle revokedHandler
      else Membrane.Forward

revokedHandler :: Server.UntypedMethodHandler
revokedHandler = Server.untypedHandler $ \_ response -> breakPromise response (eFailed "revoked")
