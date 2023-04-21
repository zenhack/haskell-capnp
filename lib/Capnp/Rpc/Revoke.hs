{-# LANGUAGE OverloadedStrings #-}

-- | Module: Capnp.Rpc.Revoke
-- Description: support for revocable capababilities
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

-- | @'makeRevocable' sup cap@ returns a pair @(wrappedCap, revoke)@, such that
-- @wrappedCap@ is @cap@ wrapped by a membrane which forwards all method invocations
-- along until @revoke@ is executed, after which all methods that cross the membrane
-- (in either direction) will return errors.
--
-- Note that, as per usual with membranes, the membrane will wrap any objects returned
-- by method calls. So revoke cuts off access to the entire object graph reached through
-- @cap@.
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
revokedHandler _ response = breakPromise response (eFailed "revoked")
