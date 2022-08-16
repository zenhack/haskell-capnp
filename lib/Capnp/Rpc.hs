-- |
-- Module: Capnp.Rpc
-- Description: Cap'n Proto RPC system
--
-- This module exposes the most commonly used parts of the RPC subsystem.
module Capnp.Rpc
  ( -- * Establishing connections
    handleConn,
    ConnConfig (..),

    -- * throwing errors
    throwFailed,

    -- * Transmitting messages
    Transport (..),
    socketTransport,
    handleTransport,
    tracingTransport,

    -- * Promises
    module Capnp.Rpc.Promise,

    -- * Clients
    Client,
    IsClient (..),
    newPromiseClient,
    waitClient,

    -- ** Reflection
    Untyped.unwrapServer,

    -- * Supervisors
    module Supervisors,

    -- * Misc.
  )
where

import Capnp.Rpc.Errors (throwFailed)
import Capnp.Rpc.Promise
import Capnp.Rpc.Transport
  ( Transport (..),
    handleTransport,
    socketTransport,
    tracingTransport,
  )
import Capnp.Rpc.Untyped
  ( Client,
    ConnConfig (..),
    IsClient (..),
    handleConn,
    newPromiseClient,
    waitClient,
  )
import qualified Capnp.Rpc.Untyped as Untyped
import Supervisors
