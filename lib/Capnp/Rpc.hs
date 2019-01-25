-- |
-- Module: Capnp.Rpc
-- Description: Cap'n Proto RPC system
--
-- This module exposes the most commonly used parts of the RPC subsystem.
module Capnp.Rpc
    (
    -- * Establishing connections
      handleConn
    , ConnConfig(..)

    -- * Calling methods
    , (?)

    -- * Handling method calls
    , MethodHandler
    , pureHandler
    , rawHandler
    , rawAsyncHandler
    , methodUnimplemented
    , methodThrow

    -- * throwing errors
    , throwFailed

    -- * Transmitting messages
    , Transport(..)
    , socketTransport
    , handleTransport
    , tracingTransport

    -- * Promises
    , module Capnp.Rpc.Promise

    -- * Clients
    , Client
    , IsClient(..)

    -- * Supervisors
    , module Supervisors
    ) where

import Supervisors

import Capnp.Rpc.Errors    (throwFailed)
import Capnp.Rpc.Invoke    ((?))
import Capnp.Rpc.Promise
import Capnp.Rpc.Server
    ( MethodHandler
    , methodThrow
    , methodUnimplemented
    , pureHandler
    , rawAsyncHandler
    , rawHandler
    )
import Capnp.Rpc.Transport
    (Transport(..), handleTransport, socketTransport, tracingTransport)
import Capnp.Rpc.Untyped   (Client, ConnConfig(..), IsClient(..), handleConn)
