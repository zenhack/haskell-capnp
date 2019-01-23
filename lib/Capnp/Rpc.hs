module Capnp.Rpc
    (
    -- * Establishing connections
      handleConn
    , ConnConfig(..)

    -- * Calling methods
    , (?)

    -- * throwing errors
    , throwFailed

    -- * Transmitting messages
    , Transport
    , socketTransport
    , handleTransport
    ) where

import Capnp.Rpc.Errors    (throwFailed)
import Capnp.Rpc.Invoke    ((?))
import Capnp.Rpc.Transport (Transport, handleTransport, socketTransport)
import Capnp.Rpc.Untyped   (ConnConfig(..), handleConn)
