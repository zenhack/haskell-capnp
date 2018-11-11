{-|
Module: Capnp.Rpc.Transport
Description: Support for exchanging messages with remote vats.

This module provides a 'Transport' type, which provides operations
used to transmit messages between vats in the RPC protocol.
-}
module Capnp.Rpc.Transport
    ( Transport(..)
    , handleTransport
    , socketTransport
    ) where

import Network.Socket (Socket)
import System.IO      (Handle)

import Capnp.Bits    (WordCount)
import Capnp.IO      (hGetMsg, hPutMsg, sGetMsg, sPutMsg)
import Capnp.Message (ConstMsg)

-- | A @'Transport'@ handles transmitting RPC messages.
data Transport = Transport
    { sendMsg :: ConstMsg -> IO ()
    -- ^ Send a message
    , recvMsg :: IO ConstMsg
    -- ^ Receive a message
    }

-- | @'handleTransport' handle limit@ is a transport which reads and writes
-- messages from/to @handle@. It uses @limit@ as the traversal limit when
-- reading messages and decoding.
handleTransport :: Handle -> WordCount -> Transport
handleTransport handle limit = Transport
    { sendMsg = hPutMsg handle
    , recvMsg = hGetMsg handle limit
    }

-- | @'socketTransport' socket limit@ is a transport which reads and writes
-- messages to/from a socket. It uses @limit@ as the traversal limit when
-- reading messages and decoing.
socketTransport :: Socket -> WordCount -> Transport
socketTransport socket limit = Transport
    { sendMsg = sPutMsg socket
    , recvMsg = sGetMsg socket limit
    }
