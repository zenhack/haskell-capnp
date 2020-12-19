{-|
Module: Capnp.Rpc.Transport
Description: Support for exchanging messages with remote vats.

This module provides a 'Transport' type, which provides operations
used to transmit messages between vats in the RPC protocol.
-}
{-# LANGUAGE DataKinds #-}
module Capnp.Rpc.Transport
    ( Transport(..)
    , handleTransport
    , socketTransport
    , tracingTransport
    ) where

import Network.Socket (Socket)
import System.IO      (Handle)

import Capnp.Bits       (WordCount)
import Capnp.Convert    (msgToValue)
import Capnp.IO         (hGetMsg, hPutMsg, sGetMsg, sPutMsg)
import Capnp.Message    (Message, Mutability(Const))
import Text.Show.Pretty (ppShow)

import qualified Capnp.Gen.Capnp.Rpc.Pure as R

-- | A @'Transport'@ handles transmitting RPC messages.
data Transport = Transport
    { sendMsg :: Message 'Const -> IO ()
    -- ^ Send a message
    , recvMsg :: IO (Message 'Const)
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

-- | @'tracingTransport' log trans@ wraps another transport @trans@, loging
-- messages when they are sent or received (using the @log@ function). This
-- can be useful for debugging.
tracingTransport :: (String -> IO ()) -> Transport -> Transport
tracingTransport log trans = Transport
    { sendMsg = \msg -> do
        rpcMsg <- msgToValue msg
        log $ "sending message: " ++ ppShow (rpcMsg :: R.Message)
        sendMsg trans msg
    , recvMsg = do
        msg <- recvMsg trans
        rpcMsg <- msgToValue msg
        log $ "received message: " ++ ppShow (rpcMsg :: R.Message)
        pure msg
    }
