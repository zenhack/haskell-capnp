{-|
Module: Capnp.Rpc.Transport
Description: Support for exchanging messages with remote vats.

This module provides a 'Transport' type, which provides operations
used to transmit messages between vats in the RPC protocol.
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
module Capnp.Rpc.Transport
    ( Transport(..)
    , handleTransport
    , socketTransport
    , tracingTransport
    , TraceConfig(..)
    ) where

import Prelude hiding (log)

import Network.Socket (Socket)
import System.IO      (Handle)

import Capnp.Bits           (WordCount)
import Capnp.Convert        (msgToParsed)
import Capnp.IO             (hGetMsg, hPutMsg, sGetMsg, sPutMsg)
import Capnp.Message        (Message, Mutability(Const))
import Capnp.New.Classes    (Parsed)
import Capnp.TraversalLimit (evalLimitT)
import Data.Default         (def)
import Text.Show.Pretty     (ppShow)

import qualified Capnp.Gen.Capnp.Rpc.New as R

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

data TraceConfig = TraceConfig
    { log          :: String -> IO ()
    , showPayloads :: !Bool
    }

-- | @'tracingTransport' log trans@ wraps another transport @trans@, loging
-- messages when they are sent or received (using the @log@ function). This
-- can be useful for debugging.
tracingTransport :: TraceConfig -> Transport -> Transport
tracingTransport tcfg trans = Transport
    { sendMsg = \msg -> do
        rpcMsg <- evalLimitT maxBound $ msgToParsed @R.Message msg
        log tcfg $ "sending message: " ++ ppShow (editForTrace tcfg rpcMsg)
        sendMsg trans msg
    , recvMsg = do
        msg <- recvMsg trans
        rpcMsg <- evalLimitT maxBound $ msgToParsed @R.Message msg
        log tcfg $ "received message: " ++ ppShow (editForTrace tcfg rpcMsg)
        pure msg
    }

editForTrace :: TraceConfig -> Parsed R.Message -> Parsed R.Message
editForTrace tcfg rpcMsg =
    if showPayloads tcfg then
        rpcMsg
    else
        (case rpcMsg of
            R.Message (R.Message'call call) ->
                R.Message $ R.Message'call $
                    call { R.params = def }
            R.Message (R.Message'return R.Return{union' = R.Return'results _, .. }) ->
                R.Message $ R.Message'return $
                    R.Return { R.union' = R.Return'results def, .. }
            _ ->
                rpcMsg
        )
