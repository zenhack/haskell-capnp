{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Network.RPC.Capnp where

import Data.Word

import System.IO (Handle, hClose)

import qualified Data.Map.Strict as M

import Capnp.Capnp.Rpc.Pure

import Data.Capnp.Pure (def, hGetValue, hPutValue)

-- These aliases are actually defined in the schema, but the schema compiler
-- doesn't expose them to the code generator plugin, so we re-define them
-- ourselves.
type QuestionId = Word32
type AnswerId = Word32
type ExportId = Word32
type ImportId = Word32

class Transport t m where
    sendMsg :: t -> Message -> m ()
    recvMsg :: t -> m Message
    disconnect :: t -> m ()

data HandleTransport = HandleTransport
    { handle :: Handle
    , limit  :: !Int
    }

instance Transport HandleTransport IO where
    sendMsg HandleTransport{handle} = hPutValue handle
    recvMsg HandleTransport{handle, limit} = hGetValue handle limit
    disconnect = hClose . handle

data Client
    = RemoteClient
        { clientId :: ImportId
        , vatState :: () -- TVar VatState
        }

data VatState = VatState
    { questions :: M.Map QuestionId Message
    , answers   :: M.Map AnswerId Message
    , imports   :: M.Map ImportId CapDescriptor
    , exports   :: M.Map ExportId CapDescriptor
    }

recvLoop :: (Monad m, Transport t m) => t -> m ()
recvLoop transport = do
    msg <- recvMsg transport
    sendMsg transport $ Message'unimplemented msg
    sendMsg transport $ Message'abort def
        { reason = "Unimplemented"
        , type_ = Exception'Type'unimplemented
        }
    disconnect transport
