{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-|
Module: Capnp.Rpc.Server
Description: handlers for incoming method calls.

A 'Server' in this context refers to a thread that handles method calls for
a particular capability (The capnproto rpc protocol itself has no concept of
clients and servers).
-}
module Capnp.Rpc.Server
    ( MethodHandler
    , pureHandler
    , untypedHandler
    , methodThrow
    , methodUnimplemented
    , ServerOps(..)
    , ServerMsg(..)
    , CallInfo(..)
    , runServer
    ) where

import Data.Word
import UnliftIO.STM

import Control.Monad.Catch    (MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Default           (def)
import Supervisors            (Supervisor, superviseSTM)

import Capnp.Classes (Cerialize, Decerialize(Cerial), FromPtr)
import Capnp.Message (ConstMsg)
import Capnp.Promise (Fulfiller, breakPromiseIO)
import Capnp.Untyped (Ptr)

import qualified Capnp.Gen.Capnp.Rpc.Pure as RpcGen

-- | a @'MethodHandler' m p r@ handles a method call with parameters @p@
-- and return type @r@, in monad @m@. See Note [Method handling].
newtype MethodHandler m p r = MethodHandler
    { handleMethod
        :: Maybe (Ptr ConstMsg)
        -> Fulfiller (Maybe (Ptr ConstMsg))
        -> m ()
    }

-- | 'pureHandler' creates a method handler from a function accepting the
-- receiver and parameters, and returning a monadic value computing the
-- result. See Note [Method handling].
pureHandler ::
    ( MonadThrow m
    , Decerialize p
    , FromPtr ConstMsg (Cerial ConstMsg p)
    , Cerialize r
    -- TODO: something like the below is needed, but I(zenhack) am having a
    -- hard time figuring out what the constraint should actually be; the
    -- below doesn't type-check.
    -- , ToPtr s (Cerial ConstMsg r)
    )
    => (c -> p -> m r)
    -> MethodHandler m (Cerial ConstMsg p) (Cerial ConstMsg r)
pureHandler = undefined

-- | Convert a 'MethodHandler' for any parameter and return types into
-- one that deals with untyped pointers.
untypedHandler
    :: MethodHandler m p r
    -> MethodHandler m (Maybe (Ptr ConstMsg)) (Maybe (Ptr ConstMsg))
untypedHandler MethodHandler{..} = MethodHandler{..}

methodThrow :: MonadIO m => RpcGen.Exception -> MethodHandler m p r
methodThrow exn = MethodHandler
    { handleMethod = \_ fulfiller -> liftIO $ breakPromiseIO fulfiller exn
    }

methodUnimplemented :: MonadIO m => MethodHandler m p r
methodUnimplemented = methodThrow def
    { RpcGen.type_ = RpcGen.Exception'Type'unimplemented
    , RpcGen.reason = "Method unimplemented"
    }

-- | The operations necessary to receive and handle method calls, i.e.
-- to implement an object. It is parametrized over the monadic context
-- in which methods are serviced.
data ServerOps m = ServerOps
    { handleCall
        :: Word64 -- ^ Interface Id
        -> Word16 -- ^ Method Id
        -> MethodHandler m (Maybe (Ptr ConstMsg)) (Maybe (Ptr ConstMsg))
    -- ^ Handle a method call; takes the interface and method id and returns
    -- a handler for the specific method.
    , handleStop :: m ()
    -- ^ Handle shutting-down the receiver; this is called when the last
    -- reference to the capability is dropped.
    }

-- | A 'CallInfo' contains the information necessary to handle an untyped
-- method call.
data CallInfo = CallInfo
    { interfaceId :: !Word64
    -- ^ The id of the interface whose method is being called.
    , methodId    :: !Word16
    -- ^ The method id of the method being called.
    , arguments   :: Maybe (Ptr ConstMsg)
    -- ^ The arguments to the method call.
    , response    :: Fulfiller (Maybe (Ptr ConstMsg))
    -- ^ A 'Fulfiller' which accepts the method's return value.
    }

-- | A message to be sent to a running server.
data ServerMsg
    = Stop
    -- ^ Shut down the server
    | Call CallInfo
    -- ^ Call a method on the server.

-- | Start a thread managing incoming messages for an object.
--
-- The new thread will be managed by the given supervisor. It will process
-- 'ServerMsg's from the queue one at a time, using the provided 'ServerOps'.
-- When it receives a 'Stop' message, it will exit.
runServer :: TQueue ServerMsg -> ServerOps IO -> Supervisor -> STM ()
runServer q recvr sup =
    superviseSTM sup go
  where
    go = atomically (readTQueue q) >>= \case
        Stop ->
            handleStop recvr
        Call CallInfo{interfaceId, methodId, arguments, response} -> do
            handleMethod
                (handleCall recvr interfaceId methodId)
                arguments
                response
            go


-- Note [Method handling]
-- ======================
--
-- We represent method handlers via an abstract type 'MethodHandler',
-- parametrized over the reciever type, parameter and return types, and the
-- monadic context in which it runs. This allows us to provide different
-- strategies for actually handling methods; there are helper functions
-- which construct these handlers. For example:
--
-- * 'pureHandler' constructs a 'MethodHandler' from a function that works
--   with the types exposed by the high-level API.
-- * We will likely additionally provide handlers affording:
--   * Working directly with the low-level data types.
--   * Replying to the method call asynchronously, allowing later method
--     calls to be serviced before the current one is finished.
