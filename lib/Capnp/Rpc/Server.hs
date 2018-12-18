{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-|
Module: Capnp.Rpc.Server
Description: handlers for incoming method calls.

The term server in this context refers to a thread that handles method calls for
a particular capability (The capnproto rpc protocol itself has no concept of
clients and servers).
-}
module Capnp.Rpc.Server
    ( ServerOps(..)
    , CallInfo(..)
    , runServer

    -- * Handling methods
    , MethodHandler
    , pureHandler
    , toUntypedHandler
    , fromUntypedHandler
    , untypedHandler
    , methodThrow
    , methodUnimplemented

    -- * Invoking methods
    , invokeIO
    ) where

import Control.Concurrent.STM
import Data.Word

import Control.Exception.Safe  (MonadCatch, try)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad, PrimState)

import Capnp.Classes
    (Cerialize, Decerialize(Cerial, decerialize), FromPtr(fromPtr), ToStruct)
import Capnp.Convert        (valueToMsg)
import Capnp.Message        (ConstMsg, MutMsg)
import Capnp.Promise        (Fulfiller, breakPromiseIO, fulfillIO)
import Capnp.Rpc.Errors     (eMethodUnimplemented, wrapException)
import Capnp.TraversalLimit (defaultLimit, evalLimitT)
import Capnp.Untyped        (Ptr)
import Data.Mutable         (freeze)

import qualified Capnp.Gen.Capnp.Rpc.Pure as RpcGen
import qualified Capnp.Message            as Message
import qualified Capnp.Untyped            as Untyped
import qualified Internal.TCloseQ         as TCloseQ

-- | a @'MethodHandler' m p r@ handles a method call with parameters @p@
-- and return type @r@, in monad @m@. See Note [Method handling].
--
-- We represent method handlers via an abstract type 'MethodHandler',
-- parametrized over parameter (@p@) and return (@r@) types, and the
-- monadic context in which it runs (@m@). This allows us to provide
-- different strategies for actually handling methods; there are various
-- helper functions which construct these handlers.
--
-- * We will likely additionally provide handlers affording:
--   * Working directly with the low-level data types.
--   * Replying to the method call asynchronously, allowing later method
--     calls to be serviced before the current one is finished.
newtype MethodHandler m p r = MethodHandler
    { handleMethod
        :: Maybe (Ptr ConstMsg)
        -> Fulfiller (Maybe (Ptr ConstMsg))
        -> m ()
    }

invokeIO
    :: MonadIO m
    => MethodHandler m (Maybe (Ptr ConstMsg)) (Maybe (Ptr ConstMsg))
    -> Maybe (Ptr ConstMsg)
    -> Fulfiller (Maybe (Ptr ConstMsg))
    -> m ()
invokeIO = handleMethod

-- | @'pureHandler' f cap@ is a 'MethodHandler' which calls a function @f@
-- that accepts the receiver and the parameter type as exposed by the
-- high-level API, and returns the high-level API representation of the
-- return type.
pureHandler ::
    ( MonadCatch m
    , MonadIO m
    , PrimMonad m
    , s ~ PrimState m
    , Decerialize p
    , FromPtr ConstMsg (Cerial ConstMsg p)
    , Cerialize r
    , ToStruct (MutMsg s) (Cerial (MutMsg s) r)
    ) =>
    (cap -> p -> m r)
    -> cap
    -> MethodHandler m p r
pureHandler f cap = MethodHandler
    { handleMethod = \ptr reply -> do
        param <- evalLimitT defaultLimit $
            fromPtr Message.empty ptr >>= decerialize
        result <- try $ f cap param
        case result of
            Right val -> do
                struct <- evalLimitT defaultLimit $
                    valueToMsg val >>= freeze >>= Untyped.rootPtr
                fulfillIO reply (Just (Untyped.PtrStruct struct))
            Left e ->
                -- TODO: find a way to get the connection config's debugMode
                -- option to be accessible from here, so we can use it.
                breakPromiseIO reply (wrapException False e)
    }

-- | Convert a 'MethodHandler' for any parameter and return types into
-- one that deals with untyped pointers.
toUntypedHandler
    :: MethodHandler m p r
    -> MethodHandler m (Maybe (Ptr ConstMsg)) (Maybe (Ptr ConstMsg))
toUntypedHandler MethodHandler{..} = MethodHandler{..}

-- | Inverse of 'toUntypedHandler'
fromUntypedHandler
    :: MethodHandler m (Maybe (Ptr ConstMsg)) (Maybe (Ptr ConstMsg))
    -> MethodHandler m p r
fromUntypedHandler MethodHandler{..} = MethodHandler{..}

-- | Construct a method handler from a function accepting an untyped
-- pointer for the method's parameter, and a 'Fulfiller' which accepts
-- an untyped pointer for the method's return value.
untypedHandler
    :: (Maybe (Ptr ConstMsg) -> Fulfiller (Maybe (Ptr ConstMsg)) -> m ())
    -> MethodHandler m (Maybe (Ptr ConstMsg)) (Maybe (Ptr ConstMsg))
untypedHandler = MethodHandler

-- | @'methodThrow' exn@ is a 'MethodHandler' which always throws @exn@.
methodThrow :: MonadIO m => RpcGen.Exception -> MethodHandler m p r
methodThrow exn = MethodHandler
    { handleMethod = \_ fulfiller -> liftIO $ breakPromiseIO fulfiller exn
    }

-- | A 'MethodHandler' which always throws an @unimplemented@ exception.
methodUnimplemented :: MonadIO m => MethodHandler m p r
methodUnimplemented = methodThrow eMethodUnimplemented

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

-- | A 'CallInfo' contains information about a method call.
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

-- | Handle incoming messages for a given object.
--
-- Accepts a queue of messages to handle, and 'ServerOps' used to handle them.
-- returns when it receives a 'Stop' message.
runServer :: TCloseQ.Q CallInfo -> ServerOps IO -> IO ()
runServer q ops = go
  where
    go = atomically (TCloseQ.read q) >>= \case
        Nothing ->
            handleStop ops
        Just CallInfo{interfaceId, methodId, arguments, response} -> do
            handleMethod
                (handleCall ops interfaceId methodId)
                arguments
                response
            go
