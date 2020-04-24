{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-|
Module: Capnp.Rpc.Server
Description: handlers for incoming method calls.

The term server in this context refers to a thread that handles method calls for
a particular capability (The capnproto rpc protocol itself has no concept of
clients and servers).
-}
module Capnp.Rpc.Server
    ( Server(..)
    , ServerOps(..)
    , CallInfo(..)
    , runServer

    -- * Handling methods
    , MethodHandler
    -- ** Using high-level representations
    , pureHandler
    -- ** Using low-level representations
    , rawHandler
    , rawAsyncHandler
    -- ** Always throwing exceptions
    , methodThrow
    , methodUnimplemented
    -- ** Working with untyped data
    , untypedHandler
    , toUntypedHandler
    , fromUntypedHandler

    -- * Invoking methods
    , invoke
    ) where

import Control.Concurrent.STM
import Control.Monad.STM.Class
import Data.Word

import Control.Exception.Safe  (MonadCatch, finally, try)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Typeable           (Typeable)

import Capnp.Classes
    ( Cerialize
    , Decerialize(Cerial, decerialize)
    , FromPtr(fromPtr)
    , ToStruct(toStruct)
    )
import Capnp.Convert        (valueToMsg)
import Capnp.Message        (ConstMsg, MutMsg)
import Capnp.Rpc.Errors     (eMethodUnimplemented, wrapException)
import Capnp.Rpc.Promise    (Fulfiller, breakPromise, fulfill, newCallback)
import Capnp.TraversalLimit (defaultLimit, evalLimitT)
import Capnp.Untyped        (Ptr)
import Data.Mutable         (freeze)

import qualified Capnp.Gen.Capnp.Rpc.Pure as RpcGen
import qualified Capnp.Message            as Message
import qualified Capnp.Untyped            as Untyped
import qualified Internal.TCloseQ         as TCloseQ

-- | a @'MethodHandler' m p r@ handles a method call with parameters @p@
-- and return type @r@, in monad @m@.
--
-- The library represents method handlers via an abstract type
-- 'MethodHandler', parametrized over parameter (@p@) and return (@r@)
-- types, and the monadic context in which it runs (@m@). This allows us
-- to provide different strategies for actually handling methods; there
-- are various helper functions which construct these handlers.
--
-- At some point we will likely additionally provide handlers affording:
--
-- * Working directly with the low-level data types.
-- * Replying to the method call asynchronously, allowing later method
--   calls to be serviced before the current one is finished.
newtype MethodHandler m p r = MethodHandler
    { handleMethod
        :: Maybe (Ptr ConstMsg)
        -> Fulfiller (Maybe (Ptr ConstMsg))
        -> m ()
    }

invoke
    :: MonadSTM m
    => MethodHandler m (Maybe (Ptr ConstMsg)) (Maybe (Ptr ConstMsg))
    -> Maybe (Ptr ConstMsg)
    -> Fulfiller (Maybe (Ptr ConstMsg))
    -> m ()
invoke = handleMethod

-- | @'pureHandler' f cap@ is a 'MethodHandler' which calls a function @f@
-- that accepts the receiver and the parameter type as exposed by the
-- high-level API, and returns the high-level API representation of the
-- return type.
pureHandler ::
    ( MonadCatch m
    , MonadSTM m
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
                liftSTM $ fulfill reply (Just (Untyped.PtrStruct struct))
            Left e ->
                -- TODO: find a way to get the connection config's debugMode
                -- option to be accessible from here, so we can use it.
                liftSTM $ breakPromise reply (wrapException False e)
    }

-- | Like 'pureHandler', except that the parameter and return value use the
-- low-level representation.
rawHandler ::
    ( MonadCatch m
    , MonadSTM m
    , PrimMonad m
    , s ~ PrimState m
    , Decerialize p
    , FromPtr ConstMsg (Cerial ConstMsg p)
    , Decerialize r
    , ToStruct ConstMsg (Cerial ConstMsg r)
    ) =>
    (cap -> Cerial ConstMsg p -> m (Cerial ConstMsg r))
    -> cap
    -> MethodHandler m p r
rawHandler f cap = MethodHandler
    { handleMethod = \ptr reply -> do
        cerial <- evalLimitT defaultLimit $ fromPtr Message.empty ptr
        result <- try $ f cap cerial
        case result of
            Right val -> liftSTM $ fulfill reply (Just (Untyped.PtrStruct (toStruct val)))
            Left e -> liftSTM $ breakPromise reply (wrapException False e)
    }

-- | Like 'rawHandler', except that it takes a fulfiller for the result,
-- instead of returning it. This allows the result to be supplied some time
-- after the method returns, making it possible to service other method
-- calls before the result is available.
rawAsyncHandler ::
    ( MonadCatch m
    , MonadSTM m
    , PrimMonad m
    , s ~ PrimState m
    , Decerialize p
    , FromPtr ConstMsg (Cerial ConstMsg p)
    , Decerialize r
    , ToStruct ConstMsg (Cerial ConstMsg r)
    ) =>
    (cap -> Cerial ConstMsg p -> Fulfiller (Cerial ConstMsg r) -> m ())
    -> cap
    -> MethodHandler m p r
rawAsyncHandler f cap = MethodHandler
    { handleMethod = \ptr reply -> do
        fulfiller <- newCallback $ \case
            Left e -> breakPromise reply e
            Right v -> fulfill reply $ Just (Untyped.PtrStruct (toStruct v))
        cerial <- evalLimitT defaultLimit $ fromPtr Message.empty ptr
        f cap cerial fulfiller
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
    { handleMethod = \_ fulfiller -> liftIO $ breakPromise fulfiller exn
    }

-- | A 'MethodHandler' which always throws an @unimplemented@ exception.
methodUnimplemented :: MonadIO m => MethodHandler m p r
methodUnimplemented = methodThrow eMethodUnimplemented

-- | Base class for things that can act as capnproto servers.
class Monad m => Server m a where
    -- | Called when the last live reference to a server is dropped.
    shutdown :: a -> m ()
    shutdown _ = pure ()

-- | The operations necessary to receive and handle method calls, i.e.
-- to implement an object. It is parametrized over the monadic context
-- in which methods are serviced.
data ServerOps m = ServerOps
    { handleCall
        :: Word64
        -> Word16
        -> MethodHandler m (Maybe (Ptr ConstMsg)) (Maybe (Ptr ConstMsg))
    -- ^ Handle a method call; takes the interface and method id and returns
    -- a handler for the specific method.
    , handleStop :: m ()
    -- ^ Handle shutting-down the receiver; this is called when the last
    -- reference to the capability is dropped.
    , handleCast :: forall a. Typeable a => Maybe a
    -- ^ used to unwrap the server when reflecting on a local client.
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
runServer q ops = go `finally` handleStop ops
  where
    go = atomically (TCloseQ.read q) >>= \case
        Nothing ->
            pure ()
        Just CallInfo{interfaceId, methodId, arguments, response} -> do
            handleMethod
                (handleCall ops interfaceId methodId)
                arguments
                response
            go
