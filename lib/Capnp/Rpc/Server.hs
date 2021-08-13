{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
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
    -- ** Working with untyped data
    , untypedHandler
    , toUntypedHandler
    , fromUntypedHandler
    ) where

import Control.Concurrent.STM
import Data.Word

import Data.Typeable (Typeable)

import Capnp.Message     (Mutability(..))
import Capnp.Rpc.Promise (Fulfiller)
import Capnp.Untyped     (Ptr)

import qualified Internal.TCloseQ as TCloseQ

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
        :: Maybe (Ptr 'Const)
        -> Fulfiller (Maybe (Ptr 'Const))
        -> m ()
    }

-- | Convert a 'MethodHandler' for any parameter and return types into
-- one that deals with untyped pointers.
toUntypedHandler
    :: MethodHandler m p r
    -> MethodHandler m (Maybe (Ptr 'Const)) (Maybe (Ptr 'Const))
toUntypedHandler MethodHandler{..} = MethodHandler{..}

-- | Inverse of 'toUntypedHandler'
fromUntypedHandler
    :: MethodHandler m (Maybe (Ptr 'Const)) (Maybe (Ptr 'Const))
    -> MethodHandler m p r
fromUntypedHandler MethodHandler{..} = MethodHandler{..}

-- | Construct a method handler from a function accepting an untyped
-- pointer for the method's parameter, and a 'Fulfiller' which accepts
-- an untyped pointer for the method's return value.
untypedHandler
    :: (Maybe (Ptr 'Const) -> Fulfiller (Maybe (Ptr 'Const)) -> m ())
    -> MethodHandler m (Maybe (Ptr 'Const)) (Maybe (Ptr 'Const))
untypedHandler = MethodHandler

-- | Base class for things that can act as capnproto servers.
class Monad m => Server m a | a -> m where
    -- | Called when the last live reference to a server is dropped.
    shutdown :: a -> m ()
    shutdown _ = pure ()

    -- | Try to extract a value of a given type. The default implementation
    -- always fails (returns 'Nothing'). If an instance chooses to implement
    -- this, it will be possible to use "reflection" on clients that point
    -- at local servers to dynamically unwrap the server value. A typical
    -- implementation will just call Typeable's @cast@ method, but this
    -- needn't be the case -- a server may wish to allow local peers to
    -- unwrap some value that is not exactly the data the server has access
    -- to.
    unwrap :: Typeable b => a -> Maybe b
    unwrap _ = Nothing

-- | The operations necessary to receive and handle method calls, i.e.
-- to implement an object. It is parametrized over the monadic context
-- in which methods are serviced.
data ServerOps m = ServerOps
    { handleCall
        :: Word64
        -> Word16
        -> MethodHandler m (Maybe (Ptr 'Const)) (Maybe (Ptr 'Const))
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
    , arguments   :: Maybe (Ptr 'Const)
    -- ^ The arguments to the method call.
    , response    :: Fulfiller (Maybe (Ptr 'Const))
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
            pure ()
        Just CallInfo{interfaceId, methodId, arguments, response} -> do
            handleMethod
                (handleCall ops interfaceId methodId)
                arguments
                response
            go
