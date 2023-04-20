{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Capnp.Rpc.Server
-- Description: handlers for incoming method calls.
--
-- The term server in this context refers to a thread that handles method calls for
-- a particular capability (The capnproto rpc protocol itself has no concept of
-- clients and servers).
module Capnp.Rpc.Server
  ( Server (..),
    ServerOps (..),
    CallInfo (..),
    runServer,

    -- * Handling methods
    MethodHandler,
    UntypedMethodHandler,
    handleUntypedMethod,

    -- ** Working with untyped data
    untypedHandler,
    toUntypedHandler,
    fromUntypedHandler,
  )
where

import Capnp.Message (Mutability (..))
import Capnp.Rpc.Promise (Fulfiller)
import Capnp.Untyped (Ptr)
import Control.Concurrent.STM
import Data.Typeable (Typeable)
import Data.Word
import qualified Internal.TCloseQ as TCloseQ

-- | a @'MethodHandler' p r@ handles a method call with parameters @p@
-- and return type @r@.
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
newtype MethodHandler p r = MethodHandler
  { handleMethod ::
      Maybe (Ptr 'Const) ->
      Fulfiller (Maybe (Ptr 'Const)) ->
      IO ()
  }

-- | Alias for a 'MethodHandler' whose parameter and return types are
-- untyped pointers.
type UntypedMethodHandler = MethodHandler (Maybe (Ptr 'Const)) (Maybe (Ptr 'Const))

handleUntypedMethod :: UntypedMethodHandler -> Maybe (Ptr 'Const) -> Fulfiller (Maybe (Ptr 'Const)) -> IO ()
handleUntypedMethod = handleMethod

-- | Convert a 'MethodHandler' for any parameter and return types into
-- one that deals with untyped pointers.
toUntypedHandler :: MethodHandler p r -> UntypedMethodHandler
toUntypedHandler MethodHandler {..} = MethodHandler {..}

-- | Inverse of 'toUntypedHandler'
fromUntypedHandler :: UntypedMethodHandler -> MethodHandler p r
fromUntypedHandler MethodHandler {..} = MethodHandler {..}

-- | Construct a method handler from a function accepting an untyped
-- pointer for the method's parameter, and a 'Fulfiller' which accepts
-- an untyped pointer for the method's return value.
untypedHandler ::
  (Maybe (Ptr 'Const) -> Fulfiller (Maybe (Ptr 'Const)) -> IO ()) ->
  MethodHandler (Maybe (Ptr 'Const)) (Maybe (Ptr 'Const))
untypedHandler = MethodHandler

-- | Base class for things that can act as capnproto servers.
class Server a where
  -- | Called when the last live reference to a server is dropped.
  shutdown :: a -> IO ()
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
-- to implement an object.
data ServerOps = ServerOps
  { -- | Handle a method call; takes the interface and method id and returns
    -- a handler for the specific method.
    handleCall ::
      Word64 ->
      Word16 ->
      MethodHandler (Maybe (Ptr 'Const)) (Maybe (Ptr 'Const)),
    -- | Handle shutting-down the receiver; this is called when the last
    -- reference to the capability is dropped.
    handleStop :: IO (),
    -- | used to unwrap the server when reflecting on a local client.
    handleCast :: forall a. Typeable a => Maybe a
  }

-- | A 'CallInfo' contains information about a method call.
data CallInfo = CallInfo
  { -- | The id of the interface whose method is being called.
    interfaceId :: !Word64,
    -- | The method id of the method being called.
    methodId :: !Word16,
    -- | The arguments to the method call.
    arguments :: Maybe (Ptr 'Const),
    -- | A 'Fulfiller' which accepts the method's return value.
    response :: Fulfiller (Maybe (Ptr 'Const))
  }

-- | Handle incoming messages for a given object.
--
-- Accepts a queue of messages to handle, and 'ServerOps' used to handle them.
-- returns when it receives a 'Stop' message.
runServer :: TCloseQ.Q CallInfo -> ServerOps -> IO ()
runServer q ops = go
  where
    go =
      atomically (TCloseQ.read q) >>= \case
        Nothing ->
          pure ()
        Just CallInfo {interfaceId, methodId, arguments, response} -> do
          handleMethod
            (handleCall ops interfaceId methodId)
            arguments
            response
          go
