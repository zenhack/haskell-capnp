{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}
-- | Support for invoking 'Server.MethodHandler's
module Capnp.Rpc.Invoke
    (
    -- * Using high-level representations
      invokePurePromise
    , (?)
    , invokePure
    , InvokePureCtx

    -- * Using low level representations
    , invokeRaw
    ) where


import Control.Concurrent.STM  (atomically)
import Control.Monad.Catch     (MonadThrow)
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad, PrimState)

import Capnp.Classes
    ( Cerialize(cerialize)
    , Decerialize(Cerial, decerialize)
    , FromPtr(fromPtr)
    , ToStruct(toStruct)
    )
import Capnp.TraversalLimit (defaultLimit, evalLimitT)
import Data.Mutable         (freeze)

import qualified Capnp.Message    as M
import qualified Capnp.Promise    as Promise
import qualified Capnp.Rpc.Server as Server
import qualified Capnp.Untyped    as U

-- | Invoke a method by passing it the low-level representation of its parameter,
-- and a 'Fulfiller' that can be used to supply (the low-level representation of)
-- its return value.
invokeRaw ::
    ( MonadThrow m
    , MonadIO m
    , PrimMonad m
    , Decerialize r
    , Decerialize p
    , ToStruct M.ConstMsg (Cerial M.ConstMsg p)
    , FromPtr M.ConstMsg (Cerial M.ConstMsg r)
    ) =>
    Server.MethodHandler m p r
    -> Cerial M.ConstMsg p
    -> Promise.Fulfiller (Cerial M.ConstMsg r)
    -> m ()
invokeRaw method params typedFulfiller = do
    (_, untypedFulfiller) <- liftIO $ atomically $ Promise.newPromiseWithCallbackSTM $ \case
        Left e -> Promise.breakPromiseSTM typedFulfiller e
        Right v -> evalLimitT defaultLimit (fromPtr M.empty v) >>= Promise.fulfillSTM typedFulfiller
    Server.invokeIO
        (Server.toUntypedHandler method)
        (Just (U.PtrStruct (toStruct params)))
        untypedFulfiller

-- | Shorthand for class contstraints needed to invoke a method using
-- the high-level API.
type InvokePureCtx m p r =
    ( MonadThrow m
    , MonadIO m
    , PrimMonad m
    , Decerialize r
    , ToStruct M.ConstMsg (Cerial M.ConstMsg p)
    , ToStruct (M.MutMsg (PrimState m)) (Cerial (M.MutMsg (PrimState m)) p)
    , Cerialize p
    , FromPtr M.ConstMsg (Cerial M.ConstMsg r)
    )

-- | Like 'invokeRaw', but uses the high-level representations of the data
-- types.
invokePure
    :: InvokePureCtx m p r
    => Server.MethodHandler m p r
    -> p
    -> Promise.Fulfiller r
    -> m ()
invokePure method params pureFulfiller = do
    struct <- evalLimitT defaultLimit $ do
        msg <- M.newMessage Nothing
        (toStruct <$> cerialize msg params) >>= freeze
    (_, untypedFulfiller) <- liftIO $ atomically $ Promise.newPromiseWithCallbackSTM $ \case
        Left e -> Promise.breakPromiseSTM pureFulfiller e
        Right v ->
            evalLimitT defaultLimit (fromPtr M.empty v >>= decerialize)
            >>= Promise.fulfillSTM pureFulfiller
    Server.invokeIO
        (Server.toUntypedHandler method)
        (Just (U.PtrStruct struct))
        untypedFulfiller

-- | Like 'invokePure', but returns a promise  instead of accepting a fulfiller.
invokePurePromise
    :: InvokePureCtx m p r
    => Server.MethodHandler m p r
    -> p
    -> m (Promise.Promise r)
invokePurePromise method params = do
    (promise, fulfiller) <- Promise.newPromise
    invokePure method params fulfiller
    pure promise

-- | Alias for 'invokePurePromise'
(?) :: InvokePureCtx m p r
    => Server.MethodHandler m p r
    -> p
    -> m (Promise.Promise r)
(?) = invokePurePromise
