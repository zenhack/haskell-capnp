{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}
-- |
-- Module: Capnp.Rpc.Invoke
-- Description: Invoke remote methods
--
-- Support for invoking 'Server.MethodHandler's
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

import Control.Monad.STM.Class

import Control.Monad.Catch     (MonadThrow)
import Control.Monad.Primitive (PrimMonad, PrimState)

import Capnp.Classes
    ( Cerialize(cerialize)
    , Decerialize(Cerial, decerialize)
    , FromPtr(fromPtr)
    , ToStruct(toStruct)
    )
import Capnp.Message        (Mutability (..))
import Capnp.TraversalLimit (defaultLimit, evalLimitT)
import Data.Mutable         (freeze)

import qualified Capnp.Message     as M
import qualified Capnp.Rpc.Promise as Promise
import qualified Capnp.Rpc.Server  as Server
import qualified Capnp.Untyped     as U

-- | Invoke a method by passing it the low-level representation of its parameter,
-- and a 'Fulfiller' that can be used to supply (the low-level representation of)
-- its return value.
invokeRaw ::
    ( MonadThrow m
    , MonadSTM m
    , PrimMonad m
    , Decerialize r
    , Decerialize p
    , ToStruct 'Const (Cerial 'Const p)
    , FromPtr 'Const (Cerial 'Const r)
    ) =>
    Server.MethodHandler m p r
    -> Cerial 'Const p
    -> Promise.Fulfiller (Cerial 'Const r)
    -> m ()
invokeRaw method params typedFulfiller = do
    (_, untypedFulfiller) <- liftSTM $ Promise.newPromiseWithCallback $ \case
        Left e -> Promise.breakPromise typedFulfiller e
        Right v -> evalLimitT defaultLimit (fromPtr M.empty v) >>= Promise.fulfill typedFulfiller
    Server.invoke
        (Server.toUntypedHandler method)
        (Just (U.PtrStruct (toStruct params)))
        untypedFulfiller

-- | Shorthand for class contstraints needed to invoke a method using
-- the high-level API.
type InvokePureCtx m p r =
    ( MonadThrow m
    , MonadSTM m
    , PrimMonad m
    , Decerialize r
    , ToStruct 'Const (Cerial 'Const p)
    , ToStruct ('Mut (PrimState m)) (Cerial ('Mut (PrimState m)) p)
    , Cerialize (PrimState m) p
    , FromPtr 'Const (Cerial 'Const r)
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
    (_, untypedFulfiller) <- liftSTM $ Promise.newPromiseWithCallback $ \case
        Left e -> Promise.breakPromise pureFulfiller e
        Right v ->
            evalLimitT defaultLimit (fromPtr M.empty v >>= decerialize)
            >>= Promise.fulfill pureFulfiller
    Server.invoke
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
