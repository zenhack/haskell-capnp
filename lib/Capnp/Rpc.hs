{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}
module Capnp.Rpc
    ( invokeRaw
    , invokePure
    , invokePurePromise
    , (?)
    , throwFailed

    -- re-exported:
    , Transport
    , socketTransport
    , handleTransport
    ) where

import Control.Concurrent.STM  (atomically)
import Control.Monad.Catch     (MonadThrow(throwM))
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Default            (def)
import Data.Text               (Text)

import Capnp.Classes
    ( Cerialize(cerialize)
    , Decerialize(Cerial, decerialize)
    , FromPtr(fromPtr)
    , ToStruct(toStruct)
    )
import Capnp.TraversalLimit (defaultLimit, evalLimitT)
import Data.Mutable         (freeze)

import qualified Capnp.Gen.Capnp.Rpc.Pure as RpcGen
import qualified Capnp.Message            as M
import qualified Capnp.Promise            as Promise
import qualified Capnp.Rpc.Server         as Server
import qualified Capnp.Untyped            as U

-- Just for re-exporting
import Capnp.Rpc.Transport (Transport, handleTransport, socketTransport)


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
    (_, untypedFulfiller) <- liftIO $ atomically $ Promise.newPromiseWithCallback $ \case
        Left e -> Promise.breakPromise typedFulfiller e
        Right v -> evalLimitT defaultLimit (fromPtr M.empty v) >>= Promise.fulfill typedFulfiller
    Server.invokeIO
        (Server.toUntypedHandler method)
        (Just (U.PtrStruct (toStruct params)))
        untypedFulfiller

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
    (_, untypedFulfiller) <- liftIO $ atomically $ Promise.newPromiseWithCallback $ \case
        Left e -> Promise.breakPromise pureFulfiller e
        Right v ->
            evalLimitT defaultLimit (fromPtr M.empty v >>= decerialize)
            >>= Promise.fulfill pureFulfiller
    Server.invokeIO
        (Server.toUntypedHandler method)
        (Just (U.PtrStruct struct))
        untypedFulfiller

invokePurePromise
    :: InvokePureCtx m p r
    => Server.MethodHandler m p r
    -> p
    -> m (Promise.Promise r)
invokePurePromise method params = do
    (promise, fulfiller) <- Promise.newPromiseIO
    invokePure method params fulfiller
    pure promise

(?) :: InvokePureCtx m p r
    => Server.MethodHandler m p r
    -> p
    -> m (Promise.Promise r)
(?) = invokePurePromise

-- | Throw an exception with a type field of 'Exception'Type'failed' and
-- the argument as a reason.
throwFailed :: MonadThrow m => Text -> m a
throwFailed reason = throwM (def
    { RpcGen.reason = reason
    , RpcGen.type_ = RpcGen.Exception'Type'failed
} :: RpcGen.Exception)
