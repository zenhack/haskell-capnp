{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
-- | Module: Capnp.Repr.Methods
-- Description: Support for working with methods
module Capnp.Repr.Methods
    ( Method(..)
    , HasMethod(..)

    , Pipeline(..)
    , Client(..)
    , pipe
    , pipelineClient
    , waitPipeline

    , AsClient(..)
    , upcast

    -- * Calling methods.
    , callB
    , callR
    , callP
    ) where

import qualified Capnp.Fields            as F
import           Capnp.Message           (Mutability(..), newMessage)
import qualified Capnp.Message           as M
import qualified Capnp.New.Classes       as NC
import           Capnp.New.Rpc.Common    (Client(..), Pipeline(..))
import qualified Capnp.Repr              as R
import           Capnp.Rpc.Promise       (newPromise)
import qualified Capnp.Rpc.Server        as Server
import qualified Capnp.Rpc.Untyped       as Rpc
import           Capnp.TraversalLimit    (evalLimitT)
import qualified Capnp.Untyped           as U
import           Control.Monad.Catch     (MonadThrow)
import           Control.Monad.STM.Class (MonadSTM(..))
import           Data.Word
import           GHC.OverloadedLabels    (IsLabel(..))
import           GHC.Prim                (coerce)
import           GHC.TypeLits            (Symbol)
import           GHC.Types               (Coercible)
import           Internal.BuildPure      (PureBuilder, createPure)

-- | Represents a method on the interface type @c@ with parameter
-- type @p@ and return type @r@.
data Method c p r = Method
    { interfaceId :: !Word64
    , methodId    :: !Word16
    }

-- | An instance @'HasMethod' name c p r@ indicates that the interface
-- type @c@ has a method named @name@ with parameter type @p@ and
-- return type @r@. The generated code includes instances of this
-- for each method in the schema.
class (R.IsCap c, R.IsStruct p, R.IsStruct r) => HasMethod (name :: Symbol) c p r | name c -> p r where
    methodByLabel :: Method c p r

instance HasMethod name c p r => IsLabel name (Method c p r) where
    fromLabel = methodByLabel @name @c @p @r

-- | The 'AsClient' class allows callers of rpc methods to abstract over 'Client's
-- and 'Pipeline's. @'asClient'@ converts either of those to a client so that
-- methods can be invoked on it.
class AsClient f where
    asClient :: MonadSTM m => R.IsCap c => f c -> m (Client c)

instance AsClient Pipeline where
    asClient = pipelineClient

instance AsClient Client where
    asClient = liftSTM . pure

-- | Upcast is a (safe) cast from an interface to one of its superclasses.
upcast :: (AsClient f, Coercible (f p) (f c), NC.Super p c) => f c -> f p
upcast = coerce

-- | Call a method. Use the provided 'PureBuilder' to construct the parameters.
callB
    :: (AsClient f, R.IsCap c, R.IsStruct p, MonadSTM m)
    => Method c p r
    -> (forall s. PureBuilder s (R.Raw p ('Mut s)))
    -> f c
    -> m (Pipeline r)
callB method buildRaw c = liftSTM $ do
    (params :: R.Raw a 'Const) <- R.Raw <$> createPure maxBound (R.fromRaw <$> buildRaw)
    callR method params c

-- | Call a method, supplying the parameters as a 'Raw' struct.
callR
    :: (AsClient f, R.IsCap c, R.IsStruct p, MonadSTM m)
    => Method c p r -> R.Raw p 'Const -> f c -> m (Pipeline r)
callR Method{interfaceId, methodId} (R.Raw arg) c = liftSTM $ do
    Client client <- asClient c
    (_, f) <- newPromise
    Pipeline <$> Rpc.call
        Server.CallInfo
            { interfaceId
            , methodId
            , arguments = Just (U.PtrStruct arg)
            , response = f
            }
            client

-- | Call a method, supplying the parmaeters in parsed form.
callP
    :: forall c p r f m pp.
        ( AsClient f
        , R.IsCap c
        , R.IsStruct p
        , NC.Parse p pp
        , MonadSTM m
        , MonadThrow m
        )
    => Method c p r -> pp -> f c -> m (Pipeline r)
callP method parsed client = do
    struct <- createPure maxBound $ do
        msg <- newMessage Nothing
        R.fromRaw <$> NC.encode msg parsed
    callR method (R.Raw struct) client

-- | Project a pipeline to a struct onto one of its pointer fields.
pipe :: ( R.IsStruct a
        , R.ReprFor b ~ 'R.Ptr pr
        ) => F.Field k a b -> Pipeline a -> Pipeline b
pipe (F.Field field) (Pipeline p) =
    case field of
        F.GroupField   -> Pipeline p
        F.PtrField idx -> Pipeline (Rpc.walkPipelinePtr p idx)

-- | Convert a 'Pipeline' for a capability into a 'Client'.
pipelineClient :: (R.IsCap a, MonadSTM m) => Pipeline a -> m (Client a)
pipelineClient (Pipeline p) =
    liftSTM $ Client <$> Rpc.pipelineClient p

-- | Wait for the result of a pipeline, and return its value.
waitPipeline ::
    forall a m pr.
    ( 'R.Ptr pr ~ R.ReprFor a
    , R.IsPtrRepr pr
    , MonadSTM m
    ) => Pipeline a -> m (R.Raw a 'Const)
waitPipeline (Pipeline p) =
    -- We need an instance of MonadLimit for IsPtrRepr's ReadCtx requirement,
    -- but none of the relevant instances do a lot of reading, so we just
    -- supply a low-ish arbitrary bound.
    liftSTM $ evalLimitT 100 $ do
        ptr <- Rpc.waitPipeline p
        R.Raw <$> R.fromPtr @pr M.empty ptr

instance R.ReprFor a ~ 'R.Ptr ('Just 'R.Cap) => Rpc.IsClient (Client a) where
    toClient (Client c) = c
    fromClient = Client
