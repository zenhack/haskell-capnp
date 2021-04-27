{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
-- | Module: Capnp.Repr.Methods
-- Description: Support for working with methods
module Capnp.Repr.Methods
    ( Method(..)
    , HasMethod

    , Pipeline(..)
    , Client(..)
    , pipe
    , pipelineClient

    , AsClient(..)

    , callR
    , callP
    ) where

import qualified Capnp.Fields            as F
import           Capnp.Message           (Mutability(..), newMessage)
import qualified Capnp.New.Classes       as NC
import qualified Capnp.Repr              as R
import           Capnp.Rpc.Promise       (newPromise)
import qualified Capnp.Rpc.Server        as Server
import qualified Capnp.Rpc.Untyped       as Rpc
import qualified Capnp.Untyped           as U
import           Control.Concurrent.STM
import           Control.Monad.Catch     (MonadThrow)
import           Control.Monad.STM.Class (MonadSTM(..))
import           Data.Word
import           GHC.OverloadedLabels    (IsLabel(..))
import           GHC.TypeLits            (Symbol)
import           Internal.BuildPure      (createPure)

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

newtype Pipeline a = Pipeline Rpc.Pipeline

newtype Client a = Client Rpc.Client

class AsClient f where
    asClient :: R.IsCap c => f c -> STM (Client c)

instance AsClient Pipeline where
    asClient = pipelineClient

instance AsClient Client where
    asClient = pure

callR
    :: (AsClient f, R.IsCap c, R.IsStruct p, MonadSTM m)
    => Method c p r -> R.Raw 'Const p -> f c -> m (Pipeline r)
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

pipe :: ( R.IsStruct a
        , R.ReprFor b ~ 'R.Ptr pr
        ) => F.Field k a b -> Pipeline a -> Pipeline b
pipe (F.Field field) (Pipeline p) =
    case field of
        F.GroupField   -> Pipeline p
        F.PtrField idx -> Pipeline (Rpc.walkPipelinePtr p idx)

pipelineClient :: (R.IsCap a, MonadSTM m) => Pipeline a -> m (Client a)
pipelineClient (Pipeline p) =
    liftSTM $ Client <$> Rpc.pipelineClient p

instance R.ReprFor a ~ 'R.Ptr ('Just 'R.Cap) => Rpc.IsClient (Client a) where
    toClient (Client c) = c
    fromClient = Client
