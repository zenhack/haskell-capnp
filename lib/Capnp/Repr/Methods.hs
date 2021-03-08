{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
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
    ) where

import qualified Capnp.Fields            as F
import           Capnp.Message           (Mutability(..))
import qualified Capnp.Repr              as R
import           Capnp.Rpc.Promise       (newPromise)
import qualified Capnp.Rpc.Server        as Server
import qualified Capnp.Rpc.Untyped       as Rpc
import qualified Capnp.Untyped           as U
import           Control.Concurrent.STM
import           Control.Monad.STM.Class (MonadSTM(..))
import           Data.Word
import           GHC.OverloadedLabels    (IsLabel)

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
class
    ( R.ReprFor c ~ 'R.Ptr ('Just 'R.Cap)
    , IsLabel name (Method c p r)
    ) => HasMethod name c p r | name c -> p r

newtype Pipeline a = Pipeline Rpc.Pipeline

newtype Client a = Client Rpc.Client

class AsClient f where
    asClient :: R.ReprFor c ~ 'R.Ptr ('Just 'R.Cap) => f c -> STM (Client c)

instance AsClient Pipeline where
    asClient = pipelineClient

instance AsClient Client where
    asClient = pure

callR ::
    ( AsClient f
    , R.ReprFor c ~ 'R.Ptr ('Just 'R.Cap)
    , R.ReprFor p ~ 'R.Ptr ('Just 'R.Struct)
    , MonadSTM m
    ) => Method c p r -> R.Raw 'Const p -> f c -> m (Pipeline r)
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

pipe :: ( R.ReprFor a ~ 'R.Ptr ('Just 'R.Struct)
        , R.ReprFor b ~ 'R.Ptr pr
        ) => F.Field k a b -> Pipeline a -> Pipeline b
pipe (F.Field field) (Pipeline p) =
    case field of
        F.GroupField   -> Pipeline p
        F.PtrField idx -> Pipeline (Rpc.walkPipelinePtr p idx)

pipelineClient :: (R.ReprFor a ~ 'R.Ptr ('Just 'R.Cap), MonadSTM m) => Pipeline a -> m (Client a)
pipelineClient (Pipeline p) =
    liftSTM $ Client <$> Rpc.pipelineClient p

instance R.ReprFor a ~ 'R.Ptr ('Just 'R.Cap) => Rpc.IsClient (Client a) where
    toClient (Client c) = c
    fromClient = Client
