module Capnp.New.Rpc.Common
    ( Client(..)
    , Pipeline(..)
    ) where

import qualified Internal.Rpc.Breaker as Rpc

-- | A @'Pipeline' a@ is a reference to possibly-not-resolved result from
-- a method call.
newtype Pipeline a = Pipeline Rpc.Pipeline

newtype Client a = Client Rpc.Client
    deriving(Show, Eq)
