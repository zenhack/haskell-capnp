module Capnp.Rpc.Breaker
    ( Client(..)
    , Pipeline(..)
    , nullClient
    -- | ** Internals
    , Opaque
    , makeOpaque
    , reflectOpaque
    ) where

import Data.Dynamic (Typeable, Dynamic, toDyn, fromDynamic)

newtype Client = Client Opaque
    deriving(Eq)

instance Show Client where
    show client =
        if client == nullClient then
            "nullClient"
        else
            "({- capability; not statically representable -})"

newtype Pipeline = Pipeline Opaque

nullClient :: Client
nullClient = Client $ makeOpaque ()

data Opaque = Opaque
    { opDyn :: Dynamic
    , opEq :: Opaque -> Bool
    }

makeOpaque :: (Typeable a, Eq a) => a -> Opaque
makeOpaque v = Opaque
    { opDyn = toDyn v
    , opEq = \o -> fromDynamic (opDyn o) == Just v
    }

reflectOpaque :: Opaque -> Dynamic
reflectOpaque = opDyn

instance Eq Opaque where
    x == y = opEq x y
