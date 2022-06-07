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

-- | A reference to a capability, which may be live either in the current vat
-- or elsewhere. Holding a client affords making method calls on a capability
-- or modifying the local vat's reference count to it.
newtype Client = Client Opaque
    deriving(Eq)

instance Show Client where
    show client =
        if client == nullClient then
            "nullClient"
        else
            "({- capability; not statically representable -})"

-- | A 'Pipeline' is a reference to a value within a message that has not yet arrived.
newtype Pipeline = Pipeline Opaque

-- | A null client. This is the only client value that can be represented
-- statically. Throws exceptions in response to all method calls.
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
