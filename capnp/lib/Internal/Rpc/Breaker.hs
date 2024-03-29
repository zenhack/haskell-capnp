-- | Module: Internal.Rpc.Breaker
--
-- This module serves to break a dependency cycle between the rpc
-- system and the serialization code; see Note [Breaker] in
-- "Capnp.Rpc.Untyped" for details.
module Internal.Rpc.Breaker
  ( Client (..),
    Pipeline (..),
    nullClient,
    invalidClient,
    -- | ** Internals
    Opaque,
    makeOpaque,
    reflectOpaque,
  )
where

import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)

-- | A reference to a capability, which may be live either in the current vat
-- or elsewhere. Holding a client affords making method calls on a capability
-- or modifying the local vat's reference count to it.
newtype Client = Client Opaque
  deriving (Eq)

instance Show Client where
  show client@(Client opaque) =
    if client == nullClient
      then "nullClient"
      else case fromDynamic (reflectOpaque opaque) of
        Just (InvalidClient errMsg) -> "(invalidClient" ++ show errMsg ++ ")"
        Nothing -> "({- capability; not statically representable -})"

-- | A 'Pipeline' is a reference to a value within a message that has not yet arrived.
newtype Pipeline = Pipeline Opaque

-- | A null client. This is the only client value that can be represented
-- statically. Throws exceptions in response to all method calls.
nullClient :: Client
nullClient = Client $ makeOpaque ()

newtype InvalidClient = InvalidClient String
  deriving (Show, Read, Eq, Typeable)

-- | Returns a client which is "invalid;" it behaves like 'nullClient',
-- but can be given a custom error message that is displayed by 'show'.
invalidClient :: String -> Client
invalidClient = Client . makeOpaque . InvalidClient

data Opaque = Opaque
  { opDyn :: Dynamic,
    opEq :: Opaque -> Bool
  }

makeOpaque :: (Typeable a, Eq a) => a -> Opaque
makeOpaque v =
  Opaque
    { opDyn = toDyn v,
      opEq = \o -> fromDynamic (opDyn o) == Just v
    }

reflectOpaque :: Opaque -> Dynamic
reflectOpaque = opDyn

instance Eq Opaque where
  x == y = opEq x y
