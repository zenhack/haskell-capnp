{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}
module Capnp.Mutability
    ( Mutability(..)
    , MaybeMutable(..)
    , create
    , createT
    ) where

import Control.Monad.Primitive (PrimMonad(PrimState))
import Control.Monad.ST        (ST, runST)
import Data.Kind               (Type)

-- | 'Mutability' is used as a type parameter (with the DataKinds extension)
-- to indicate the mutability of some values in this library; 'Const' denotes
-- an immutable value, while @'Mut' s@ denotes a value that can be mutated
-- in the scope of the state token @s@.
data Mutability = Const | Mut Type

-- | 'MaybeMutable' relates mutable and immutable versions of a type.
class MaybeMutable (f :: Mutability -> *) where
    -- | Convert an immutable value to a mutable one.
    thaw :: (PrimMonad m, PrimState m ~ s) => f 'Const -> m (f ('Mut s))

    -- | Convert a mutable value to an immutable one.
    freeze :: (PrimMonad m, PrimState m ~ s) => f ('Mut s) -> m (f 'Const)

    -- | Like 'thaw', except that the caller is responsible for ensuring that
    -- the original value is not subsequently used; doing so may violate
    -- referential transparency.
    --
    -- The default implementation of this is just the same as 'thaw', but
    -- typically an instance will override this with a trivial (unsafe) cast,
    -- hence the obligation described above.
    unsafeThaw :: (PrimMonad m, PrimState m ~ s) => f 'Const -> m (f ('Mut s))
    unsafeThaw = thaw

    -- | Unsafe version of 'freeze' analagous to 'unsafeThaw'. The caller must
    -- ensure that the original value is not used after this call.
    unsafeFreeze :: (PrimMonad m, PrimState m ~ s) => f ('Mut s) -> m (f 'Const)
    unsafeFreeze = freeze

-- | Create and freeze a mutable value, safely, without doing a full copy.
-- internally, 'create' calls unsafeFreeze, but it cannot be directly used to
-- violate referential transparency, as the value is not available to the
-- caller after freezing.
create :: MaybeMutable f => (forall s. ST s (f ('Mut s))) -> f 'Const
create st = runST (st >>= unsafeFreeze)

-- | Like 'create', but the result is wrapped in an instance of 'Traversable'.
createT :: (Traversable t, MaybeMutable f) => (forall s. ST s (t (f ('Mut s)))) -> t (f 'Const)
createT st = runST (st >>= traverse unsafeFreeze)
