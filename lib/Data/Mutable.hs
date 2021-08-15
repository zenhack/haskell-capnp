{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module: Data.Mutable
Description: Generic support for converting between mutable and immutable values.

There is a common pattern in Haskell libraries that work with mutable data:

* Two types, a mutable and an immutable variant of the same structure.
* @thaw@ and @freeze@ functions to convert between these.
* Sometimes unsafe variants of @thaw@ and @freeze@, which avoid a copy but
  can break referential transparency if misused.

This module abstracts out the above pattern into a generic type family 'Thaw',
and provides some of the common higher-level tools built on top of these
primitives.

Note that there's nothing terribly Cap'N Proto specific about this module; we
may even factor it out into a separate package at some point.
-}
module Data.Mutable {-# DEPRECATED "use Capnp.Mutability instead" #-} where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST        (ST, runST)

-- | The 'Thaw' type class relates mutable and immutable versions of a type.
-- The instance is defined on the immutable variant; @'Mutable' s a@ is the
-- mutable version of an immutable type @a@, bound to the state token @s@.
class Thaw a where
    -- | The mutable version of @a@, bound to the state token @s@.
    type Mutable s a

    -- | Convert an immutable value to a mutable one.
    thaw :: (PrimMonad m, PrimState m ~ s) => a -> m (Mutable s a)

    -- | Convert a mutable value to an immutable one.
    freeze :: (PrimMonad m, PrimState m ~ s) => Mutable s a -> m a

    -- | Like 'thaw', except that the caller is responsible for ensuring that
    -- the original value is not subsequently used; doing so may violate
    -- referential transparency.
    --
    -- The default implementation of this is just the same as 'thaw', but
    -- typically an instance will override this with a trivial (unsafe) cast,
    -- hence the obligation described above.
    unsafeThaw :: (PrimMonad m, PrimState m ~ s) => a -> m (Mutable s a)
    unsafeThaw = thaw

    -- | Unsafe version of 'freeze' analagous to 'unsafeThaw'. The caller must
    -- ensure that the original value is not used after this call.
    unsafeFreeze :: (PrimMonad m, PrimState m ~ s) => Mutable s a -> m a
    unsafeFreeze = freeze

-- | Create and freeze a mutable value, safely, without doing a full copy.
-- internally, 'create' calls unsafeFreeze, but it cannot be directly used to
-- violate referential transparency, as the value is not available to the
-- caller after freezing.
create :: Thaw a => (forall s. ST s (Mutable s a)) -> a
create st = runST (st >>= unsafeFreeze)

-- | Like 'create', but the result is wrapped in an instance of 'Traversable'.
createT :: (Traversable f, Thaw a) => (forall s. ST s (f (Mutable s a))) -> f a
createT st = runST (st >>= traverse unsafeFreeze)
