{-# LANGUAGE TypeFamilies #-}
{- |
Module: Data.Mutable
Description: Generic support for converting between mutable and immutable values.

There is a common pattern in Haskell libraries that work with mutable data:

* Two types, a mutable and an immutable variant of the same structure.
* @thaw@ and @freeze@ functions to convert between these.
* Sometimes unsafe variants of @thaw@ and @freeze@, which avoid a copy but
  can break referential transparency if misused.

This module abstracts out the above pattern into a generic type family 'Mutable',
and provides some of the common higher-level tools built on top of these
primitives.

Note that there's nothing terribly Cap'N Proto specific about this module; we
may even factor it out into a separate package at some point.
-}
module Data.Mutable where

import Control.Monad.Primitive (PrimMonad, PrimState)

-- | The 'Mutable' type class relates mutable and immutable versions of a type.
-- The instance is defined on the mutable variant; @'Frozen' a@ is the immutable
-- version of a mutable type @a@.
class Mutable a where
    -- | The state token for a mutable value.
    type Scope a

    -- | The immutable version of @a@.
    type Frozen a

    -- | Convert an immutable value to a mutable one.
    thaw :: (PrimMonad m, PrimState m ~ Scope a) => Frozen a -> m a

    -- | Convert a mutable value to an immutable one.
    freeze :: (PrimMonad m, PrimState m ~ Scope a) => a -> m (Frozen a)

    -- | Like 'thaw', except that the caller is responsible for ensuring that
    -- the original value is not subsequently used; doing so may violate
    -- referential transparency.
    --
    -- The default implementation of this is just the same as 'thaw', but
    -- typically an instance will override this with a trivial (unsafe) cast,
    -- hence the obligation described above.
    unsafeThaw :: (PrimMonad m, PrimState m ~ Scope a) => Frozen a -> m a
    unsafeThaw = thaw

    -- | Unsafe version of 'freeze' analagous to 'unsafeThaw'. The caller must
    -- ensure that the original value is not used after this call.
    unsafeFreeze :: (PrimMonad m, PrimState m ~ Scope a) => a -> m (Frozen a)
    unsafeFreeze = freeze
