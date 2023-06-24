{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Internal.SnocList
  ( SnocList,
    fromList,
    empty,
    snoc,
    singleton,
  )
where

import Data.Foldable
import Data.Hashable (Hashable)

-- | A 'SnocList' is just a list, but with efficient appending instead of
-- prepending. The indended use case is when you want to append a bunch of
-- things to a list, and then get the final list to work with.
--
-- A standard trick for this is to cons each element onto the *front* of
-- the list, and then reverse the list before processing. A SnocList
-- just packages up this trick so you can't forget to do the reverse.
newtype SnocList a = SnocList [a]
  deriving (Eq, Hashable)

-- | Convert a list to a 'SnocList'. O(n)
fromList :: [a] -> SnocList a
fromList = SnocList . reverse

-- | A one-element 'SnocList'.
singleton :: a -> SnocList a
singleton x = SnocList [x]

-- | The empty 'SnocList'.
empty :: SnocList a
empty = SnocList []

-- | Append a value to the 'SnocList'. A note on the name: 'snoc' is @cons@
-- backwards.
snoc :: SnocList a -> a -> SnocList a
snoc (SnocList xs) x = SnocList (x : xs)

instance Foldable SnocList where
  foldMap f = foldMap f . toList
  toList (SnocList xs) = reverse xs

instance Semigroup (SnocList a) where
  (SnocList l) <> (SnocList r) = SnocList (r <> l)

instance Monoid (SnocList a) where
  mempty = empty
