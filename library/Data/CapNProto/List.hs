{-| Utility module for working with capnproto lists.

This supplies many familiar operators for working with sequence types.
Unfortunately, because the capnproto readers operate inside of a monadic
context, we can't just implement e.g. Traversable, but this module provides
close analouges to the things allowed by Traversable (and similair type
classes).
-}
module Data.CapNProto.List
    ( ListOf
    , length
    , map
    , mapM_
    , forM_
    , sequence_
    , foldl
    , fold
    )
  where

import Prelude hiding (fold, foldl, length, map, mapM_, sequence_)

import Data.CapNProto.Untyped (ListOf, ReadCtx, index, length)
import Data.Monoid            (mappend, mempty)

-- | Alias for 'fmap'
map :: (a -> c) -> ListOf b a -> ListOf b c
map = fmap

-- | @mapM_ f list@ applies @f@ to each element and then executes the
-- resulting action, in left to right order. This does not allocate
-- space for the intermediate list.
mapM_ :: ReadCtx m b => (a -> c) -> ListOf b a -> m ()
mapM_ = sequence_ . fmap

-- | Like 'mapM_', but with the arguments flipped.
forM_ :: ReadCtx m b => ListOf b a -> (a -> c) -> m ()
forM_ = flip mapM_

-- | @sequence_ list@ executes each of the actions in @list@, from
-- left to right.
sequence_ :: ReadCtx m b => ListOf b (m a) -> m ()
sequence_ = foldl (\m -> (>> void m)) (return ())

-- | Analouge of 'Prelude.foldl'.
foldl :: ReadCtx m b => (c -> a -> c) -> c -> ListOf b a -> m c
foldl f empty list = foldl_ list 0 empty
  where
    foldl_ list start accum
        | length list == start = return accum
        | otherwise =
            foldl_ list (start + 1) =<< (f accum <$> index list start)

-- | Analouge of 'Prelude.fold'.
fold :: (Monoid w, ReadCtx m b) => ListOf b w -> m w
fold = foldl mappend mempty
