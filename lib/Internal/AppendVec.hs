{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Internal.AppendVec
-- Description: Helpers for efficient appending to vectors.
module Internal.AppendVec
  ( AppendVec,
    fromVector,
    makeEmpty,
    getVector,
    getCapacity,
    grow,
    canGrowWithoutCopy,
  )
where

import Capnp.Errors (Error (SizeError))
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Generic.Mutable as GMV

-- | 'AppendVec' wraps a mutable vector, and affords amortized O(1) appending.
data AppendVec v s a = AppendVec
  { mutVec :: v s a,
    mutVecLen :: !Int
  }

-- | 'fromVector' wraps a mutable vector in an appendVector, with no initial
-- spare capacity.
fromVector :: GMV.MVector v a => v s a -> AppendVec v s a
fromVector vec =
  AppendVec
    { mutVec = vec,
      mutVecLen = GMV.length vec
    }

-- | 'makeEmpty' makes an initially empty 'AppendVec', using the argument
-- as allocation space for 'grow'.
makeEmpty :: GMV.MVector v a => v s a -> AppendVec v s a
makeEmpty vec =
  AppendVec
    { mutVec = vec,
      mutVecLen = 0
    }

-- | 'getVector' returns the valid portion of the underlying mutable vector.
getVector :: GMV.MVector v a => AppendVec v s a -> v s a
getVector AppendVec {mutVec, mutVecLen} = GMV.slice 0 mutVecLen mutVec

getCapacity :: GMV.MVector v a => AppendVec v s a -> Int
getCapacity AppendVec {mutVec} = GMV.length mutVec

-- | @'grow' vec amount maxSize@ grows the vector @vec@ by @amount@ elements,
-- provided the result does not exceed @maxSize@. Amortized O(@amount@). Returns
-- the new vector; the original should not be used.
-- .
-- If the result does exceed @maxSize@, throws 'SizeError'.
grow ::
  (MonadThrow m, PrimMonad m, s ~ PrimState m, GMV.MVector v a) =>
  AppendVec v s a ->
  Int ->
  Int ->
  m (AppendVec v s a)
grow vec@AppendVec {mutVec, mutVecLen} amount maxSize = do
  when (maxSize - amount < mutVecLen) $
    throwM SizeError
  mutVec <-
    if canGrowWithoutCopy vec amount
      then -- we have enough un-allocated space already; leave the vector
      -- itself alone.
        pure mutVec
      else -- Allocate some more space. we at least double the underlying
      -- vector's size, to make appending amortized O(1), but if the
      -- vector is small enough and the allocation is big enough, we
      -- may need to do more to satisfy the request:
        GMV.grow mutVec (max amount (mutVecLen * 2))
  pure
    AppendVec
      { mutVec = mutVec,
        mutVecLen = mutVecLen + amount
      }

canGrowWithoutCopy :: (GMV.MVector v a) => AppendVec v s a -> Int -> Bool
canGrowWithoutCopy AppendVec {mutVec, mutVecLen} amount =
  mutVecLen + amount <= GMV.length mutVec
