{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-|
Module: Data.CapNProto.Message
Description: Tools for working with messages.

-}
module Data.CapNProto.Message where

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.CapNProto.Errors (BoundsError(..))
import Data.Word (Word64)
import Prelude hiding (length, lookup)

-- | An array-like data structure.
class Array a e | a -> e where
    length :: a -> Int
    lookup :: (MonadThrow m) => Int -> a -> m e

-- | A CapNProto message
class (Array msg seg, Array seg Word64) => Message msg seg

-- | @checkBounds arr i@ verifies that @i@ is a legal index into @arr@,
-- calling throwing a @BoundsError@ if not.
checkBounds :: (MonadThrow m, Array a e) => a -> Int -> m ()
checkBounds arr i = when (i < 0 || i >= length arr) $
    throwM $ BoundsError { index = i, maxIndex = length arr }
