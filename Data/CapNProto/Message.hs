{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    RecordWildCards #-}
{-|
Module: Data.CapNProto.Message
Description: Tools for working with messages.

-}
module Data.CapNProto.Message where

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Quota (MonadQuota, invoice)
import Data.CapNProto.Address (WordAddr(..))
import Data.CapNProto.Errors (BoundsError(..))
import Data.Word (Word64)
import Prelude hiding (length, lookup)

-- | An array-like data structure.
class Array a e | a -> e where
    -- | Return the length of the array
    length :: a -> Int
    -- | @lookup i arr@ retrieves the element at index @i@ in @arr@.
    -- if @i@ is out of bounds, a @BoundsError@ will be thrown.
    lookup :: (MonadThrow m) => Int -> a -> m e

-- | A CapNProto message
class (Array msg seg, Array seg Word64) => Message msg seg

-- | @checkBounds arr i@ verifies that @i@ is a legal index into @arr@,
-- calling throwing a @BoundsError@ if not.
checkBounds :: (MonadThrow m, Array a e) => a -> Int -> m ()
checkBounds arr i = when (i < 0 || i >= length arr) $
    throwM $ BoundsError { index = i, maxIndex = length arr }

-- | @getWord addr@ returns the word at @addr@ within @msg@. It deducts
-- 1 from the quota, and throws a @BoundsError@ if the address is out of
-- bounds.
getWord :: (Message msg seg, MonadThrow m, MonadQuota m)
    => WordAddr -> msg -> m Word64
getWord WordAt{..} msg = do
    invoice 1
    seg <- lookup segIndex msg
    lookup wordIndex seg
