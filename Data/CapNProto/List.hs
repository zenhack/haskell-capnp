module Data.CapNProto.List where

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM, Exception)
import Prelude hiding (length, lookup)

data List m e = List
    { length :: m Int
    , lookup :: Int -> m e
    }

-- | An exception indicating an out-of-bounds list access.
data BoundsError = BoundsError
    { index :: Int
    , maxIndex :: Int
    } deriving(Show, Eq)

instance Exception BoundsError

-- | @checkBounds i max@ verifies that the index @i@ is legal for a
-- list of length @max@. If not, it throws a BoundsError.
checkBounds :: (Monad m, MonadThrow m) => Int -> Int -> m ()
checkBounds i max = when (i < 0 || i >= max) $ throwM $ BoundsError
    { index = i
    , maxIndex = max
    }

-- | @slice list lo hi@ returns a 'List' containing the elements from @list@
-- at indexes in the range [lo, hi).
slice :: (Monad m, MonadThrow m) => List m e -> Int -> Int -> List m e
slice list lo hi = List { length = return (hi - lo)
                        , lookup = \i -> do
                            checkBounds i (hi - lo)
                            lookup list (i + lo)
                        }
