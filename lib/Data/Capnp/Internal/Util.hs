module Data.Capnp.Internal.Util where

import Control.Monad       (when)
import Control.Monad.Catch (MonadThrow(..))
import Data.Capnp.Errors   (Error(..))

checkIndex :: MonadThrow m => Int -> Int -> m ()
checkIndex i len =
    when (i < 0 || i >= len) $
        throwM BoundsError { index = i, maxIndex = len }
