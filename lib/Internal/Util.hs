{-|
Module: Internal.Util
Description: Misc. helpers.

-}
module Internal.Util where

import Control.Monad       (when)
import Control.Monad.Catch (MonadThrow(..))
import Data.Capnp.Errors   (Error(..))

-- | @'checkIndex' index length@ checkes that 'index' is in the range
-- [0, length), throwing a 'BoundsError' if not.
checkIndex :: MonadThrow m => Int -> Int -> m ()
checkIndex i len =
    when (i < 0 || i >= len) $
        throwM BoundsError { index = i, maxIndex = len }
