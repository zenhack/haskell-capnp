{-|
Module: Internal.Util
Description: Misc. helpers.

-}
module Internal.Util where

import Capnp.Errors        (Error(..))
import Control.Monad       (when)
import Control.Monad.Catch (MonadThrow(..))

-- | @'checkIndex' index length@ checkes that 'index' is in the range
-- [0, length), throwing a 'BoundsError' if not.
checkIndex :: MonadThrow m => Int -> Int -> m ()
checkIndex i len =
    when (i < 0 || i >= len) $
        throwM BoundsError { index = i, maxIndex = len }
