{-|
Module: Data.CapNProto.Errors
Description: Exception types used by haskell-capnp
-}
module Data.CapNProto.Errors where

import Control.Monad.Catch (Exception)

-- | A @BoundsError@ is an exception indicating an attempt
-- to access an illegal index @index@ within a sequence of length
-- @len@.
data BoundsError = BoundsError
    { index :: Int
    , maxIndex :: Int
    } deriving(Show, Eq)

instance Exception BoundsError

-- | A @RecursionLimitError@ is an exception indicating that
-- the recursion depth limit was exceeded.
data RecursionLimitError
    = RecursionLimitError
    deriving(Show, Eq)

instance Exception RecursionLimitError
