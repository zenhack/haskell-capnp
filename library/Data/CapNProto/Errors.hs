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
    { index    :: Int
    , maxIndex :: Int
    } deriving(Show, Eq)

instance Exception BoundsError

-- | A @RecursionLimitError@ is an exception indicating that
-- the recursion depth limit was exceeded.
data RecursionLimitError
    = RecursionLimitError
    deriving(Show, Eq)

instance Exception RecursionLimitError

-- | An @InvalidDataError@ indicates that a part of a message being
-- parsed was malformed.
newtype InvalidDataError
    = InvalidDataError String -- error message
    deriving(Show, Eq)

instance Exception InvalidDataError

-- | A @SchemaViolationError@ indicates that part of the message does
-- not match the schema.

newtype SchemaViolationError
    = SchemaViolationError String
    deriving(Show, Eq)

instance Exception SchemaViolationError
