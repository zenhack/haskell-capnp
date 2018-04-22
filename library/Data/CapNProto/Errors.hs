{-|
Module: Data.CapNProto.Errors
Description: Exception types used by haskell-capnp
-}
module Data.CapNProto.Errors
    ( Error(..)
    )
  where

import Control.Monad.Catch (Exception)

data Error
    -- | A @BoundsError@ is an exception indicating an attempt
    -- to access an illegal index @index@ within a sequence of length
    -- @len@.
    = BoundsError
    { index    :: Int
    , maxIndex :: Int
    }
    -- | A @RecursionLimitError@ is an exception indicating that
    -- the recursion depth limit was exceeded.
    | RecursionLimitError
    -- | An @InvalidDataError@ indicates that a part of a message being
    -- parsed was malformed.
    | InvalidDataError String -- error message
    -- | A @SchemaViolationError@ indicates that part of the message does
    -- not match the schema.
    | SchemaViolationError String
    deriving(Show, Eq)

instance Exception Error
