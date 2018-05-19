{-|
Module: Data.Capnp.Errors
Description: Error handling utilities
-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Capnp.Errors
    ( Error(..)
    )
  where

import Control.Monad.Catch      (Exception)
import Data.Text.Encoding.Error (UnicodeException)

data Error
    -- | A 'BoundsError' indicates an attempt to access an illegal
    -- index 'index' within a sequence of length 'maxIndex'.
    = BoundsError
        { index    :: Int
        , maxIndex :: Int
        }
    -- | A 'RecursionLimitError' indicates that the recursion depth limit
    -- was exceeded.
    | RecursionLimitError
    -- | A 'TraversalLimitError' indicates that the traversal limit was
    -- exceeded.
    | TraversalLimitError
    -- | An 'InvalidDataError' indicates that a part of a message being
    -- parsed was malformed.
    | InvalidDataError String -- error message
    -- | A 'SchemaViolationError' indicates that part of the message does
    -- not match the schema.
    | SchemaViolationError String
    -- | An 'InvalidUtf8Error' indicates that a text value in the message
    -- was invalid utf8.
    --
    -- Note well: Most parts of the library don't actually check for valid
    -- utf8 -- don't assume the check is made unless an interface says it is.
    | InvalidUtf8Error UnicodeException
    deriving(Show, Eq)

instance Exception Error
