{-|
Module: Capnp.Errors
Description: Error handling utilities
-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Capnp.Errors
    ( Error(..)
    )
  where

import Control.Monad.Catch      (Exception)
import Data.Text.Encoding.Error (UnicodeException)

-- | An error that may occur when processing a capnproto message.
data Error
    -- | A 'BoundsError' indicates an attempt to access an illegal
    -- index 'index' within a sequence of length 'maxIndex'.
    = BoundsError
        { index    :: !Int
        , maxIndex :: !Int
        -- TODO: choose a better name than maxIndex; this is confusing
        -- since it's supposed to be the length, rather than the maximum
        -- legal index. The latter would make it impossible to represent
        -- an error for an empty sequence. I(zenhack) also think there may
        -- be places in the library where we are misusing this field.
        }
    -- | A 'RecursionLimitError' indicates that the recursion depth limit
    -- was exceeded.
    | RecursionLimitError
    -- | A 'TraversalLimitError' indicates that the traversal limit was
    -- exceeded.
    | TraversalLimitError
    -- | An 'InvalidDataError' indicates that a part of a message being
    -- parsed was malformed. The argument to the data constructor is a
    -- human-readable error message.
    | InvalidDataError String
    -- | A 'SizeError' indicates that an operation would have resulted in
    -- a message that violated the library's limit on either segment size
    -- or number of segments.
    | SizeError
    -- | A 'SchemaViolationError' indicates that part of the message does
    -- not match the schema. The argument to the data construtor is a
    -- human-readable error message.
    | SchemaViolationError String
    -- | An 'InvalidUtf8Error' indicates that a text value in the message
    -- was invalid utf8.
    --
    -- Note well: Most parts of the library don't actually check for valid
    -- utf8 -- don't assume the check is made unless an interface says it is.
    | InvalidUtf8Error UnicodeException
    deriving(Show, Eq)

instance Exception Error
