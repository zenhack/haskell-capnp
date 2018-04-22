{-|
Module: Data.CapNProto.Errors
Description: Exception types used by haskell-capnp
-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.CapNProto.Errors
    ( Error(..)
    , ThrowError(..)
    )
  where

import Control.Monad.Catch (Exception, MonadThrow(throwM))

-- | Similar MonadThrow, but:
--
-- 1. Only require Applicative.
-- 2. Don't allow general exceptions, just the 'Error' type defined in
--    this module.
-- 3. Don't necessarily enforce evaluation order to the same extent;
--    while @throwM e >> m@ is required to be equivalent to @throwM e@,
--    it is legal (though not required) for e.g:
--
--      @snd <$> ((,) <$> throwError e <*> pure 4@
--
-- to be equivalnet to @pure 4@.
class Applicative f => ThrowError f where
    throwError :: Error -> f a

-- I don't understand why GHC can't deduce Applicative from MonadThrow,
-- but it's giving me an error when I try to leave it out of the context.
instance (Applicative m, MonadThrow m) => ThrowError m where
    throwError = throwM

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
