{-|
Module: Data.Capnp.Errors
Description: Error handling utilities
-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Capnp.Errors
    ( Error(..)
    , ThrowError(..)
    )
  where

import Control.Monad.Catch (Exception, MonadThrow(throwM))

import Control.Monad.Trans.Class (lift)

import Data.Text.Encoding.Error (UnicodeException)

-- Just so we can define ThrowError instances:
import           Control.Monad.Catch.Pure   (CatchT)
import           Control.Monad.Identity     (IdentityT)
import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.RWS          (RWST)
import qualified Control.Monad.State.Lazy   as LazyState
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Writer       (WriterT)
import           GHC.Conc                   (STM)

-- | 'ThrowError' is similar 'MonadThrow', but:
--
-- 1. Only requires 'Applicative'.
-- 2. Doesn't allow general exceptions, just the 'Error' type defined in
--    this module.
-- 3. Doesn't necessarily enforce evaluation order to the same extent;
--    while @'throwM' e >> m@ is required to be equivalent to @'throwM' e@,
--    it is also legal for @'throwError' e *> m@ to be equivalent to @m@.
class Applicative f => ThrowError f where
    throwError :: Error -> f a
    default throwError :: MonadThrow f => Error -> f a
    throwError = throwM

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

-- ThrowError instances for many common library types. TODO: round all this
-- out with instances for all the sensible things in the libraries that we
-- already pull in as dependencies.

instance ThrowError IO
instance ThrowError []
instance ThrowError Maybe
instance ThrowError STM
instance (Monad m) => ThrowError (CatchT m)

instance ThrowError (Either Error) where
    throwError = Left

instance (Monad m, ThrowError m) => ThrowError (IdentityT m) where
    throwError = lift . throwError
instance (Monad m, ThrowError m) => ThrowError (ReaderT r m) where
    throwError = lift . throwError
instance (Monad m, ThrowError m, Monoid w) => ThrowError (RWST r w s m) where
    throwError = lift . throwError
instance (Monad m, ThrowError m) => ThrowError (StateT s m) where
    throwError = lift . throwError
instance (Monad m, ThrowError m) => ThrowError (LazyState.StateT s m) where
    throwError = lift . throwError
instance (Monad m, ThrowError m, Monoid w) => ThrowError (WriterT w m) where
    throwError = lift . throwError
