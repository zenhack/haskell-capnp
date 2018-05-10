{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-| Support for managing message traversal limits.

This module is used to mitigate several pitfalls with the capnproto format,
which could potentially lead to denial of service vulnerabilities.

In particular, while they are illegal according to the spec, it is possible to
encode objects which have many pointers pointing the same place, or even
cycles. A naive traversal therefore could involve quite a lot of computation
for a message that is very small on the wire.

Accordingly, most implementations of the format keep track of how many bytes
of a message have been accessed, and start signaling errors after a certain
value (the "traversal limit") has been reached. The Haskell implementation is
no exception; this module implements that logic. We provide a monad
transformer and mtl-style type class to track the limit; reading from the
message happens inside of this monad.

-}
module Data.Capnp.TraversalLimit
    ( Limit(..)
    , LimitT
    , runWithLimit
    , evalWithLimit
    , execWithLimit
    ) where

import Control.Monad              (when)
import Control.Monad.Catch        (MonadThrow(throwM))
import Control.Monad.State.Strict
    (MonadState, StateT, evalStateT, execStateT, get, put, runStateT)
import Control.Monad.Trans.Class  (MonadTrans(lift))
import Data.Capnp.Errors
    (Error(TraversalLimitError), ThrowError(throwError))

-- Just to define Limit instances:
import           Control.Monad.Reader     (ReaderT)
import           Control.Monad.RWS        (RWST)
import qualified Control.Monad.State.Lazy as LazyState
import           Control.Monad.Writer     (WriterT)

-- | mtl-style type class to track the traversal limit. This is used
-- by other parts of the library which actually do the reading.
--
-- Note that, deviating from the standard mtl type classes, there is no
-- Monad constraint. The motivations are similar to 'ThrowError': We
-- may at some point develop an instance of this class that allows
-- parrallel or non-deterministic exploration of a message, and only
-- 'Applicative' is really needed.
class Limit m where
    -- | @'invoice' n@ deducts @n@ from the traversal limit, signalling
    -- an error if the limit is exhausted.
    invoice :: Int -> m ()

-- | Monad transformer implementing 'Limit'. The underlying monad must
-- implement 'ThrowError', which will be used to signal an error when
-- the limit is exhausted.
newtype LimitT m a = LimitT { runLimitT :: StateT Int m a }
    deriving(Functor, Applicative, Monad)

-- Run a LimitT, returning the value from the computation and the remaining
-- traversal limit.
runWithLimit :: (Monad m, ThrowError m) => Int -> LimitT m a -> m (a, Int)
runWithLimit limit (LimitT stateT) = runStateT stateT limit

-- Run a LimitT, returning the value from the computation.
evalWithLimit :: (Monad m, ThrowError m) => Int -> LimitT m a -> m a
evalWithLimit limit (LimitT stateT) = evalStateT stateT limit

-- Run a LimitT, returning the remaining traversal limit.
execWithLimit :: (Monad m, ThrowError m) => Int -> LimitT m a -> m Int
execWithLimit limit (LimitT stateT) = execStateT stateT limit

------ Instances of mtl type classes for LimitT.

instance MonadThrow m => MonadThrow (LimitT m) where
    throwM = lift . throwM

instance (Monad m, ThrowError m) => ThrowError (LimitT m) where
    throwError = lift . throwError

instance (Monad m, ThrowError m) => Limit (LimitT m) where
    invoice deduct = LimitT $ do
        limit <- get
        when (limit < deduct) $ throwError TraversalLimitError
        put (limit - deduct)

instance MonadTrans LimitT where
    lift = LimitT . lift

instance MonadState s m => MonadState s (LimitT m) where
    get = lift get
    put = lift . put

------ Instances of Limit for standard monad transformers

instance (Monad m, Limit m) => Limit (StateT s m) where
    invoice = lift . invoice

instance (Monad m, Limit m) => Limit (LazyState.StateT s m) where
    invoice = lift . invoice

instance (Monoid w, Monad m, Limit m) => Limit (WriterT w m) where
    invoice = lift . invoice

instance (Monad m, Limit m) => Limit (ReaderT r m) where
    invoice = lift . invoice

instance (Monoid w, Monad m, Limit m) => Limit (RWST r w s m) where
    invoice = lift . invoice
