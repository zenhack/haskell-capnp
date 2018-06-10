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
    ( MonadLimit(..)
    , LimitT
    , runLimitT
    , evalLimitT
    , execLimitT
    ) where

import Control.Monad              (when)
import Control.Monad.Catch        (MonadThrow(throwM))
import Control.Monad.State.Strict
    (MonadState, StateT, evalStateT, execStateT, get, put, runStateT)
import Control.Monad.Trans.Class  (MonadTrans(lift))
import Data.Capnp.Errors          (Error(TraversalLimitError))

-- Just to define 'MonadLimit' instances:
import Control.Monad.Reader (ReaderT)
import Control.Monad.RWS    (RWST)
import Control.Monad.Writer (WriterT)

import qualified Control.Monad.State.Lazy as LazyState

-- | mtl-style type class to track the traversal limit. This is used
-- by other parts of the library which actually do the reading.
class Monad m => MonadLimit m where
    -- | @'invoice' n@ deducts @n@ from the traversal limit, signaling
    -- an error if the limit is exhausted.
    invoice :: Int -> m ()

-- | Monad transformer implementing 'MonadLimit'. The underlying monad
-- must implement 'MonadThrow', which will be used to signal an error when
-- the limit is exhausted.
newtype LimitT m a = LimitT (StateT Int m a)
    deriving(Functor, Applicative, Monad)

-- | Run a 'LimitT', returning the value from the computation and the remaining
-- traversal limit.
runLimitT :: MonadThrow m => Int -> LimitT m a -> m (a, Int)
runLimitT limit (LimitT stateT) = runStateT stateT limit

-- | Run a 'LimitT', returning the value from the computation.
evalLimitT :: MonadThrow m => Int -> LimitT m a -> m a
evalLimitT limit (LimitT stateT) = evalStateT stateT limit

-- | Run a 'LimitT', returning the remaining traversal limit.
execLimitT :: MonadThrow m => Int -> LimitT m a -> m Int
execLimitT limit (LimitT stateT) = execStateT stateT limit

------ Instances of mtl type classes for 'LimitT'.

instance MonadThrow m => MonadThrow (LimitT m) where
    throwM = lift . throwM

instance MonadThrow m => MonadLimit (LimitT m) where
    invoice deduct = LimitT $ do
        limit <- get
        when (limit < deduct) $ throwM TraversalLimitError
        put (limit - deduct)

instance MonadTrans LimitT where
    lift = LimitT . lift

instance MonadState s m => MonadState s (LimitT m) where
    get = lift get
    put = lift . put

------ Instances of 'MonadLimit' for standard monad transformers

instance MonadLimit m => MonadLimit (StateT s m) where
    invoice = lift . invoice

instance MonadLimit m => MonadLimit (LazyState.StateT s m) where
    invoice = lift . invoice

instance (Monoid w, MonadLimit m) => MonadLimit (WriterT w m) where
    invoice = lift . invoice

instance (MonadLimit m) => MonadLimit (ReaderT r m) where
    invoice = lift . invoice

instance (Monoid w, MonadLimit m) => MonadLimit (RWST r w s m) where
    invoice = lift . invoice
