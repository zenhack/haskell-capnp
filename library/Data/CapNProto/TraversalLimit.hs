{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.CapNProto.TraversalLimit
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
import Data.CapNProto.Errors
    (Error(TraversalLimitError), ThrowError(throwError))

-- Just to define Limit instances:
import           Control.Monad.Reader     (ReaderT)
import           Control.Monad.RWS        (RWST)
import qualified Control.Monad.State.Lazy as LazyState
import           Control.Monad.Writer     (WriterT)

class Limit m where
    invoice :: Int -> m ()


newtype LimitT m a = LimitT { runLimitT :: StateT Int m a }
    deriving(Functor, Applicative, Monad)

runWithLimit :: (Monad m, ThrowError m) => Int -> LimitT m a -> m (a, Int)
runWithLimit limit (LimitT stateT) = runStateT stateT limit

evalWithLimit :: (Monad m, ThrowError m) => Int -> LimitT m a -> m a
evalWithLimit limit (LimitT stateT) = evalStateT stateT limit

execWithLimit :: (Monad m, ThrowError m) => Int -> LimitT m a -> m Int
execWithLimit limit (LimitT stateT) = execStateT stateT limit

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

------ # Instances of Limit for standard monad transformers

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
