{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}
{- |
Module: Internal.BuildPure
Description: Helpers for building capnproto messages in pure code.

This module provides some helpers for building capnproto messages and values
in pure code, using the low-level API.
-}
module Internal.BuildPure
    ( PureBuilder
    , createPure
    ) where

import Control.Monad.Catch      (MonadThrow(..), SomeException)
import Control.Monad.Catch.Pure (CatchT, runCatchT)
import Control.Monad.Primitive  (PrimMonad(..))
import Control.Monad.ST         (ST)
import Control.Monad.Trans      (MonadTrans(..))

import Capnp.TraversalLimit (LimitT, MonadLimit, evalLimitT)
import Data.Mutable         (Thaw(..), createT)

-- | 'PureBuilder' is a monad transformer stack with the instnaces needed
-- manipulate mutable messages. @'PureBuilder' s a@ is morally equivalent
-- to @'LimitT ('CatchT' ('ST' s)) a@
newtype PureBuilder s a = PureBuilder (LimitT (PrimCatchT (ST s)) a)
    deriving(Functor, Applicative, Monad, MonadThrow, MonadLimit)

instance PrimMonad (PureBuilder s) where
    type PrimState (PureBuilder s) = s
    primitive = PureBuilder . primitive

runPureBuilder :: Int -> PureBuilder s a -> ST s (Either SomeException a)
runPureBuilder limit (PureBuilder m) = runPrimCatchT $ evalLimitT limit m

-- | @'createPure' limit m@ creates a capnproto value in pure code according
-- to @m@, then freezes it without copying. If @m@ calls 'throwM' then
-- 'createPure' returns a 'Left' with the exception.
createPure :: Thaw a => Int -> (forall s. PureBuilder s (Mutable s a)) -> Either SomeException a
createPure limit m = createT (runPureBuilder limit m)

-- | 'PrimCatchT' is a trivial wrapper around 'CatchT', which implements
-- 'PrimMonad'. This is a temporary workaround for:
--
-- https://github.com/ekmett/exceptions/issues/65
--
-- If we can get that issue fixed, we can delete this and just bump the
-- min bound on the exceptions package.
newtype PrimCatchT m a = PrimCatchT (CatchT m a)
    deriving(Functor, Applicative, Monad, MonadThrow)

runPrimCatchT :: Monad m => PrimCatchT m a -> m (Either SomeException a)
runPrimCatchT (PrimCatchT m) = runCatchT m

instance MonadTrans PrimCatchT where
    lift = PrimCatchT . lift

instance PrimMonad m => PrimMonad (PrimCatchT m) where
    type PrimState (PrimCatchT m) = PrimState m
    primitive = lift . primitive
