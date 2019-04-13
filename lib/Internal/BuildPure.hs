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

import Control.Monad.Catch      (Exception, MonadThrow(..), SomeException)
import Control.Monad.Catch.Pure (CatchT, runCatchT)
import Control.Monad.Primitive  (PrimMonad(..))
import Control.Monad.ST         (ST)
import Control.Monad.Trans      (MonadTrans(..))

import Capnp.Bits           (WordCount)
import Capnp.TraversalLimit (LimitT, MonadLimit, evalLimitT)
import Data.Mutable         (Thaw(..), createT)

-- | 'PureBuilder' is a monad transformer stack with the instnaces needed
-- manipulate mutable messages. @'PureBuilder' s a@ is morally equivalent
-- to @'LimitT' ('CatchT' ('ST' s)) a@
newtype PureBuilder s a = PureBuilder (LimitT (PrimCatchT (ST s)) a)
    deriving(Functor, Applicative, Monad, MonadThrow, MonadLimit)

instance PrimMonad (PureBuilder s) where
    type PrimState (PureBuilder s) = s
    primitive = PureBuilder . primitive

runPureBuilder :: WordCount -> PureBuilder s a -> ST s (Either SomeException a)
runPureBuilder limit (PureBuilder m) = runPrimCatchT $ evalLimitT limit m

-- | @'createPure' limit m@ creates a capnproto value in pure code according
-- to @m@, then freezes it without copying. If @m@ calls 'throwM' then
-- 'createPure' rethrows the exception in the specified monad.
createPure :: (MonadThrow m, Thaw a) => WordCount -> (forall s. PureBuilder s (Mutable s a)) -> m a
createPure limit m = throwLeft $ createT (runPureBuilder limit m)
  where
    -- I(zenhack) am surprised not to have found this in one of the various
    -- exception packages:
    throwLeft :: (Exception e, MonadThrow m) => Either e a -> m a
    throwLeft (Left e)  = throwM e
    throwLeft (Right a) = pure a

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
