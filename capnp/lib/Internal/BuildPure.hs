{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Internal.BuildPure
-- Description: Helpers for building capnproto messages in pure code.
--
-- This module provides some helpers for building capnproto messages and values
-- in pure code, using the low-level API.
module Internal.BuildPure
  ( PureBuilder,
    createPure,
  )
where

import Capnp.Bits (WordCount)
import Capnp.Mutability
import Capnp.TraversalLimit (LimitT, MonadLimit, evalLimitT)
import Control.Monad.Catch (Exception, MonadThrow (..), SomeException)
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.ST (ST)
import Internal.STE

-- | 'PureBuilder' is a monad transformer stack with the instnaces needed
-- manipulate mutable messages. @'PureBuilder' s a@ is morally equivalent
-- to @'LimitT' ('CatchT' ('ST' s)) a@
newtype PureBuilder s a = PureBuilder (LimitT (STE SomeException s) a)
  deriving (Functor, Applicative, Monad, MonadThrow, MonadLimit)

instance PrimMonad (PureBuilder s) where
  type PrimState (PureBuilder s) = s
  primitive = PureBuilder . primitive

runPureBuilder :: WordCount -> PureBuilder s a -> ST s (Either SomeException a)
runPureBuilder limit (PureBuilder m) = steToST $ evalLimitT limit m

-- | @'createPure' limit m@ creates a capnproto value in pure code according
-- to @m@, then freezes it without copying. If @m@ calls 'throwM' then
-- 'createPure' rethrows the exception in the specified monad.
createPure :: (MonadThrow m, MaybeMutable f) => WordCount -> (forall s. PureBuilder s (f ('Mut s))) -> m (f 'Const)
createPure limit m = throwLeft $ createT (runPureBuilder limit m)
  where
    -- I(zenhack) am surprised not to have found this in one of the various
    -- exception packages:
    throwLeft :: (Exception e, MonadThrow m) => Either e a -> m a
    throwLeft (Left e) = throwM e
    throwLeft (Right a) = pure a
