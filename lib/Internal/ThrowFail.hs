{-|
Module: Internal.ThrowFail
Description: Implement 'MonadThrow' on top of 'fail'.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Internal.ThrowFail (ThrowFailT(..)) where

import Control.Monad.Catch       (MonadThrow(..))
import Control.Monad.Fail        (MonadFail)
import Control.Monad.Trans.Class (MonadTrans(..))

-- | 'ThrowFailT' is a monad transformer which implements 'MonadThrow' on
-- top of 'MonadFail'. The 'throwM' method is eqivalent to @'fail' . 'show'@.
newtype ThrowFailT m a = ThrowFailT { runThrowFailT :: m a }
    deriving(Functor, Applicative, Monad)

instance MonadTrans ThrowFailT where
    lift = ThrowFailT

instance MonadFail m => MonadThrow (ThrowFailT m) where
    throwM = lift . fail . show
