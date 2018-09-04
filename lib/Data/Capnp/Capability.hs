{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Data.Capnp.Capability where

import Control.Monad.Catch       (MonadThrow(..))
import Control.Monad.Primitive   (PrimMonad(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Word                 (Word32)

-- Just to define 'MonadGetCap' instances:
import Control.Monad.Reader       (ReaderT)
import Control.Monad.RWS          (RWST)
import Control.Monad.State.Strict (MonadState(..), StateT)
import Control.Monad.Writer       (WriterT)

import qualified Control.Monad.State.Lazy as LazyState

import Data.Capnp.TraversalLimit (LimitT, MonadLimit(..))

class Monad m => MonadGetCap m where
    type Capability m

    getCap :: Word32 -> m (Capability m)

newtype NoCapsT m a = NoCapsT { runNoCapsT :: m a }
    deriving(Functor, Applicative, Monad)

instance MonadTrans NoCapsT where
    lift = NoCapsT

instance Monad m => MonadGetCap (NoCapsT m) where
    type Capability (NoCapsT m) = Word32
    getCap = pure

-------- Instances of standard mtl type classes for NoCapsT -------

instance MonadThrow m => MonadThrow (NoCapsT m) where
    throwM = lift . throwM

instance (PrimMonad m, s ~ PrimState m) => PrimMonad (NoCapsT m) where
    type PrimState (NoCapsT m) = PrimState m
    primitive = lift . primitive

-------- Instances for our own mtl-style type classes -------------

instance (MonadLimit m) => MonadLimit (NoCapsT m) where
    invoice = lift . invoice

-------- Instances of MonadGetCap for other monads ---------------

instance MonadGetCap m => MonadGetCap (LimitT m) where
    type Capability (LimitT m) = Capability m
    getCap = lift . getCap

instance MonadGetCap m => MonadGetCap (StateT s m) where
    type Capability (StateT s m) = Capability m
    getCap = lift . getCap

instance MonadGetCap m => MonadGetCap (LazyState.StateT s m) where
    type Capability (LazyState.StateT s m) = Capability m
    getCap = lift . getCap

instance MonadGetCap m => MonadGetCap (ReaderT r m) where
    type Capability (ReaderT r m) = Capability m
    getCap = lift . getCap

instance (Monoid w, MonadGetCap m) => MonadGetCap (WriterT w m) where
    type Capability (WriterT w m) = Capability m
    getCap = lift . getCap

instance (Monoid w, MonadGetCap m) => MonadGetCap (RWST r w s m) where
    type Capability (RWST r w s m) = Capability m
    getCap = lift . getCap
