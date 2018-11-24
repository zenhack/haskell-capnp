{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-|
Module: Internal.Rc
Description: Reference counted boxes.

This module provides a reference-counted cell type 'Rc', which contains a
value and a finalizer. When the reference count reaches zero, the value is
dropped and the finalizer is run.
-}
module Internal.Rc
    ( Rc
    , new
    , get
    , incr
    , decr
    , release
    ) where

import Control.Concurrent.STM

-- | A reference-counted container for a value of type @a@.
newtype Rc a
    = Rc (TVar (Maybe (RcState a)))
    deriving(Eq)

data RcState a = RcState
    { refCount  :: !Int
    , value     :: a
    , finalizer :: STM ()
    }

-- | @'new' val finalizer@ creates a new 'Rc' containing the value @val@, with
-- an initial reference count of 1. When the reference count drops to zero, the
-- finalizer will be run.
new :: a -> STM () -> STM (Rc a)
new value finalizer = fmap Rc $ newTVar $ Just RcState
    { refCount = 1
    , value
    , finalizer
    }

-- | Increment the reference count.
incr :: Rc a -> STM ()
incr (Rc tv) = modifyTVar' tv $
    fmap $ \s@RcState{refCount} -> s { refCount = refCount + 1 }

-- | Decrement the reference count. If this brings the count to zero, run the
-- finalizer and release the value.
decr :: Rc a -> STM ()
decr (Rc tv) = readTVar tv >>= \case
    Nothing ->
        pure ()
    Just RcState{refCount=1, finalizer} -> do
        writeTVar tv Nothing
        finalizer
    Just s@RcState{refCount} ->
        writeTVar tv $ Just s { refCount = refCount - 1 }

-- | Release the value immediately, and run the finalizer, regardless of the
-- current value.
release :: Rc a -> STM ()
release (Rc tv) = readTVar tv >>= \case
    Nothing ->
        pure ()
    Just RcState{finalizer} -> do
        finalizer
        writeTVar tv Nothing

-- | Fetch the value, or 'Nothing' if it has been released.
get :: Rc a -> STM (Maybe a)
get (Rc tv) = fmap value <$> readTVar tv
