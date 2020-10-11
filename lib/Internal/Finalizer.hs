{-|
Module: Internal.Finalizer
Description: Make resource-safe wrappers for values, with finalizers

This module wrappers for values to which finalizers can safely be
attached, without worrying that they may be collected early. It is
useful when the natural thing to attach a finalizer to is a simple
datatype.

From the docs for the 'Weak' type:

> WARNING: weak pointers to ordinary non-primitive Haskell types
> are particularly fragile, because the compiler is free to optimise
> away or duplicate the underlying data structure. Therefore
> attempting to place a finalizer on an ordinary Haskell type may
> well result in the finalizer running earlier than you expected.
>
> [...]
>
> Finalizers can be used reliably for types that are created
> explicitly and have identity, such as IORef and MVar. [...]

So instead, we provide a 'Cell' type, which:

* Wraps simple value
* Can be created inside STM, and
* May safely have finalizers, using the 'addFinalizer' function in
  this module.

Note that it is *not* safe to use the primitives from "Sys.Mem.Weak" to
add finalizers.

-}
{-# LANGUAGE NamedFieldPuns #-}
module Internal.Finalizer (Cell, with, newCell, addFinalizer) where

import Control.Concurrent.MVar (MVar, mkWeakMVar, newEmptyMVar)
import Control.Concurrent.STM  (STM, TVar, atomically, modifyTVar', newTVar)
import Control.Monad.Primitive (touch)
import GHC.Conc (unsafeIOToSTM)

-- | A cell, containing a value and possibly finalizers.
data Cell a = Cell
    { value      :: a
    -- ^ The value wrapped by the cell.
    , finalizers :: TVar [MVar ()]
    -- ^ Experimentally, TVars appear not to be safe for finalizers, so
    -- instead we create MVars for the finalizers, and store them this
    -- list so that we maintain a reference to them.
    }
    deriving(Eq)

-- | Use the cell's value, suppressing the cell's garbage collection
with :: Cell a -> (a -> STM b) -> STM b
with cell action = do
    result <- action $ value cell
    unsafeIOToSTM (touch cell)
    return result

-- Create  a new cell, initially with no finalizers.
newCell :: a -> STM (Cell a)
newCell value = do
    finalizers <- newTVar []
    pure Cell { value, finalizers }

-- Add a new finalizer to the cell. Cells may have many finalizers
-- attached.
addFinalizer :: Cell a -> IO () -> IO ()
addFinalizer Cell{finalizers} fin = do
    mvar <- newEmptyMVar
    _ <- mkWeakMVar mvar fin
    atomically $ modifyTVar' finalizers (mvar:)
