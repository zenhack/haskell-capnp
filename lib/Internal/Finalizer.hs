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
* Can be created and read inside STM, and
* May safely have finalizers, using the 'addFinalizer' function in
  this module.
* Ensures that the finalizers will not be run before any transaction that
  reads data is complete.

Note that it is *not* safe to use the primitives from "Sys.Mem.Weak" to
add finalizers.

-}
{-# LANGUAGE NamedFieldPuns #-}
module Internal.Finalizer (Cell, get, newCell, addFinalizer) where

import Control.Concurrent.MVar (MVar, mkWeakMVar, newEmptyMVar)
import Control.Concurrent.STM
    (STM, TVar, atomically, modifyTVar', newTVar, readTVar)

-- | A cell, containing a value and possibly finalizers.
newtype Cell a
    = Cell (TVar (CellData a))
    deriving(Eq)

-- The actual contents of a cell. This is wrapped in a 'TVar' to force accesses
-- to add the a reference the transaction log from which the finalizers are
-- reachable, thus preventing them from running before the completion of any
-- transaction that examines the value.
data CellData a = CellData
    { value      :: a
    -- ^ The value wrapped by the cell.

    , finalizers :: [MVar ()]
    -- ^ Experimentally, TVars appear not to be safe for finalizers, so
    -- instead we create MVars for the finalizers, and store them this
    -- list so that we maintain a reference to them.
    }
    deriving(Eq)

-- | Get the value from a cell
get :: Cell a -> STM a
get (Cell state) = value <$> readTVar state

-- Create  a new cell, initially with no finalizers.
newCell :: a -> STM (Cell a)
newCell value = Cell <$> newTVar CellData { value, finalizers = [] }

-- Add a new finalizer to the cell. Cells may have many finalizers
-- attached.
addFinalizer :: Cell a -> STM () -> IO ()
addFinalizer (Cell stateVar) fin = do
    mvar <- newEmptyMVar
    _ <- mkWeakMVar mvar $ atomically fin
    atomically $ modifyTVar' stateVar $ \state@CellData{finalizers} ->
        state { finalizers = mvar : finalizers }
