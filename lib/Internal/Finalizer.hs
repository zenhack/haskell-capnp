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
module Internal.Finalizer (Cell, get, newCell, addFinalizer) where

import Control.Concurrent.MVar (MVar, mkWeakMVar, newEmptyMVar)
import Control.Concurrent.STM
    ( STM
    , TVar
    , atomically
    , modifyTVar'
    , newTVar
    , readTVar
    , readTVarIO
    , retry
    , writeTVar
    )
import Control.Monad           (when)
import GHC.Conc                (unsafeIOToSTM)

-- | A cell, containing a value and possibly finalizers.
newtype Cell a = Cell (TVar (CellState a))
    deriving(Eq)

data CellState a = CellState
    { value       :: a
    -- ^ The value wrapped by the cell.
    , finalizers  :: TVar [MVar ()]
    -- ^ Experimentally, TVars appear not to be safe for finalizers, so
    -- instead we create MVars for the finalizers, and store them this
    -- list so that we maintain a reference to them.

    , isFinalized :: TVar Bool
    -- ^ Note [Finalizer race]
    }
    deriving(Eq)

-- | Get the value from a cell
get :: Cell a -> STM a
get (Cell state) = do
    CellState{value, isFinalized} <- readTVar state
    finalized <- readTVar isFinalized
    when finalized $ do
        -- Note [Finalizer race]
        unsafeIOToSTM $ putStrLn "BUG in haskell-capnp: finalizer ran before Cell was unreachable!"
        retry
    pure value

-- Create  a new cell, initially with no finalizers.
newCell :: a -> STM (Cell a)
newCell value = do
    isFinalized <- newTVar False
    finalizers <- newTVar []
    Cell <$> newTVar CellState { value, finalizers, isFinalized }

-- Add a new finalizer to the cell. Cells may have many finalizers
-- attached.
addFinalizer :: Cell a -> STM () -> IO ()
addFinalizer (Cell state) fin = do
    CellState {finalizers, isFinalized} <- readTVarIO state
    mvar <- newEmptyMVar
    _ <- mkWeakMVar mvar $ atomically $ do
        fin
        writeTVar isFinalized True
    atomically $ modifyTVar' finalizers (mvar:)

-- Note [Finalizer race]
--
-- Per issue #74, in a previous implementation it was possible for the cell to
-- be collected before a transaction that used it completes, and for the finalizer
-- to run to completion also before the transaction completes. To avoid this,
-- we need to make sure that the finalizer and any transaction that uses the cell
-- conflict, such that the finalizer always comes in a *later* transaction.
--
-- To achieve this we store a shared isFinalized TVar, and write to it from the
-- finalizers. 'get' reads it and verifies that it has not been set; if it has
-- it retries.
--
-- Subtly, this actually prevents the finalizer from running, because the
-- possibility of a retry means we need to keep holding the reference so we
-- can access the cell in a subsequent attempt -- so the retry should never actually
-- occur, but still needs to be there for correctness. We add a print statement
-- to the branch, so that if there is a bug where this actually *can* happen,
-- we get some indicator of this, rather than just deadlocking.
