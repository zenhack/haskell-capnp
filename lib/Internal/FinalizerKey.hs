{-|
Module: Internal.FinalizerKey
Description: Make resource-safe finalizer keys.

This module creates values to which finalizers can safely be attached,
without worrying that they may be collected early. Finalizer keys can be
created inside STM (though finalizers must be attached in IO).

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

Experimentally, TVars appear not to be safe in this manner, so being
able to create a finalizer key within STM is a bit fiddly. This module
encapsulates that logic.

-}
module Internal.FinalizerKey (Key, newSTM, set) where

import Control.Concurrent.MVar (MVar, mkWeakMVar, newEmptyMVar)
import Control.Concurrent.STM  (STM, TVar, atomically, newTVar, writeTVar)

-- | A key to which a finalizer may be attached.
newtype Key = Key (TVar (Maybe (MVar ())))

-- | Create a key within STM, with no finalizer attached initially.
newSTM :: STM Key
newSTM = Key <$> newTVar Nothing

-- | Set the finalizer for a key. If there is already a finalizer
-- registered, it may be run early; usesrs are encouraged to only
-- register one finalizer per key.
set :: Key -> IO () -> IO ()
set (Key tvar) finalize = do
    mvar <- newEmptyMVar
    _ <- mkWeakMVar mvar finalize
    atomically $ writeTVar tvar (Just mvar)
