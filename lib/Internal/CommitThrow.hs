{-|
Module: Internal.CommitThrow
Description: Helpers for throwing exceptions from an STM transaction, while still committing.
-}
module Internal.CommitThrow (atomicallyCommitErrs, throwAndCommit) where

import Control.Concurrent.STM

import Control.Exception (Exception, SomeException, throwIO, toException)

newtype CommitThrow = CommitThrow SomeException deriving(Show)
instance Exception CommitThrow

-- | Like 'atomically', but if the argument raises an exception using
-- 'throwAndCommit', The transaction will be committed before re-raising
-- the exception, rather than aborted.
--
-- Note that if the argument calls throwSTM, the transaction will be
-- aborted as usual.
atomicallyCommitErrs :: STM a -> IO a
atomicallyCommitErrs tx = do
    result <- atomically $ catchSTM
                (Right <$> tx)
                (pure . Left)
    case result of
        Right v              -> pure v
        Left (CommitThrow e) -> throwIO e

-- | Throw the exception. If it bubbles up to the top level of the transaction,
-- and the transaction was started with 'atomicallyCommitErrs', The changes will
-- be committed, rather than rolled back. The exception will then be re-thrown.
throwAndCommit :: Exception e => e -> STM a
throwAndCommit = throwSTM . CommitThrow . toException
