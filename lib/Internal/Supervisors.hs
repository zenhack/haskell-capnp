{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-|
Module: Internal.Supervisors
Description: Montior a pool of threads.

This module exposes a 'Supervisor' construct, which can be used to safely
spawn threads while guaranteeing that:

* When the supervisor is killed, all of the threads it supervises will be
  killed.
* Child threads can terminate in any order, and memory usage will always
  be proportional to the number of *live* supervised threads.
-}
module Internal.Supervisors (Supervisor, supervise, withSupervisor) where

import Control.Concurrent.STM

import Control.Concurrent
    (ThreadId, forkIO, myThreadId, threadDelay, throwTo)
import Control.Concurrent.Async (withAsync)
import Control.Exception
    (Exception, SomeException, bracketOnError, bracket_, catch, toException)
import Control.Monad            (forever, void)
import Data.Foldable            (traverse_)

import qualified Data.Set as S

newtype Supervisor = Supervisor (TVar (Either SomeException (S.Set ThreadId)))

newSupervisor :: IO Supervisor
newSupervisor = Supervisor <$> newTVarIO (Right S.empty)

runSupervisor :: Supervisor -> IO ()
runSupervisor sup =
    forever (threadDelay (1000 * 1000 * 1000))
    `catch`
    (\e -> throwKids sup (e :: SomeException))

withSupervisor :: (Supervisor -> IO ()) -> IO ()
withSupervisor f = do
    sup <- newSupervisor
    withAsync (runSupervisor sup) $ const (f sup)

throwKids :: Exception e => Supervisor -> e -> IO ()
throwKids (Supervisor stateVar) exn = bracketOnError
    getState
    restoreState
    $ \case
        Left _ ->
            pure ()
        Right kids ->
            traverse_ (`throwTo` exn) kids
  where
    getState = atomically $ do
        state <- readTVar stateVar
        case state of
            Left _ ->
                pure ()
            Right kids ->
                writeTVar' stateVar $ Left (toException exn)
        pure state

    restoreState state =
        atomically $ writeTVar' stateVar state

    -- the docs don't specify whether 'writeTVar' forces the contents of
    -- the TVar before leaving. We need this so we don't leak things, so
    -- we write a wrapper that is guaranteed to do so.
    writeTVar' var val =
        modifyTVar' var $ const val


-- | Launch the IO action in a thread, monitored by the 'Supervisor'. If the
-- supervisor receives an exception, the exception will also be raised in the
-- child thread.
supervise :: IO () -> Supervisor -> IO ()
supervise task (Supervisor stateVar) =
    void $ forkIO $ bracket_ addMe removeMe task
  where
    -- | Add our ThreadId to the supervisor.
    addMe = do
        me <- myThreadId
        atomically $ do
            supState <- readTVar stateVar
            case supState of
                Left e ->
                    throwSTM e
                Right kids -> do
                    let !newKids = S.insert me kids
                    writeTVar stateVar (Right newKids)
    -- | Remove our ThreadId from the supervisor, so we don't leak it.
    removeMe = do
        me <- myThreadId
        atomically $ modifyTVar' stateVar $ \state -> case state of
            Left _ ->
                -- The supervisor is already stopped; we don't need to
                -- do anything.
                state
            Right kids ->
                -- We need to remove ourselves from the list of children;
                -- if we don't, we'll leak our ThreadId until the supervisor
                -- exits.
                --
                -- The bang pattern here is very important, because even though
                -- modifyTVar' is strict, it only does whnf, so it would leave
                -- the state only evaluated as far as @Right (S.delete me kids)@;
                -- in that case we would still leak @me@.
                let !newKids = S.delete me kids
                in Right newKids
