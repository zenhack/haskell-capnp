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

import UnliftIO.STM

import Control.Concurrent
    (ThreadId, forkIO, myThreadId, threadDelay, throwTo)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM   (throwSTM)
import Control.Monad            (forever, void)
import Data.Foldable            (traverse_)
import UnliftIO.Exception
    (Exception, SomeException, bracket, bracket_, toException, withException)

import qualified Data.Set as S

newtype Supervisor = Supervisor (TVar (Either SomeException (S.Set ThreadId)))

newSupervisor :: IO Supervisor
newSupervisor = Supervisor <$> newTVarIO (Right S.empty)

runSupervisor :: Supervisor -> IO ()
runSupervisor sup =
    forever (threadDelay (1000 * 1000 * 1000))
    `withException`
    \e -> throwKids sup (e :: SomeException)

withSupervisor :: (Supervisor -> IO ()) -> IO ()
withSupervisor f = do
    sup <- newSupervisor
    withAsync (runSupervisor sup) $ const (f sup)

-- | Throw an exception to all of a supervisor's children, using 'throwTo'.
throwKids :: Exception e => Supervisor -> e -> IO ()
throwKids (Supervisor stateVar) exn = do
    bracket
        (atomically $ readTVar stateVar >>= \case
            Left _ ->
                pure S.empty
            Right kids -> do
                writeTVar stateVar $! Left (toException exn)
                pure kids)
        (traverse_ (`throwTo` exn))
        (\_ -> pure ())

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
                    writeTVar stateVar $! Right newKids
    -- | Remove our ThreadId from the supervisor, so we don't leak it.
    removeMe = do
        me <- myThreadId
        atomically $ modifyTVar' stateVar $ \case
            state@(Left _) ->
                -- The supervisor is already stopped; we don't need to
                -- do anything.
                state
            Right kids ->
                -- We need to remove ourselves from the list of children;
                -- if we don't, we'll leak our ThreadId until the supervisor
                -- exits.
                --
                -- The use of $! here is very important, because even though
                -- modifyTVar' is strict, it only does whnf, so it would leave
                -- the state only evaluated as far as @Right (S.delete me kids)@;
                -- in that case we would still leak @me@.
                Right $! S.delete me kids
