{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
-- |
-- Module: Capnp.Rpc.Promise
-- Description: Promises
--
-- This module defines a 'Promise' type, represents a value which is not yet
-- available, and related utilities.
module Capnp.Rpc.Promise
    ( Promise
    , Fulfiller

    -- * Creating promises
    , newPromise
    , newPromiseSTM
    , newPromiseWithCallback
    , newPromiseWithCallbackSTM
    , newCallback
    , newCallbackSTM

    -- * Fulfilling or breaking promises
    , fulfill
    , fulfillSTM
    , breakPromise
    , breakPromiseSTM
    , ErrAlreadyResolved(..)

    -- * Getting the value of a promise
    , wait
    , waitSTM
    ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Control.Exception.Safe as HsExn

import Capnp.Gen.Capnp.Rpc.Pure
-- For exception instance:
import Capnp.Rpc.Errors ()

-- | An exception thrown if 'breakPromise' or 'fulfill' is called on an
-- already-resolved fulfiller.
data ErrAlreadyResolved = ErrAlreadyResolved deriving(Show)
instance HsExn.Exception ErrAlreadyResolved

-- | A 'Fulfiller' is used to fulfill a promise.
newtype Fulfiller a = Fulfiller
    { callback :: Either Exception a -> STM ()
    }

-- | Like 'fulfill', but in 'STM'
fulfillSTM :: Fulfiller a -> a -> STM ()
fulfillSTM Fulfiller{callback} val = callback (Right val)

-- | Fulfill a promise by supplying the specified value. It is an error to
-- call 'fulfill' if the promise has already been fulfilled (or broken).
fulfill :: MonadIO m => Fulfiller a -> a -> m ()
fulfill fulfiller = liftIO . atomically . fulfillSTM fulfiller

-- | Like 'breakPromise', but in 'STM'.
breakPromiseSTM :: Fulfiller a -> Exception -> STM ()
breakPromiseSTM Fulfiller{callback} exn = callback (Left exn)

-- | Break a promise. When the user of the promise executes 'wait', the
-- specified exception will be raised. It is an error to call 'breakPromise'
-- if the promise has already been fulfilled (or broken).
breakPromise :: MonadIO m => Fulfiller a -> Exception -> m ()
breakPromise fulfiller = liftIO . atomically . breakPromiseSTM fulfiller

-- | Like 'wait', but runs in 'STM'.
waitSTM :: Promise a -> STM a
waitSTM Promise{var} = do
    val <- readTVar var
    case val of
        Nothing ->
            retry
        Just (Right result) ->
            pure result
        Just (Left exn) ->
            throwSTM exn

-- | Wait for a promise to resolve, and return the result. If the promise
-- is broken, this raises an exception instead (see 'breakPromise').
wait :: MonadIO m => Promise a -> m a
wait = liftIO . atomically . waitSTM

-- | Like 'newPromise', but in 'STM'.
newPromiseSTM :: STM (Promise a, Fulfiller a)
newPromiseSTM = do
    var <- newTVar Nothing
    pure
        ( Promise{var}
        , Fulfiller
            { callback = \result -> do
                val <- readTVar var
                case val of
                    Nothing ->
                        writeTVar var (Just result)
                    Just _ ->
                        throwSTM ErrAlreadyResolved
            }
        )

-- | Like 'newPromiseWithCallbackSTM', but runs in 'STM'.
newPromiseWithCallbackSTM :: (Either Exception a -> STM ()) -> STM (Promise a, Fulfiller a)
newPromiseWithCallbackSTM callback = do
    (promise, Fulfiller{callback=oldCallback}) <- newPromiseSTM
    pure
        ( promise
        , Fulfiller
            { callback = \result -> oldCallback result >> callback result
            }
        )

-- | Create a new promise which also excecutes an STM action when it is resolved.
newPromiseWithCallback :: MonadIO m => (Either Exception a -> STM ()) -> m (Promise a, Fulfiller a)
newPromiseWithCallback = liftIO . atomically . newPromiseWithCallbackSTM

-- | Like 'newCallback', but runs in 'STM'.
newCallbackSTM :: (Either Exception a -> STM ()) -> STM (Fulfiller a)
newCallbackSTM = fmap snd . newPromiseWithCallbackSTM

-- | Like 'newPromiseWithCallback', but doesn't return the promise.
newCallback :: MonadIO m => (Either Exception a -> STM ()) -> m (Fulfiller a)
newCallback = liftIO . atomically . newCallbackSTM

-- | Create a new promise and an associated fulfiller.
newPromise :: MonadIO m => m (Promise a, Fulfiller a)
newPromise = liftIO $ atomically newPromiseSTM

-- | A promise is a value that may not be ready yet.
newtype Promise a = Promise
    { var :: TVar (Maybe (Either Exception a))
    }
    deriving(Eq)
