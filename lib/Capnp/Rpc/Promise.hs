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
    , newPromiseWithCallback
    , newCallback

    -- * Fulfilling or breaking promises
    , fulfill
    , breakPromise
    , breakOrFulfill
    , ErrAlreadyResolved(..)

    -- * Getting the value of a promise
    , wait
    ) where

import Control.Concurrent.STM
import Control.Monad.STM.Class

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

-- | Fulfill a promise by supplying the specified value. It is an error to
-- call 'fulfill' if the promise has already been fulfilled (or broken).
fulfill :: MonadSTM m => Fulfiller a -> a -> m ()
fulfill f val = breakOrFulfill f (Right val)

-- | Break a promise. When the user of the promise executes 'wait', the
-- specified exception will be raised. It is an error to call 'breakPromise'
-- if the promise has already been fulfilled (or broken).
breakPromise :: MonadSTM m => Fulfiller a -> Exception -> m ()
breakPromise f exn = breakOrFulfill f (Left exn)

-- | 'breakOrFulfill' calls either 'breakPromise' or 'fulfill', depending
-- on the argument.
breakOrFulfill :: MonadSTM m => Fulfiller a -> Either Exception a -> m ()
breakOrFulfill Fulfiller{callback} result = liftSTM $ callback result

-- | Wait for a promise to resolve, and return the result. If the promise
-- is broken, this raises an exception instead (see 'breakPromise').
wait :: MonadSTM m => Promise a -> m a
wait Promise{var} = liftSTM $ do
    val <- readTVar var
    case val of
        Nothing ->
            retry
        Just (Right result) ->
            pure result
        Just (Left exn) ->
            throwSTM exn

-- | Create a new promise and an associated fulfiller.
newPromise :: MonadSTM m => m (Promise a, Fulfiller a)
newPromise = liftSTM $ do
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

-- | Create a new promise which also excecutes an STM action when it is resolved.
newPromiseWithCallback :: MonadSTM m => (Either Exception a -> STM ()) -> m (Promise a, Fulfiller a)
newPromiseWithCallback callback = liftSTM $ do
    (promise, Fulfiller{callback=oldCallback}) <- newPromise
    pure
        ( promise
        , Fulfiller
            { callback = \result -> oldCallback result >> callback result
            }
        )

-- | Like 'newPromiseWithCallback', but doesn't return the promise.
newCallback :: MonadSTM m => (Either Exception a -> STM ()) -> m (Fulfiller a)
newCallback = liftSTM . fmap snd . newPromiseWithCallback

-- | A promise is a value that may not be ready yet.
newtype Promise a = Promise
    { var :: TVar (Maybe (Either Exception a))
    }
    deriving(Eq)
