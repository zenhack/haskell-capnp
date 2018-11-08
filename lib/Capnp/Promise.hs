{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Capnp.Promise
    ( Promise
    , Fulfiller
    , newPromise
    , newPromiseIO
    , fulfill
    , fulfillIO
    , breakPromise
    , breakPromiseIO
    , wait
    , waitIO
    ) where

import UnliftIO.STM

import Control.Concurrent.STM (throwSTM)
import Control.Monad.IO.Class (MonadIO)

import qualified UnliftIO.Exception as HsExn

import Capnp.Gen.Capnp.Rpc.Pure

instance HsExn.Exception Exception

-- | A 'Fulfiller' is used to fulfill a promise.
newtype Fulfiller a = Fulfiller
    { var :: TVar (Maybe (Either Exception a))
    }
    deriving(Eq)

-- | Fulfill a promise by supplying the specified value. It is an error to
-- call 'fulfill' if the promise has already been fulfilled (or broken).
fulfill :: Fulfiller a -> a -> STM ()
fulfill Fulfiller{var} val = modifyTVar' var $ \case
    Nothing ->
        Just (Right val)
    Just _ ->
        -- TODO: report this in a more controlled way.
        error "BUG: tried to fullfill a promise twice!"

-- | Like 'fulfill', but in the IO monad.
fulfillIO :: MonadIO m => Fulfiller a -> a -> m ()
fulfillIO fulfiller = atomically . fulfill fulfiller

-- | Break a promise. When the user of the promise executes 'wait', the
-- specified exception will be raised. It is an error to call 'breakPromise'
-- if the promise has already been fulfilled (or broken).
breakPromise :: Fulfiller a -> Exception -> STM ()
breakPromise Fulfiller{var} exn = modifyTVar' var $ \case
    Nothing ->
        Just (Left exn)
    Just _ ->
        error "BUG: tried to break an already resolved promise!"

breakPromiseIO :: MonadIO m => Fulfiller a -> Exception -> m ()
breakPromiseIO fulfiller = atomically . breakPromise fulfiller

-- | Wait for a promise to resolve, and return the result. If the promise
-- is broken, this raises an exception instead (see 'breakPromise').
wait :: Promise a -> STM a
wait Promise{var} = do
    val <- readTVar var
    case val of
        Nothing ->
            retrySTM
        Just (Right result) ->
            pure result
        Just (Left exn) ->
            throwSTM exn

-- | Like 'wait', but in the 'IO' monad.
waitIO :: MonadIO m => Promise a -> m a
waitIO = atomically . wait

-- | Create a new promise and an associated fulfiller.
newPromise :: STM (Promise a, Fulfiller a)
newPromise = do
    var <- newTVar Nothing
    pure (Promise{var}, Fulfiller{var})

-- | Like 'newPromise', but in the IO monad.
newPromiseIO :: MonadIO m => m (Promise a, Fulfiller a)
newPromiseIO = atomically newPromise

-- | A promise is a value that may not be ready yet.
newtype Promise a = Promise
    { var :: TVar (Maybe (Either Exception a))
    }
    deriving(Eq)
