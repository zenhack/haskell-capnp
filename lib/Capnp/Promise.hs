{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Capnp.Promise
    ( Promise
    , Fulfiller
    , ErrAlreadyResolved(..)
    , newPromise
    , newPromiseIO
    , newPromiseWithCallback
    , newCallback
    , fulfill
    , fulfillIO
    , breakPromise
    , breakPromiseIO
    , wait
    , waitIO
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

-- | Fulfill a promise by supplying the specified value. It is an error to
-- call 'fulfill' if the promise has already been fulfilled (or broken).
fulfill :: Fulfiller a -> a -> STM ()
fulfill Fulfiller{callback} val = callback (Right val)

-- | Like 'fulfill', but in the IO monad.
fulfillIO :: MonadIO m => Fulfiller a -> a -> m ()
fulfillIO fulfiller = liftIO . atomically . fulfill fulfiller

-- | Break a promise. When the user of the promise executes 'wait', the
-- specified exception will be raised. It is an error to call 'breakPromise'
-- if the promise has already been fulfilled (or broken).
breakPromise :: Fulfiller a -> Exception -> STM ()
breakPromise Fulfiller{callback} exn = callback (Left exn)

breakPromiseIO :: MonadIO m => Fulfiller a -> Exception -> m ()
breakPromiseIO fulfiller = liftIO . atomically . breakPromise fulfiller

-- | Wait for a promise to resolve, and return the result. If the promise
-- is broken, this raises an exception instead (see 'breakPromise').
wait :: Promise a -> STM a
wait Promise{var} = do
    val <- readTVar var
    case val of
        Nothing ->
            retry
        Just (Right result) ->
            pure result
        Just (Left exn) ->
            throwSTM exn

-- | Like 'wait', but in the 'IO' monad.
waitIO :: MonadIO m => Promise a -> m a
waitIO = liftIO . atomically . wait

-- | Create a new promise and an associated fulfiller.
newPromise :: STM (Promise a, Fulfiller a)
newPromise = do
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
newPromiseWithCallback :: (Either Exception a -> STM ()) -> STM (Promise a, Fulfiller a)
newPromiseWithCallback callback = do
    (promise, Fulfiller{callback=oldCallback}) <- newPromise
    pure
        ( promise
        , Fulfiller
            { callback = \result -> oldCallback result >> callback result
            }
        )

newCallback :: (Either Exception a -> STM ()) -> STM (Fulfiller a)
newCallback = fmap snd . newPromiseWithCallback

-- | Like 'newPromise', but in the IO monad.
newPromiseIO :: MonadIO m => m (Promise a, Fulfiller a)
newPromiseIO = liftIO $ atomically newPromise

-- | A promise is a value that may not be ready yet.
newtype Promise a = Promise
    { var :: TVar (Maybe (Either Exception a))
    }
    deriving(Eq)
