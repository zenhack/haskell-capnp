{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Internal.TCloseQ
-- Description: Transactional queues with a close operation.
--
-- This module provides a thin layer over 'TQueue', which affords "closing"
-- queues. Reading from a closed queue returns 'Nothing'.
module Internal.TCloseQ
  ( Q,
    ErrClosed (..),
    new,
    read,
    write,
    close,
  )
where

import Control.Concurrent.STM
import Control.Exception (Exception)
import Control.Monad (unless, when)
import Data.Maybe (isNothing)
import Prelude hiding (read)

-- | A Queue with a close operation, with element type @a@.
data Q a = Q
  { q :: TQueue (Maybe a),
    isClosed :: TVar Bool
  }

-- | An exception which is thrown if a caller tries to write to a closed queue.
data ErrClosed = ErrClosed
  deriving (Show)

instance Exception ErrClosed

-- | Create a new empty queue.
new :: STM (Q a)
new = do
  q <- newTQueue
  isClosed <- newTVar False
  pure Q {..}

-- | Read a value from the queue. Returns Nothing if the queue is closed.
read :: Q a -> STM (Maybe a)
read Q {q} = do
  ret <- readTQueue q
  when (isNothing ret) $
    -- put it back in, so future reads also return nothing:
    writeTQueue q ret
  pure ret

-- | Write a value to the queue, which must not be closed. If it is closed,
-- this will throw 'ErrClosed'.
write :: Q a -> a -> STM ()
write Q {q, isClosed} v = do
  c <- readTVar isClosed
  when c $ throwSTM ErrClosed
  writeTQueue q (Just v)

-- | Close a queue. It is safe to close a queue more than once; subsequent
-- closes will have no effect.
close :: Q a -> STM ()
close Q {q, isClosed} = do
  c <- readTVar isClosed
  unless c $ do
    writeTVar isClosed True
    writeTQueue q Nothing
