{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}
module Control.Monad.CapNProto.MessageBuilder
    ( BuilderT
    , runBuilderT
    )
  where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.State (StateT, get, modify, evalStateT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Word

import qualified Data.CapNProto.Blob as B

class B.Blob m b => MutBlob m b where
    write :: b -> Int -> Word64 -> m ()
    grow :: b -> Int -> m b
    new :: Int -> m b

data BuilderState s b = BuilderState
    { nextAlloc :: Int
    , segment :: b
    }

newtype BuilderT s b m a = BuilderT (StateT (BuilderState s b) m a)

runBuilderT :: (PrimMonad m, MutBlob m b) => BuilderT (PrimState m) b m a -> m a
runBuilderT (BuilderT st) = do
    segment <- new 0
    evalStateT st BuilderState { nextAlloc = 0
                               , segment = segment
                               }


alloc :: (PrimMonad m, MutBlob m b) => Int -> BuilderT (PrimState m) b m Int
alloc size = BuilderT $ do
    BuilderState{..} <- get
    blobLen <- lift $ B.length segment
    when (blobLen - nextAlloc < size) $ do
        segment' <- lift $ grow segment (nextAlloc + 1)
        modify $ \bs -> bs { segment = segment' }
    modify $ \bs -> bs { nextAlloc = nextAlloc + size }
    return nextAlloc
