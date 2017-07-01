{-# LANGUAGE RecordWildCards #-}
module Control.Monad.CapNProto.MessageBuilder
    ( BuilderT
    , runBuilderT
    )
  where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.State (StateT, get, modify, evalStateT)
import Data.Word
import qualified Data.Vector.Generic.Mutable as V

data BuilderState s v = BuilderState
    { nextAlloc :: Int
    , segment :: v s Word64
    }


newtype BuilderT s v m a = BuilderT (StateT (BuilderState s v) m a)

runBuilderT :: (PrimMonad m, V.MVector v Word64) =>
    BuilderT (PrimState m) v m a -> m a
runBuilderT (BuilderT st) = do
    segment <- V.new 0
    evalStateT st BuilderState { nextAlloc = 0
                               , segment = segment
                               }


alloc :: (PrimMonad m, V.MVector v Word64) => Int -> BuilderT (PrimState m) v m Int
alloc size = BuilderT $ do
    BuilderState{..} <- get
    when (V.length segment - nextAlloc < size) $ do
        segment' <- V.grow segment (nextAlloc + 1)
        modify $ \bs -> bs { segment = segment' }
    modify $ \bs -> bs { nextAlloc = nextAlloc + size }
    return nextAlloc
