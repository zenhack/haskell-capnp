{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module Control.Monad.CapNProto.MessageBuilder where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.RWS.Strict (RWST(runRWST), local, get, put)
import Data.Primitive.ByteArray
    ( MutableByteArray
    , newByteArray
    , sizeofMutableByteArray
    , copyMutableByteArray
    )

import Data.CapNProto.Blob (BlobSlice(..))


newtype ByteCount = ByteCount Int deriving(Num, Real, Integral, Ord, Eq, Enum)
newtype WordCount = WordCount Int deriving(Num, Real, Integral, Ord, Eq, Enum)

bytesToWords :: ByteCount -> WordCount
bytesToWords (ByteCount n) = WordCount (n `div` 8)
wordsToBytes :: WordCount -> ByteCount
wordsToBytes (WordCount n) = ByteCount (n * 8)

data BuilderState s = BuilderState
    { nextAlloc :: !ByteCount
    , array :: MutableByteArray s
    }

data BuilderEnv = BuilderEnv
    { parentOff :: !WordCount
    }

newtype BuilderT p s m a =
    BuilderT (RWST BuilderEnv () (BuilderState s) m a)
    deriving(Functor, Applicative, Monad)

runBuilderT :: (PrimMonad m)
    => BuilderT p (PrimState m) m a
    -> m (BlobSlice (MutableByteArray (PrimState m)), a)
runBuilderT (BuilderT m) = do
    initialArray <- newByteArray 0
    (x, bs, ()) <- runRWST
                       m
                       BuilderEnv   { parentOff = 0 }
                       BuilderState { array = initialArray, nextAlloc = 0 }
    return ( BlobSlice { blob = array bs
                       , offset = 0
                       , nextAlloc bs
                       }
           , x
           )


instance MonadTrans (BuilderT p s) where
    lift = BuilderT . lift

ensureSpaceFor :: (PrimMonad m) => ByteCount -> BuilderT p (PrimState m) m ()
ensureSpaceFor (ByteCount sz) = BuilderT $ do
    bs@BuilderState{..} <- get
    when (sizeofMutableByteArray array - fromIntegral nextAlloc < sz) $ do
        array' <- lift $ newByteArray $ sizeofMutableByteArray array * 2 + sz
        copyMutableByteArray array' 0 array 0 (fromIntegral nextAlloc)
        put bs{ array = array' }

alloc :: (PrimMonad m)
    => WordCount -> BuilderT p (PrimState m) m WordCount
alloc szWords = do
    let szBytes = wordsToBytes szWords
    ensureSpaceFor $ szBytes
    bs@BuilderState{..} <- BuilderT get
    BuilderT $ put bs { nextAlloc = nextAlloc + szBytes }
    return $ bytesToWords nextAlloc


withParent :: (PrimMonad m)
    => WordCount -> BuilderT c (PrimState m) m () -> BuilderT p (PrimState m) m WordCount
withParent sz (BuilderT m) = do
    off <- alloc sz
    BuilderT $ local (\env -> env { parentOff = off }) m
    return off
