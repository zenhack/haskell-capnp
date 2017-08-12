{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies #-}
module Control.Monad.CapNProto.MessageBuilder where

import Control.Monad (when, forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.RWS.Strict (RWST(runRWST), local, get, put)
import Data.Primitive.ByteArray
    ( MutableByteArray
    , newByteArray
    , sizeofMutableByteArray
    , copyMutableByteArray
    , writeByteArray
    )

import Data.CapNProto.Blob (BlobSlice(..))
import Data.CapNProto.Schema (Field)
import Data.Bits
import Data.Word


-- wrapper types for numbers of bytes & words -- helpful for avoiding mixing
-- up units:
newtype ByteCount = ByteCount Int deriving(Num, Real, Integral, Ord, Eq, Enum)
newtype WordCount = WordCount Int deriving(Num, Real, Integral, Ord, Eq, Enum)

-- conversion functions for the above:
bytesToWords :: ByteCount -> WordCount
bytesToWords (ByteCount n) = WordCount (n `div` 8)
wordsToBytes :: WordCount -> ByteCount
wordsToBytes (WordCount n) = ByteCount (n * 8)

-- | Internal state of a builder.
data BuilderState s = BuilderState
    { nextAlloc :: !ByteCount -- ^ offset into the array for the next allocation
    , array :: MutableByteArray s -- ^ array storing the message being built
    }

data BuilderEnv = BuilderEnv
    { parentOff :: !WordCount -- ^ offset into the array to the start of
                              -- the parent object.
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
                       , sliceLen = fromIntegral $ nextAlloc bs
                       }
           , x
           )


instance MonadTrans (BuilderT p s) where
    lift = BuilderT . lift

-- | @ensureSpaceFor bytes@ ensures that the array in the builder state has
-- at least @bytes@ bytes of unused space; if not it is resized. resizing is
-- done in such a way that allocation of n bytes runs amortized O(n) time.
ensureSpaceFor :: (PrimMonad m) => ByteCount -> BuilderT p (PrimState m) m ()
ensureSpaceFor (ByteCount sz) = BuilderT $ do
    bs@BuilderState{..} <- get
    when (sizeofMutableByteArray array - fromIntegral nextAlloc < sz) $ do
        array' <- lift $ newByteArray $ sizeofMutableByteArray array * 2 + sz
        copyMutableByteArray array' 0 array 0 (fromIntegral nextAlloc)
        put bs{ array = array' }

-- | @alloc words@ allocates space for @words@ words in the builder state, and
-- returns the index of the first word in the allocation.
alloc :: (PrimMonad m)
    => WordCount -> BuilderT p (PrimState m) m WordCount
alloc szWords = do
    let szBytes = wordsToBytes szWords
    ensureSpaceFor $ szBytes
    bs@BuilderState{..} <- BuilderT get
    BuilderT $ put bs { nextAlloc = nextAlloc + szBytes }
    return $ bytesToWords nextAlloc


-- | @withParent words builder@ allocates a new object with size @words@, and
-- runs builder with that object set as the parent object. The location of the
-- newly allocated parent object is returned.
withParent :: (PrimMonad m)
    => WordCount -> BuilderT c (PrimState m) m () -> BuilderT p (PrimState m) m WordCount
withParent sz (BuilderT m) = do
    off <- alloc sz
    BuilderT $ local (\env -> env { parentOff = off }) m
    return off

class BuildSelf a where
    -- | @buildSelf x word offset@ stores the value @x@ in the message at the
    -- word @word@, shifted @offset@ bits from the start of the word. It should
    -- not distrupt other values in the word.
    --
    -- implementations may assume @offset@ is properly aligned; the caller is
    -- required to ensure this.
    buildSelf :: (PrimMonad m, s ~ PrimState m)
        => a -> WordCount -> Word16 -> BuilderT p s m ()

instance BuildSelf Word64 where
    buildSelf n words 0 = BuilderT $ do
        arr <- array <$> get
        let base = fromIntegral $ wordsToBytes words
        forM_ ([0,1..7] :: [Int]) $ \i -> do
            writeByteArray arr (base + i) $ n `shiftR` (i * 8)
    buildSelf _ _ off =
        error $ "call to (Word64) buildSelf with bit offset " ++ show off ++
            " is not Word64-aligned."

-- | @setField field value@ is a builder which sets the field @field@ in the
-- parent object to the value @value@.
setField, (%~) :: (PrimMonad m, s ~ PrimState m, BuildSelf c)
    => Field p c -> c                 -> BuilderT p s m ()
setField = undefined
-- | Infix alias for setField
(%~) = setField


-- | @buildField field builder@ is a builder which sets the field @field@ in
-- the parent object to the value built by @builder@.
buildField, (<~) :: (PrimMonad m, s ~ PrimState m)
    => Field p c -> BuilderT c s m () -> BuilderT p s m ()
buildField = undefined
-- | Infix alias for buildField
(<~) = buildField
