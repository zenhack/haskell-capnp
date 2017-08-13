{-# LANGUAGE
      MultiParamTypeClasses
    , FlexibleInstances
    , TypeFamilies #-}
module Data.CapNProto.Blob
    ( Blob(..)
    , MutBlob(..)
    , BlobSlice(..)
    )
  where

import Prelude hiding (length)

import Control.Monad (when, forM_)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.ByteString as BS
import Data.Bits
import Data.Word
import Data.Primitive.ByteArray
    ( MutableByteArray
    , sizeofMutableByteArray
    , copyMutableByteArray
    , newByteArray
    , readByteArray
    , writeByteArray
    , ByteArray
    , sizeofByteArray
    , indexByteArray
    )

import qualified Data.CapNProto.Errors as E

-- TODO: be clearer about error handling re: these classes. The general notion
-- is to either throw (in a monad that supports it), or just not check the
-- inputs at all, relying on the caller not to violate the invariants.
-- Preference would be for the former, of course.

-- | A 'Blob' is an array of 'Word64's, which may be accessed
-- inside of a Monad.
class Blob m a where
    -- | Return the length of the blob, in words.
    length :: a -> m Int
    -- | @index b i@ returns the ith word in the blob @b@. Typically,
    -- instances will include a MonadThrow constraint, throwing on out
    -- of bounds errors.
    index :: a -> Int -> m Word64

-- | A mutable 'Blob'.
class Blob m a => MutBlob m a where
    -- | @write b i value@ writes @value@ to the ith position in the blob.
    write :: a -> Int -> Word64 -> m ()
    -- | @grow b amount@ grows the blob @b@ by @amount@ words. In an instance
    -- of PrimMonad, this may modify or destroy the original blob.
    grow :: a -> Int -> m a

-- | A slice of a blob.
--
-- This wraps an instance of blob, exposing a sub-range of it, and allowing
-- further slicing operations. The resulting value is itself a blob.
data BlobSlice a = BlobSlice
    { blob :: a
    , offset :: Int
    , sliceLen :: Int
    } deriving(Show)


-- | @lengthFromBytes f@ is a valid implenetation of the 'Blob' class's length
-- method, given that @f b@ returns the length of the blob @b@, in bytes.
lengthFromBytes :: (Monad m) => (a -> m Int) -> a -> m Int
lengthFromBytes length arr = do
    len <- length arr
    return $ len `div` 8

-- | @indexFromBytes f@ is a valid implenetation of the 'Blob' class's index
-- method, given that @f b i@ returns the @ith@ byte of the blob @b@.
indexFromBytes :: (Monad m) => (a -> Int -> m Word8) -> a -> Int -> m Word64
indexFromBytes index arr i = do
        foldl (.|.) 0 <$> mapM byteN [0,1..7]
      where
        byteN n = do
            b <- index arr (i * 8 + n)
            return $ fromIntegral b `shiftL` (n * 8)

writeFromBytes :: Monad m => (a -> Int -> Word8 -> m ()) -> a -> Int -> Word64 -> m ()
writeFromBytes writeByte arr words value = do
    let base = words * 8
    forM_ ([0,1..7] :: [Int]) $ \i -> do
        writeByte arr (base + i) $ fromIntegral $ value `shiftR` (i * 8)


instance (Blob m a, MonadThrow m) => Blob m (BlobSlice a) where
    length b = return $ sliceLen b
    index b i = do
        when (i > sliceLen b) $
            throwM E.BoundsError { E.index = i
                                 , E.maxIndex = sliceLen b - 1
                                 }
        index (blob b) (offset b + i)


instance (Monad m) => Blob m BS.ByteString where
    length = lengthFromBytes (return . BS.length)
    index = indexFromBytes $ \bs i -> return $ BS.index bs i

instance (Monad m) => Blob m ByteArray where
    length = lengthFromBytes (return . sizeofByteArray)
    index = indexFromBytes $ \arr i -> return $ indexByteArray arr i

instance (PrimMonad m, s ~ PrimState m) => Blob m (MutableByteArray s) where
    length = lengthFromBytes (return . sizeofMutableByteArray)
    index = indexFromBytes readByteArray

instance (PrimMonad m, s ~ PrimState m) => MutBlob m (MutableByteArray s) where
    write = writeFromBytes writeByteArray
    grow arr amount = do
        let oldSize = sizeofMutableByteArray arr
        let amountBytes = amount * 8
        arr' <- newByteArray $ oldSize + amountBytes
        copyMutableByteArray arr' 0 arr 0 oldSize
        return arr'
