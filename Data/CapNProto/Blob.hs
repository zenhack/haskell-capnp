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
    , fillByteArray
    , newByteArray
    , readByteArray
    , writeByteArray
    , ByteArray
    , sizeofByteArray
    , indexByteArray
    )

import Data.CapNProto.Bits
    ( WordCount(..)
    , ByteCount(..)
    , wordsToBytes
    , bytesToWords
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
    length :: a -> m WordCount
    -- | @index b i@ returns the ith word in the blob @b@. Typically,
    -- instances will include a MonadThrow constraint, throwing on out
    -- of bounds errors.
    index :: a -> WordCount -> m Word64

-- | A mutable 'Blob'.
class Blob m a => MutBlob m a where
    -- | @write b i value@ writes @value@ to the ith position in the blob.
    write :: a -> WordCount -> Word64 -> m ()
    -- | @grow b amount@ grows the blob @b@ by @amount@ words. In an instance
    -- of PrimMonad, this may modify or destroy the original blob.
    grow :: a -> WordCount -> m a

-- | A slice of a blob.
--
-- This wraps an instance of blob, exposing a sub-range of it, and allowing
-- further slicing operations. The resulting value is itself a blob.
data BlobSlice a = BlobSlice
    { blob :: a
    , offset :: WordCount
    , sliceLen :: WordCount
    } deriving(Show)


-- | @lengthFromBytes f@ is a valid implenetation of the 'Blob' class's length
-- method, given that @f b@ returns the length of the blob @b@, in bytes.
lengthFromBytes :: (Monad m) => (a -> m ByteCount) -> a -> m WordCount
lengthFromBytes length arr = bytesToWords <$> length arr

-- | @indexFromBytes f@ is a valid implenetation of the 'Blob' class's index
-- method, given that @f b i@ returns the @ith@ byte of the blob @b@.
indexFromBytes :: (Monad m) => (a -> ByteCount -> m Word8) -> a -> WordCount -> m Word64
indexFromBytes index arr i = foldl (.|.) 0 <$> mapM byteN [0,1..7]
      where
        byteN n = do
            b <- index arr (wordsToBytes i + n)
            return $ fromIntegral b `shiftL` (fromIntegral n * 8)

writeFromBytes :: Monad m => (a -> ByteCount -> Word8 -> m ()) -> a -> WordCount -> Word64 -> m ()
writeFromBytes writeByte arr words value = do
    let base = wordsToBytes words
    forM_ ([0,1..7] :: [Int]) $ \i ->
        writeByte arr (base + fromIntegral i) $ fromIntegral $ value `shiftR` (i * 8)


instance (Blob m a, MonadThrow m) => Blob m (BlobSlice a) where
    length b = return $ sliceLen b
    index b i = do
        when (i > sliceLen b) $
            throwM E.BoundsError { E.index = fromIntegral i
                                 , E.maxIndex = fromIntegral $ sliceLen b - 1
                                 }
        index (blob b) (offset b + i)


instance (Monad m) => Blob m BS.ByteString where
    length = lengthFromBytes (return . ByteCount . BS.length)
    index = indexFromBytes $ \bs i -> return $ BS.index bs (fromIntegral i)

instance (Monad m) => Blob m ByteArray where
    length = lengthFromBytes (return . ByteCount . sizeofByteArray)
    index = indexFromBytes $ \arr i -> return $ indexByteArray arr (fromIntegral i)

instance (PrimMonad m, s ~ PrimState m) => Blob m (MutableByteArray s) where
    length = lengthFromBytes (return . ByteCount . sizeofMutableByteArray)
    index = indexFromBytes $ \arr (ByteCount i) -> readByteArray arr i

instance (PrimMonad m, s ~ PrimState m) => MutBlob m (MutableByteArray s) where
    write = writeFromBytes $ \arr (ByteCount i) -> writeByteArray arr i
    grow arr amount = do
        let oldSize = sizeofMutableByteArray arr
        let ByteCount amountBytes = wordsToBytes amount
        arr' <- newByteArray $ oldSize + amountBytes
        copyMutableByteArray arr' 0 arr 0 oldSize
        fillByteArray arr' oldSize amountBytes 0
        return arr'
