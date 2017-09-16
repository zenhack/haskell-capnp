{-# LANGUAGE
      MultiParamTypeClasses
    , FlexibleInstances
    , TypeFamilies #-}
module Data.CapNProto.Blob
    ( Blob(..)
    , MutBlob(..)
    , BlobSlice(..)
    , lengthInWords
    , indexWord
    , writeWord
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
    , bytesToWordsFloor
    )
import qualified Data.CapNProto.Errors as E

-- TODO: be clearer about error handling re: these classes. The general notion
-- is to either throw (in a monad that supports it), or just not check the
-- inputs at all, relying on the caller not to violate the invariants.
-- Preference would be for the former, of course.

-- | A 'Blob' is an array of bytes, which may be accessed
-- inside of a Monad.
class Blob m a where
    -- | Return the length of the blob, in words.
    length :: a -> m ByteCount
    -- | @index b i@ returns the ith byte in the blob @b@. Typically,
    -- instances will include a MonadThrow constraint, throwing on out
    -- of bounds errors.
    index :: a -> ByteCount -> m Word8

-- | A mutable 'Blob'.
class Blob m a => MutBlob m a where
    -- | @write b i value@ writes @value@ to the ith position in the blob.
    write :: a -> ByteCount -> Word8 -> m ()
    -- | @grow b amount@ grows the blob @b@ by @amount@ words. In an instance
    -- of PrimMonad, this may modify or destroy the original blob.
    grow :: a -> ByteCount -> m a

-- | A slice of a blob.
--
-- This wraps an instance of blob, exposing a sub-range of it, and allowing
-- further slicing operations. The resulting value is itself a blob.
data BlobSlice a = BlobSlice
    { blob :: a
    , offset :: ByteCount
    , sliceLen :: ByteCount
    } deriving(Show)

-- | @lengthInWords blob@ is The length of @blob@, in 64-bit words, rounded down.
lengthInWords :: (Monad m, Blob m b) => b -> m WordCount
lengthInWords = fmap bytesToWordsFloor . length

-- | @indexWord blob i@ returns the @ith@ little-endian 64-bit word of the blob @b@.
indexWord :: (Monad m, Blob m b) => b -> WordCount -> m Word64
indexWord blob i = foldl (.|.) 0 <$> mapM byteN [0,1..7]
      where
        byteN n = do
            b <- index blob (wordsToBytes i + n)
            return $ fromIntegral b `shiftL` (fromIntegral n * 8)

-- | @writeWord blob i word@ writes @word@ to the @ith@ little-endian 64-bit
-- word in @blob@.
writeWord :: (Monad m, MutBlob m b) => b -> WordCount -> Word64 -> m ()
writeWord arr words value = do
    let base = wordsToBytes words
    forM_ ([0,1..7] :: [Int]) $ \i ->
        write arr (base + fromIntegral i) $ fromIntegral $ value `shiftR` (i * 8)


instance (Blob m a, MonadThrow m) => Blob m (BlobSlice a) where
    length b = return $ sliceLen b
    index b i = do
        when (i > sliceLen b) $
            throwM E.BoundsError { E.index = fromIntegral i
                                 , E.maxIndex = fromIntegral $ sliceLen b - 1
                                 }
        index (blob b) (offset b + i)


instance (Monad m) => Blob m BS.ByteString where
    length = return . ByteCount . BS.length
    index bs (ByteCount i) = return $ BS.index bs i

instance (Monad m) => Blob m ByteArray where
    length = return . ByteCount . sizeofByteArray
    index arr (ByteCount i) = return $ indexByteArray arr i

instance (PrimMonad m, s ~ PrimState m) => Blob m (MutableByteArray s) where
    length = return . ByteCount . sizeofMutableByteArray
    index blob (ByteCount i) = readByteArray blob i

instance (PrimMonad m, s ~ PrimState m) => MutBlob m (MutableByteArray s) where
    write arr (ByteCount i) = writeByteArray arr i
    grow arr (ByteCount amount) = do
        let oldSize = sizeofMutableByteArray arr
        arr' <- newByteArray $ oldSize + amount
        copyMutableByteArray arr' 0 arr 0 oldSize
        fillByteArray arr' oldSize amount 0
        return arr'
