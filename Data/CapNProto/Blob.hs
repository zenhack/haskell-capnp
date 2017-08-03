{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.CapNProto.Blob
    ( Blob(..)
    , BlobSlice(..)
    )
  where

import Prelude hiding (length)

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.ByteString as BS
import Data.Bits
import Data.Word
import Data.Primitive.ByteArray
    ( MutableByteArray
    , sizeofMutableByteArray
    , readByteArray
    , ByteArray
    , sizeofByteArray
    , indexByteArray
    )

import qualified Data.CapNProto.Errors as E

class Blob m a where
    length :: a -> m Int
    index :: a -> Int -> m Word64


data BlobSlice a = BlobSlice
    { blob :: a
    , offset :: Int
    , sliceLen :: Int
    } deriving(Show)


lengthFromBytes :: (Monad m) => (a -> m Int) -> a -> m Int
lengthFromBytes length arr = do
    len <- length arr
    return $ len `div` 8

indexFromBytes :: (Monad m) => (a -> Int -> m Word8) -> a -> Int -> m Word64
indexFromBytes index arr i = do
        foldl (.|.) 0 <$> mapM byteN [0,1..7]
      where
        byteN n = do
            b <- index arr (i * 8 + n)
            return $ fromIntegral b `shiftL` (n * 8)


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

instance (PrimMonad m) => Blob m (MutableByteArray (PrimState m)) where
    length = lengthFromBytes (return . sizeofMutableByteArray)
    index = indexFromBytes readByteArray

instance (Monad m) => Blob m ByteArray where
    length = lengthFromBytes (return . sizeofByteArray)
    index = indexFromBytes $ \arr i -> return $ indexByteArray bs i
