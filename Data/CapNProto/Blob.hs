{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.CapNProto.Blob
    ( Blob(..)
    , BlobSlice(..)
    )
  where

import Prelude hiding (length)

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.ByteString as BS
import Data.Bits
import Data.Word

import qualified Data.CapNProto.Errors as E

class Blob m a where
    length :: a -> m Int
    index :: a -> Int -> m Word64


data BlobSlice a = BlobSlice
    { blob :: a
    , offset :: Int
    , sliceLen :: Int
    }


instance (Blob m a, MonadThrow m) => Blob m (BlobSlice a) where
    length b = return $ sliceLen b
    index b i = do
        when (i > sliceLen b) $
            throwM E.BoundsError { E.index = i
                                 , E.maxIndex = sliceLen b - 1
                                 }
        index (blob b) (offset b + i)


instance (Monad m) => Blob m BS.ByteString where
    length bs = return $ BS.length bs `div` 8
    index bs i = do
        return $ foldl (.|.) 0 $ map byteN [0,1..7]
      where
        byteN n = fromIntegral (BS.index bs (i * 8 + n)) `shiftL` (n * 8)
