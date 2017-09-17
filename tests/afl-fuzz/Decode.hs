{-# LANGUAGE ForeignFunctionInterface #-}

import qualified Data.ByteString as BS
import Control.Monad.Quota
import Control.Monad.Catch
import qualified Data.Vector as V
import Data.CapNProto.Message as M

import Data.CapNProto.Blob as B

import Foreign.C.Types
import Foreign.C.String

decodeCGR :: (MonadThrow m) => BS.ByteString -> m BS.ByteString
decodeCGR bytes = do
  (Message segs) <- M.decode bytes
  return $ BS.concat $ V.toList $ V.map B.blob segs

decodeStdin_hs :: IO CInt
decodeStdin_hs = do
  bytes <- BS.getContents
  decoded <- decodeCGR bytes
  return $ fromIntegral $ BS.length decoded

foreign export ccall decodeStdin_hs :: IO CInt

