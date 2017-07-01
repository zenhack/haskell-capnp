module Main where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.Quota (QuotaT(..), Quota(..))
import Control.Monad.State (MonadState, put, get, StateT(..))
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.CapNProto.Message as M
import Data.Word (Word8, Word32, Word64)
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U


main :: IO ()
main = do
    contents <- BS.getContents
    print $ BS.length contents
    print contents
    msg <- M.decode contents
    print msg
