module Main where

import qualified Data.ByteString as B
import Data.CapNProto.Stream (getMessage, StreamReader(..))
import System.IO (stdin)

instance StreamReader IO where
    getByteString = B.hGet stdin

main :: IO ()
main = do
    msg <- getMessage
    print msg
