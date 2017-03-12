module Main where

{-
import qualified Data.ByteString as B
import Data.CapNProto.Stream (getMessage, StreamReader(..))
import System.IO (stdin)

import Data.CapNProto.Message
import Data.CapNProto.Message.Vector

instance StreamReader IO where
    getByteString = B.hGet stdin

main :: IO ()
main = do
    msg <- getMessage
    print msg
-}

import Data.CapNProto.Message
import Data.CapNProto.Message.Vector

main :: IO ()
main = return ()
