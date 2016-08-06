module Main where

import qualified Data.ByteString.Lazy as B
import Encoding
import Data.Binary.Get

main :: IO ()
main = do
    contents <- B.getContents
    let msg = runGet getMessage contents
    print msg
