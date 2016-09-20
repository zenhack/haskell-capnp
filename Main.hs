module Main where

import qualified Data.ByteString.Lazy as B
import Data.CapNProto.LowLevel
import Data.Binary.Get

main :: IO ()
main = do
    contents <- B.getContents
    print $ do
        msg <- runGet (getMessage defaultMaxMessageLen) contents
        getRootView msg defaultMaxPointerDepth
