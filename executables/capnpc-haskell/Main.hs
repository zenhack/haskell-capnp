module Main (main) where

import qualified Data.ByteString        as BS
import qualified Data.CapNProto.Message as M

main :: IO ()
main = do
    contents <- BS.getContents
    print $ BS.length contents
    print contents
    msg <- M.decode contents
    print msg
