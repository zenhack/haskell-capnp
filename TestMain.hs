module Main where

import Test.Framework (defaultMain)
import Tests.Data.CapNProto.Pointer (ptrTests)

main :: IO ()
main = defaultMain [ptrTests]
