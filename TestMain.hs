module Main where


import Tests.Util

import Test.Framework (defaultMain)
import Tests.Data.CapNProto.Pointer (ptrTests)

main :: IO ()
main = defaultMain [ptrTests]
