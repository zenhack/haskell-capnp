module Main where


import Tests.Util

import Test.Framework (defaultMain)
import Tests.Data.CapNProto.Pointer (ptrTests)
import Tests.Data.CapNProto.Untyped (untypedTests)

main :: IO ()
main = defaultMain [ ptrTests
                   , untypedTests
                   ]
