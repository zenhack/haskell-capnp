module Main where


import Test.Framework (defaultMain)
import Tests.Data.CapNProto.Bits (bitsTests)
import Tests.Data.CapNProto.Pointer (ptrTests)
import Tests.Data.CapNProto.Untyped (untypedTests)

main :: IO ()
main = defaultMain [ bitsTests
                   , ptrTests
                   , untypedTests
                   ]
