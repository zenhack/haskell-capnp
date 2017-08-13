module Main where


import Test.Framework (defaultMain)
import Tests.Data.CapNProto.Bits (bitsTests)
import Tests.Data.CapNProto.Pointer (ptrTests)
import Tests.Data.CapNProto.Untyped (untypedTests)
import Tests.Control.Monad.CapNProto.MessageBuilder (buildTests)

main :: IO ()
main = defaultMain [ bitsTests
                   , ptrTests
                   , untypedTests
                   , buildTests
                   ]
