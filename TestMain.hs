module Main where


import Test.Framework (defaultMain)
import Tests.Module.Data.CapNProto.Bits (bitsTests)
import Tests.Module.Data.CapNProto.Pointer (ptrTests)
import Tests.Module.Data.CapNProto.Untyped (untypedTests)
import Tests.Module.Control.Monad.CapNProto.MessageBuilder (buildTests)

import Tests.EncodeDecodeUntyped (encodeDecodeUntypedTests)

main :: IO ()
main = defaultMain [ bitsTests
                   , ptrTests
                   , untypedTests
                   , buildTests
                   , encodeDecodeUntypedTests
                   ]
