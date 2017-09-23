module Main where


import Test.Framework                                      (defaultMain)
import Tests.EncodeDecodeUntyped
    (encodeDecodeUntypedTests)
import Tests.Module.Control.Monad.CapNProto.MessageBuilder (buildTests)
import Tests.Module.Data.CapNProto.Bits                    (bitsTests)
import Tests.Module.Data.CapNProto.Pointer                 (ptrTests)
import Tests.Module.Data.CapNProto.Untyped                 (untypedTests)
import Tests.WalkSchemaCodeGenRequest
    (walkSchemaCodeGenRequestTest)

main :: IO ()
main = defaultMain [ bitsTests
                   , ptrTests
                   , untypedTests
                   , buildTests
                   , encodeDecodeUntypedTests
                   , walkSchemaCodeGenRequestTest
                   ]
