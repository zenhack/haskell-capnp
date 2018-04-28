module Main (main) where

import Test.Framework                                      (defaultMain)
import Tests.EncodeDecodeUntyped
    (encodeDecodeUntypedTests)
import Tests.Module.Control.Monad.CapNProto.MessageBuilder (buildTests)
import Tests.Module.Data.CapNProto.Bits                    (bitsTests)
import Tests.Module.Data.CapNProto.Core.Schema             (schemaTests)
import Tests.Module.Data.CapNProto.List                    (listTests)
import Tests.Module.Data.CapNProto.Pointer                 (ptrTests)
import Tests.Module.Data.CapNProto.Untyped                 (untypedTests)
import Tests.Module.Data.CapNProto.Untyped.ADT             (untypedADTTests)
import Tests.WalkSchemaCodeGenRequest
    (walkSchemaCodeGenRequestTest)

import Tests.SchemaQuickCheck (schemaCGRQuickCheck)

main :: IO ()
main = defaultMain [ bitsTests
                   , ptrTests
                   , untypedTests
                   , untypedADTTests
                   , listTests
                   , buildTests
                   , encodeDecodeUntypedTests
                   , walkSchemaCodeGenRequestTest
                   , schemaCGRQuickCheck
                   , schemaTests
                   ]
