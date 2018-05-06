module Main (main) where

import Test.Framework                                      (defaultMain)
import Tests.EncodeDecodeUntyped
    (encodeDecodeUntypedTests)
import Tests.Module.Control.Monad.CapNProto.MessageBuilder (buildTests)
import Tests.Module.Data.CapNProto.Bits                    (bitsTests)
import Tests.Module.Data.CapNProto.Core.Schema             (schemaTests)
import Tests.Module.Data.CapNProto.Pointer                 (ptrTests)
import Tests.Module.Data.CapNProto.Untyped                 (untypedTests)
import Tests.Module.Data.CapNProto.Untyped.Pure            (pureUntypedTests)

-- These tests are currently broken, due to the removed TH-based code
-- generation, but a version of them may be useful again at some point:
--
-- import Tests.Module.Data.CapNProto.List (listTests)
-- import Tests.SchemaQuickCheck           (schemaCGRQuickCheck)
-- import Tests.WalkSchemaCodeGenRequest   (walkSchemaCodeGenRequestTest)

main :: IO ()
main = defaultMain [ bitsTests
                   , ptrTests
                   , untypedTests
                   , pureUntypedTests
                   -- , listTests
                   , buildTests
                   , encodeDecodeUntypedTests
                   -- , walkSchemaCodeGenRequestTest
                   -- , schemaCGRQuickCheck
                   , schemaTests
                   ]
