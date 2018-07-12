module Main (main) where

import Test.Framework (defaultMain)
-- import Tests.EncodeDecodeUntyped       (encodeDecodeUntypedTests)
import Tests.Module.Capnp.Capnp.Schema (schemaTests)
import Tests.Module.Data.Capnp.Bits    (bitsTests)
-- import Tests.Module.Data.Capnp.Builder      (buildTests)
import Tests.Module.Data.Capnp.Pointer         (ptrTests)
import Tests.Module.Data.Capnp.Untyped         (untypedTests)
import Tests.Module.Data.Capnp.Untyped.Generic (genericUntypedTests)
import Tests.Module.Data.Capnp.Untyped.Pure    (pureUntypedTests)
import Tests.SchemaQuickCheck                  (schemaCGRQuickCheck)
import Tests.WalkSchemaCodeGenRequest          (walkSchemaCodeGenRequestTest)

main :: IO ()
main = defaultMain [ bitsTests
                   , ptrTests
                   , untypedTests
                   , genericUntypedTests
                   , pureUntypedTests
                   -- , buildTests
                   -- , encodeDecodeUntypedTests
                   , walkSchemaCodeGenRequestTest
                   , schemaCGRQuickCheck
                   , schemaTests
                   ]
