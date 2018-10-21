module Main (main) where

import Test.Framework (defaultMain)

import Tests.Module.Capnp.Bits                  (bitsTests)
import Tests.Module.Capnp.Gen.Capnp.Schema      (schemaTests)
import Tests.Module.Capnp.Gen.Capnp.Schema.Pure (pureSchemaTests)
import Tests.Module.Capnp.Pointer               (ptrTests)
import Tests.Module.Capnp.Untyped               (untypedTests)
import Tests.Module.Capnp.Untyped.Pure          (pureUntypedTests)
import Tests.SchemaQuickCheck                   (schemaCGRQuickCheck)
import Tests.WalkSchemaCodeGenRequest           (walkSchemaCodeGenRequestTest)

main :: IO ()
main = defaultMain [ bitsTests
                   , ptrTests
                   , untypedTests
                   , pureUntypedTests
                   , walkSchemaCodeGenRequestTest
                   , schemaCGRQuickCheck
                   , schemaTests
                   , pureSchemaTests
                   ]
