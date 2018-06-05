module Main (main) where

import Test.Framework                                  (defaultMain)
import Tests.EncodeDecodeUntyped
    (encodeDecodeUntypedTests)
import Tests.Module.Control.Monad.Capnp.MessageBuilder (buildTests)
import Tests.Module.Data.Capnp.Bits                    (bitsTests)
import Tests.Module.Data.Capnp.List                    (listTests)
import Tests.Module.Data.Capnp.Pointer                 (ptrTests)
import Tests.Module.Data.Capnp.Schema.Capnp.Schema     (schemaTests)
import Tests.Module.Data.Capnp.Untyped                 (untypedTests)
import Tests.Module.Data.Capnp.Untyped.Pure            (pureUntypedTests)
import Tests.SchemaQuickCheck                          (schemaCGRQuickCheck)
import Tests.WalkSchemaCodeGenRequest
    (walkSchemaCodeGenRequestTest)

main :: IO ()
main = defaultMain [ bitsTests
                   , ptrTests
                   , untypedTests
                   , pureUntypedTests
                   , listTests
                   , buildTests
                   , encodeDecodeUntypedTests
                   , walkSchemaCodeGenRequestTest
                   , schemaCGRQuickCheck
                   , schemaTests
                   ]
