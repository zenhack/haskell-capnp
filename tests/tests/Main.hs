module Main (main) where

import Test.Hspec

import Tests.Module.Capnp.Bits                  (bitsTests)
import Tests.Module.Capnp.Gen.Capnp.Schema      (schemaTests)
import Tests.Module.Capnp.Gen.Capnp.Schema.Pure (pureSchemaTests)
import Tests.Module.Capnp.Pointer               (ptrTests)
import Tests.Module.Capnp.Rpc                   (rpcTests)
import Tests.Module.Capnp.Untyped               (untypedTests)
import Tests.Module.Capnp.Untyped.Pure          (pureUntypedTests)
import Tests.SchemaQuickCheck                   (schemaCGRQuickCheck)
import Tests.WalkSchemaCodeGenRequest           (walkSchemaCodeGenRequestTest)

main :: IO ()
main = hspec $ do
    describe "Tests for specific modules" $ do
        describe "Capnp.Bits" bitsTests
        describe "Capnp.Pointer" ptrTests
        describe "Capnp.Rpc" rpcTests
        describe "Capnp.Untyped" untypedTests
        describe "Capnp.Untyped.Pure" pureUntypedTests
    describe "Tests for generated output" $ do
        describe "low-level output" schemaTests
        describe "high-level output" pureSchemaTests
    describe "Tests relate to schema" $ do
        describe "tests using tests/data/schema-codegenreq" walkSchemaCodeGenRequestTest
        describe "property tests for schema" schemaCGRQuickCheck
