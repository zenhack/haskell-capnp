module Main (main) where

import Test.Hspec

import Module.Capnp.Basics                (basicsTests)
import Module.Capnp.Bits                  (bitsTests)
import Module.Capnp.Gen.Capnp.Schema      (schemaTests)
import Module.Capnp.Gen.Capnp.Schema.Pure (pureSchemaTests)
import Module.Capnp.Pointer               (ptrTests)
import Module.Capnp.Rpc                   (rpcTests)
import Module.Capnp.Untyped               (untypedTests)
import Module.Capnp.Untyped.Pure          (pureUntypedTests)
import Regression                         (regressionTests)
import SchemaQuickCheck                   (schemaCGRQuickCheck)
import WalkSchemaCodeGenRequest           (walkSchemaCodeGenRequestTest)

import qualified Capnp.Rpc.Untyped

main :: IO ()
main = hspec $ parallel $ do
    describe "Tests for specific modules" $ do
        describe "Capnp.Basics" basicsTests
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
    describe "Regression tests" regressionTests
    describe "Self-tests for individual modules"
        Capnp.Rpc.Untyped.selfTests
