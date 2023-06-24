module Main (main) where

import qualified CalculatorExample
import qualified Constants
import Module.Capnp.Basics (basicsTests)
import Module.Capnp.Bits (bitsTests)
import Module.Capnp.Canonicalize (canonicalizeTests)
import Module.Capnp.Gen.Capnp.Schema (schemaTests)
import Module.Capnp.Gen.Capnp.Schema.Pure (pureSchemaTests)
import Module.Capnp.Pointer (ptrTests)
import Module.Capnp.Rpc (rpcTests)
import Module.Capnp.Untyped (untypedTests)
import Module.Capnp.Untyped.Pure (pureUntypedTests)
import qualified PointerOOB
import Regression (regressionTests)
import Rpc.Unwrap (unwrapTests)
import SchemaQuickCheck (schemaCGRQuickCheck)
import Test.Hspec
import WalkSchemaCodeGenRequest (walkSchemaCodeGenRequestTest)

main :: IO ()
main = hspec $ do
  describe "Tests for specific modules" $ do
    describe "Capnp.Basics" basicsTests
    describe "Capnp.Bits" bitsTests
    describe "Capnp.Pointer" ptrTests
    describe "Capnp.Rpc" rpcTests
    describe "Capnp.Untyped" untypedTests
    describe "Capnp.Untyped.Pure" pureUntypedTests
    describe "Capnp.Canonicalize" canonicalizeTests
  describe "Tests for generated output" $ do
    describe "low-level output" schemaTests
    describe "high-level output" pureSchemaTests
  describe "Tests relate to schema" $ do
    describe "tests using tests/data/schema-codegenreq" walkSchemaCodeGenRequestTest
    describe "property tests for schema" schemaCGRQuickCheck
  describe "Regression tests" regressionTests
  CalculatorExample.tests
  PointerOOB.tests
  Constants.tests
  describe "Tests for client unwrapping" unwrapTests
