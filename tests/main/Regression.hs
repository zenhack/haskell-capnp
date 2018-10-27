{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Regression (regressionTests) where

import Test.Hspec

import Capnp (bsToValue, def)

import Capnp.Gen.Capnp.Rpc.Pure

regressionTests :: Spec
regressionTests = describe "Regression tests" $
    it "Should decode abort message successfully (issue #56)" $ do
        let bytes =
                "\NUL\NUL\NUL\NUL\ETB\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL" <>
                "\SOH\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL" <>
                "\SOH\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL" <>
                "\NUL\NULz\EOT\NUL\NULYour vat sent an 'unimplemented' " <>
                "message for an abort message that its remote peer never " <>
                "sent. This is likely a bug in your capnproto library.\NUL\NUL"
        msg <- bsToValue bytes
        msg `shouldBe` Message'abort def
            { reason =
                "Your vat sent an 'unimplemented' message for an abort " <>
                "message that its remote peer never sent. This is likely " <>
                "a bug in your capnproto library."
            , type_ = Exception'Type'failed
            }
