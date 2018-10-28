{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Regression (regressionTests) where

import Test.Hspec

import Capnp (bsToValue, def)

import Capnp.Gen.Aircraft.Pure
import Capnp.Gen.Capnp.Rpc.Pure

regressionTests :: Spec
regressionTests = describe "Regression tests" $ do
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
    it "Should decode negative default values correctly (issue #55)" $ do
        -- Note that this was never actually broken, but we were getting
        -- a warning about a literal overflowing the bounds of its type.
        -- It worked anyway, since it became the right value after casting,
        -- but the warning has been fixed and this test makes sure it still
        -- actually works.
        let Defaults{int} = def
        int `shouldBe` (-123)
