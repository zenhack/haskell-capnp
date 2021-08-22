{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Module.Capnp.Untyped.Pure (pureUntypedTests) where

import Test.Hspec

import GHC.Float    (castDoubleToWord64)
import Text.Heredoc (here)

import qualified Data.Vector as V

import Capnp.New.Basics
import Util

import Capnp.New            (msgToRaw, parse)
import Capnp.TraversalLimit (runLimitT)

-- This is analogous to Tests.Module.Capnp.Untyped.untypedTests, but
-- using the Pure module:
pureUntypedTests :: Spec
pureUntypedTests =
    describe "high-level untyped decoding" $
        it "Should agree with `capnp decode`" $ do
            msg <- encodeValue
                        aircraftSchemaSrc
                        "Aircraft"
                        [here|(f16 = (base = (
                           name = "bob",
                           homes = [],
                           rating = 7,
                           canFly = true,
                           capacity = 5173,
                           maxSpeed = 12.0
                        )))|]
            (actual, 117) <- runLimitT 128 $ msgToRaw msg >>= parse
            actual `shouldBe` Struct
                [3]
                [ Just $ PtrStruct $ Struct
                   []
                    [ Just $ PtrStruct $ Struct
                        [ 7
                        , 1
                        , 5173
                        , castDoubleToWord64 12.0
                        ]
                        [ Just $ PtrList $ List8 $ V.fromList $ map (fromIntegral . fromEnum) "bob\0"
                        , Just $ PtrList $ List16 []
                        ]
                    ]
                ]
