{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Tests.Module.Capnp.Untyped.Pure (pureUntypedTests) where

import Test.Hspec

import Data.ReinterpretCast (doubleToWord)
import Text.Heredoc         (here, there)

import qualified Data.Vector as V

import Capnp.Untyped.Pure
import Util

import Capnp.Classes        (decerialize)
import Capnp.TraversalLimit (LimitT, runLimitT)

import qualified Capnp.Message as M
import qualified Capnp.Untyped as U

schemaText = [there|tests/data/aircraft.capnp|]

-- This is analogous to Tests.Module.Capnp.Untyped.untypedTests, but
-- using the Pure module:
pureUntypedTests =
    describe "high-level untyped decoding" $
        it "Should agree with `capnp decode`" $ do
            msg <- encodeValue
                        schemaText
                        "Aircraft"
                        [here|(f16 = (base = (
                           name = "bob",
                           homes = [],
                           rating = 7,
                           canFly = true,
                           capacity = 5173,
                           maxSpeed = 12.0
                        )))|]
            (actual, 110) <- runLimitT 128 $ U.rootPtr msg >>= readStruct
            actual `shouldBe` Struct
                [3]
                [ Just $ PtrStruct $ Struct
                   []
                    [ Just $ PtrStruct $ Struct
                        [ 7
                        , 1
                        , 5173
                        , doubleToWord 12.0
                        ]
                        [ Just $ PtrList $ List8 $ V.fromList $ map (fromIntegral . fromEnum) "bob\0"
                        , Just $ PtrList $ List16 []
                        ]
                    ]
                ]
  where
    readStruct :: U.Struct M.ConstMsg -> LimitT IO Struct
    readStruct = decerialize
