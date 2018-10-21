{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Tests.Module.Capnp.Untyped.Pure (pureUntypedTests) where

import Data.ReinterpretCast (doubleToWord)
import Test.Framework       (Test)
import Test.HUnit           (assertEqual)
import Text.Heredoc         (here, there)

import qualified Data.Vector as V

import Capnp.Untyped.Pure
import Tests.Util

import Capnp.Classes        (decerialize)
import Capnp.TraversalLimit (LimitT, runLimitT)

import qualified Capnp.Message as M
import qualified Capnp.Untyped as U

-- This is analogous to Tests.Module.Capnp.Untyped.untypedTests, but
-- using the Pure module:
pureUntypedTests :: Test
pureUntypedTests = assertionsToTest "Untyped ADT Tests"
    [ do
        msg <- encodeValue
                    [there|tests/data/aircraft.capnp|]
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
        assertEqual
            "Untyped ADT test (assertEqual)"
            (Struct
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
                ])
            actual
    ]
  where
    readStruct :: U.Struct M.ConstMsg -> LimitT IO Struct
    readStruct = decerialize
