{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Tests.Module.Data.CapNProto.Untyped.ADT (untypedADTTests) where

import Data.CapNProto.Untyped.ADT
import Tests.Util

import Data.CapNProto.TraversalLimit (runWithLimit)
import Data.ReinterpretCast          (doubleToWord)
import Test.Framework                (Test)
import Test.HUnit                    (assertEqual)
import Text.Heredoc                  (here, there)

import qualified Data.CapNProto.Untyped as U
import qualified Data.Vector            as V

-- This is analogous to Tests.Module.Data.CapNProto.Untyped.untypedTests, but
-- using the ADT module:
untypedADTTests :: Test
untypedADTTests = assertionsToTest "Untyped ADT Tests"
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
        (actual, 110) <- runWithLimit 128 $ U.rootPtr msg >>= readStruct
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
                        [ Just $ PtrList $ List8' $ List $ V.fromList $ map (fromIntegral . fromEnum) "bob\0"
                        , Just $ PtrList $ List16' []
                        ]
                    ]
                ])
            actual
    ]
