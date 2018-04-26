{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Tests.Module.Data.CapNProto.Untyped.ADT (untypedADTTests) where

import Data.CapNProto.Untyped.ADT
import Tests.Util

import Data.Bits                     (shiftR)
import Data.CapNProto.TraversalLimit (evalWithLimit)
import Data.ReinterpretCast          (doubleToWord)
import Data.Word                     (Word8)
import Test.Framework                (Test)
import Test.HUnit                    (assertEqual)
import Text.Heredoc                  (here, there)

import qualified Data.ByteString        as BS
import qualified Data.CapNProto.Message as M
import qualified Data.CapNProto.Untyped as U
import qualified Data.Vector            as V

aircraftSchema :: String
aircraftSchema = [there|tests/data/aircraft.capnp|]

doubleToBytes :: Double -> [Word8]
doubleToBytes num =
    let word = doubleToWord 12.0
    in map fromIntegral
        [ word `shiftR` 0
        , word `shiftR` 8
        , word `shiftR` 16
        , word `shiftR` 24
        , word `shiftR` 32
        , word `shiftR` 40
        , word `shiftR` 48
        , word `shiftR` 56
        ]

untypedADTTests :: Test
untypedADTTests = assertionsToTest "Untyped ADT Tests"  $ map tst
    [ ( aircraftSchema
      , "Aircraft"
      , [here|(f16 = (base = (
           name = "bob",
           homes = [],
           rating = 7,
           canFly = true,
           capacity = 5173,
           maxSpeed = 12.0
        )))|]
      , 128
      , Struct
            (BS.pack [3,0,0,0,0,0,0,0])
            [ Just $ PtrStruct $ Struct
                ""
                [ Just $ PtrStruct $ Struct
                    (BS.pack $
                        [7 ,0 ,0,0,0,0,0,0
                        ,1 ,0 ,0,0,0,0,0,0
                        ,53,20,0,0,0,0,0,0
                        ] ++ doubleToBytes 12.0)
                    [ Just $ PtrList $ List8' $ List $ V.fromList $ map (fromIntegral . fromEnum) "bob\0"
                    , Just $ PtrList $ List16' []
                    ]
                ]
            ]
      )
    ]
  where
    tst :: ( String
           , String
           , String
           , Int
           , Struct
           ) -> IO ()
    tst (schema, typename, value, quota, expected) = do
        let meta = MsgMetaData schema typename
        msg <- capnpEncode value meta >>= M.decode
        actual <- evalWithLimit quota $ U.rootPtr msg >>= readStruct
        assertEqual (show (meta, value)) expected actual
