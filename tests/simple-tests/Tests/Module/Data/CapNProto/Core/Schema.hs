{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Tests.Module.Data.CapNProto.Core.Schema (schemaTests) where

import Data.CapNProto.Core.Schema
import Tests.Util

import Data.CapNProto.TraversalLimit (evalWithLimit)
import Data.CapNProto.Untyped.ADT    (readStruct)
import Test.HUnit                    (assertEqual)
import Text.Heredoc                  (there)

import qualified Data.CapNProto.Untyped as U

schemaTests = valueTests

valueTests = assertionsToTest "Decode values" $ map testCase
    [ ("(bool = true)", Value $ Value'Bool True)
    , ("(bool = false)", Value $ Value'Bool False)
    , ("(int8 = -4)", Value $ Value'Int8 (-4))
    , ("(int8 = -128)", Value $ Value'Int8 (-128))
    , ("(int8 = 127)", Value $ Value'Int8 127)
    , ("(uint8 = 23)", Value $ Value'Uint8 23)
    , ("(uint8 = 255)", Value $ Value'Uint8 255)
    , ("(int16 = 1012)", Value $ Value'Int16 1012)
    , ("(uint16 = 40000)", Value $ Value'Uint16 40000)
    , ("(uint32 = 1000100)", Value $ Value'Uint32 1000100)
    , ("(int32 = 1000100)", Value $ Value'Int32 1000100)
    , ("(uint64 = 1234567890123456)", Value $ Value'Uint64 1234567890123456)
    , ("(int64 = 12345678901234567)", Value $ Value'Int64 12345678901234567)
    , ("(float32 = 17.32)", Value $ Value'Float32 17.32)
    , ("(float64 = 13.99)", Value $ Value'Float64 13.99)
    , ("(data = \"beep boop.\")", Value $ Value'Data "beep boop.")
    , ("(text = \"Hello, World!\")", Value $ Value'Text "Hello, World!")
    , ("(enum = 2313)", Value $ Value'Enum 2313)
    -- TODO: It would be nice to test list, struct, interface, and anyPointer
    -- variants, but I(zenhack) haven't figured out what those should look
    -- like in the input to capnp encode.
    ]
  where
    testCase (capnpText, expected) = do
        msg <- encodeValue [there|tests/data/schema.capnp|] "Value" capnpText
        actual <- evalWithLimit 128 $ U.rootPtr msg >>= readStruct >>= readValue
        assertEqual (show (capnpText, expected)) expected actual
