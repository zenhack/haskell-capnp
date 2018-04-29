{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Tests.Module.Data.CapNProto.Core.Schema (schemaTests) where

import Data.CapNProto.Core.Schema
import Tests.Util

import Data.CapNProto.TraversalLimit (evalWithLimit)
import Data.CapNProto.Untyped.ADT    (readStruct)
import Test.Framework                (testGroup)
import Test.HUnit                    (assertEqual)
import Text.Heredoc                  (here, there)

import qualified Data.CapNProto.Untyped as U

schemaTests = testGroup "schema decode tests"
    [ decodeTests "Value" readValue
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
        , ("(interface = void)", Value Value'Interface)
        -- TODO: It would be nice to test list, struct, and anyPointer
        -- variants, but I(zenhack) haven't figured out how to specify
        -- an AnyPointer in the input to capnp encode. Maybe capnp eval
        -- can do something like this? will have to investigate.
        ]
    , decodeTests "CapnpVersion" readCapnpVersion
        [ ("(major = 0, minor = 5, micro = 3)", CapnpVersion 0 5 3)
        , ("(major = 1, minor = 0, micro = 2)", CapnpVersion 1 0 2)
        ]
    , decodeTests "Superclass" readSuperclass
        [ ("(id = 34, brand = (scopes = []))", Superclass 34 (Brand []))
        ]
    , decodeTests "Type" readType
        [ ("(bool = void)", Type $ Type'Bool)
        , ("(int8 = void)", Type $ Type'Int8)
        , ("(int16 = void)", Type $ Type'Int16)
        , ("(int32 = void)", Type $ Type'Int32)
        , ("(int64 = void)", Type $ Type'Int64)
        , ("(uint8 = void)", Type $ Type'Uint8)
        , ("(uint16 = void)", Type $ Type'Uint16)
        , ("(uint32 = void)", Type $ Type'Uint32)
        , ("(uint64 = void)", Type $ Type'Uint64)
        , ("(float32 = void)", Type $ Type'Float32)
        , ("(float64 = void)", Type $ Type'Float64)
        , ("(text = void)", Type $ Type'Text)
        , ("(data = void)", Type $ Type'Data)
        , ( "(list = (elementType = (list = (elementType = (text = void)))))"
          , Type $ Type'List $ Type $ Type'List $ Type Type'Text
          )
        , ( "(enum = (typeId = 4, brand = (scopes = [])))"
          , Type $ Type'Enum 4 (Brand [])
          )
        , ( "(struct = (typeId = 7, brand = (scopes = [])))"
          , Type $ Type'Struct 7 (Brand [])
          )
        , ( "(interface = (typeId = 1, brand = (scopes = [])))"
          , Type $ Type'Interface 1 (Brand [])
          )
        ]
    , decodeTests "Brand" readBrand
        [ ("(scopes = [])", Brand [])
        , ( [here|
                ( scopes =
                    [ (scopeId = 32, inherit = void)
                    , (scopeId = 23, bind =
                        [ (unbound = void)
                        , (type = (bool = void))
                        ]
                      )
                    ]
                )
            |]
          , Brand
                [ Brand'Scope 32 Brand'Scope'Inherit
                , Brand'Scope 23 $ Brand'Scope'Bind
                    [ Brand'Binding'Unbound
                    , Brand'Binding'Type $ Type Type'Bool
                    ]
                ]
          )
        ]
    ]
  where
    decodeTests typename reader cases =
        assertionsToTest ("Decode " ++ typename) $ map (testCase typename reader) cases
    testCase typename reader (capnpText, expected) = do
        msg <- encodeValue [there|tests/data/schema.capnp|] typename capnpText
        actual <- evalWithLimit 128 $ U.rootPtr msg >>= readStruct >>= reader
        assertEqual (show (capnpText, expected)) expected actual
