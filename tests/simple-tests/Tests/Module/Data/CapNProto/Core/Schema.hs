{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Tests.Module.Data.CapNProto.Core.Schema (schemaTests) where

import Codec.CapNProto (Decerialize(..))

import Data.CapNProto.Core.Schema
import Tests.Util

import Data.CapNProto.TraversalLimit (evalWithLimit)
import Data.CapNProto.Untyped.Pure   (readStruct)
import Test.Framework                (testGroup)
import Test.HUnit                    (assertEqual)
import Text.Heredoc                  (here, there)

import qualified Data.CapNProto.Untyped as U

schemaTests = testGroup "schema decode tests"
    [ decodeTests "CodeGeneratorRequest"
        [ ( [here|
                ( capnpVersion = (major = 0, minor = 6, micro = 1)
                , nodes = []
                , requestedFiles =
                    [ ( id = 4
                      , filename = "hello.capnp"
                      , imports =
                          [ (id = 2, name = "std")
                          ]
                      )
                    ]
                )
            |]
          , CodeGeneratorRequest
                (CapnpVersion 0 6 1)
                []
                [ CodeGeneratorRequest'RequestedFile
                    4
                    "hello.capnp"
                    [CodeGeneratorRequest'RequestedFile'Import 2 "std"]
                ]
          )
        ]
    , decodeTests "Node"
        [ ( [here|
                ( id = 7
                , displayName = "foo:MyType"
                , displayNamePrefixLength = 4
                , scopeId = 2
                , parameters = [ (name = "theName") ]
                , isGeneric = true
                , nestedNodes = [(name = "theName", id = 321)]
                , annotations = [ (id = 2, brand = (scopes = []), value = (bool = true)) ]
                , |] ++ unionText ++ [here|
                )
            |]
          , Node
                7
                "foo:MyType"
                4
                2
                [Node'Parameter "theName"]
                True
                [Node'NestedNode "theName" 321]
                [Annotation 2 (Brand []) (Value $ Value'Bool True)]
                unionVal
          )
        | (unionText, unionVal) <-
            [ ("file = void", Node'File)
            , ( [here| struct =
                    ( dataWordCount = 3
                    , pointerCount = 2
                    , preferredListEncoding = inlineComposite
                    , isGroup = false
                    , discriminantCount = 4
                    , discriminantOffset = 2
                    , fields =
                        [ ( name = "fieldName"
                          , codeOrder = 3
                          , annotations = [ (id = 2, brand = (scopes = []), value = (bool = true)) ]
                          , discriminantValue = 3
                          , group = (typeId = 4)
                          , ordinal = (implicit = void)
                          )
                        ]
                    )
                |]
              , Node'Struct
                    3
                    2
                    InlineComposite
                    False
                    4
                    2
                    [ Field
                        "fieldName"
                        3
                        [Annotation 2 (Brand []) (Value $ Value'Bool True)]
                        3
                        (Field'Group $ Field'Group' 4)
                        Field'Ordinal'Implicit
                    ]
              )
            , ( "enum = (enumerants = [(name = \"blue\", codeOrder = 2, annotations = [])])"
              , Node'Enum [Enumerant "blue" 2 []]
              )
            , ( "interface = (methods = [], superclasses = [(id = 0, brand = (scopes = []))])"
              , Node'Interface [] [Superclass 0 (Brand [])]
              )
            , ( "const = (type = (bool = void), value = (bool = false))"
              , Node'Const
                    (Type Type'Bool)
                    (Value $ Value'Bool False)
              )
            , ( [here| annotation =
                    ( type = (bool = void)
                    , targetsFile = true
                    , targetsConst = false
                    , targetsEnum = false
                    , targetsEnumerant = true
                    , targetsStruct = true
                    , targetsField = true
                    , targetsUnion = false
                    , targetsGroup = false
                    , targetsInterface = true
                    , targetsMethod = false
                    , targetsParam = true
                    , targetsAnnotation = false
                    )
                |]
              , Node'Annotation
                    (Type Type'Bool)
                    True
                    False
                    False
                    True
                    True
                    True
                    False
                    False
                    True
                    False
                    True
                    False
              )
            ]
        ]
    , decodeTests "Node.Parameter"
        [ ("(name = \"theName\")", Node'Parameter "theName")
        ]
    , decodeTests "Node.NestedNode"
        [ ("(name = \"theName\", id = 321)", Node'NestedNode "theName" 321)
        ]
    , decodeTests "Value"
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
    , decodeTests "Annotation"
        [ ( "(id = 323, brand = (scopes = []), value = (bool = true))"
          , Annotation 323 (Brand []) (Value $ Value'Bool True)
          )
        ]
    , decodeTests "CapnpVersion"
        [ ("(major = 0, minor = 5, micro = 3)", CapnpVersion 0 5 3)
        , ("(major = 1, minor = 0, micro = 2)", CapnpVersion 1 0 2)
        ]
    , decodeTests "Field"
        [ ( [here|
                ( name = "fieldName"
                , codeOrder = 3
                , annotations = [ (id = 2, brand = (scopes = []), value = (bool = true)) ]
                , discriminantValue = 3
                , group = (typeId = 4)
                , ordinal = (implicit = void)
                )
            |]
          , Field
                "fieldName"
                3
                [Annotation 2 (Brand []) (Value $ Value'Bool True)]
                3
                (Field'Group $ Field'Group' 4)
                Field'Ordinal'Implicit
          )
        , ( [here|
                ( name = "fieldName"
                , codeOrder = 3
                , annotations = [ (id = 2, brand = (scopes = []), value = (bool = true)) ]
                , discriminantValue = 3
                , slot =
                    ( offset = 3
                    , type = (bool = void)
                    , defaultValue = (bool = false)
                    , hadExplicitDefault = true
                    )
                , ordinal = (explicit = 7)
                )
            |]
          , Field
                "fieldName"
                3
                [Annotation 2 (Brand []) (Value $ Value'Bool True)]
                3
                (Field'Slot $ Field'Slot'
                    3
                    (Type Type'Bool)
                    (Value $ Value'Bool False)
                    True)
                (Field'Ordinal'Explicit 7)
          )
        ]
    , decodeTests "Enumerant"
        [ ( [here|
                ( name = "red"
                , codeOrder = 4
                , annotations =
                    [ (id = 23, brand = (scopes = []), value = (uint8 = 3))
                    ]
                )
            |]
          , Enumerant "red" 4 [Annotation 23 (Brand []) (Value $ Value'Uint8 3)]
          )
        ]
    , decodeTests "Superclass"
        [ ("(id = 34, brand = (scopes = []))", Superclass 34 (Brand []))
        ]
    , decodeTests "Type"
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
        , ( "(anyPointer = (unconstrained = (anyKind = void)))"
          , Type $ Type'AnyPointer $ Type'AnyPointer'Unconstrained $ Unconstrained'AnyKind
          )
        , ( "(anyPointer = (unconstrained = (struct = void)))"
          , Type $ Type'AnyPointer $ Type'AnyPointer'Unconstrained $ Unconstrained'Struct
          )
        , ( "(anyPointer = (unconstrained = (list = void)))"
          , Type $ Type'AnyPointer $ Type'AnyPointer'Unconstrained $ Unconstrained'List
          )
        , ( "(anyPointer = (unconstrained = (capability = void)))"
          , Type $ Type'AnyPointer $ Type'AnyPointer'Unconstrained $ Unconstrained'Capability
          )
        , ( "(anyPointer = (parameter = (scopeId = 4, parameterIndex = 2)))"
          , Type $ Type'AnyPointer $ Type'AnyPointer'Parameter 4 2
          )
        , ( "(anyPointer = (implicitMethodParameter = (parameterIndex = 7)))"
          , Type $ Type'AnyPointer $ Type'AnyPointer'ImplicitMethodParameter 7
          )
        ]
    , decodeTests "Brand"
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
    -- decodeTests :: Decerialize Struct a => String -> [(String, a)] -> IO ()
    decodeTests typename cases =
        assertionsToTest ("Decode " ++ typename) $ map (testCase typename) cases
    testCase typename (capnpText, expected) = do
        msg <- encodeValue schemaText typename capnpText
        actual <- evalWithLimit 128 $ U.rootPtr msg >>= readStruct >>= decerialize
        assertEqual (show (capnpText, expected)) expected actual
    schemaText = [there|tests/data/schema.capnp|]
