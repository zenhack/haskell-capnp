{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Tests.Module.Data.Capnp.Core.Schema (schemaTests) where

import Codec.Capnp (Decerialize(..))

import Data.Capnp.Core.Schema
import Tests.Util

import Data.Capnp.TraversalLimit (evalWithLimit)
import Data.Capnp.Untyped.Pure   (readStruct)
import Test.Framework            (testGroup)
import Test.HUnit                (assertEqual)
import Text.Heredoc              (here, there)

import qualified Data.Capnp.Untyped as U

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
                { capnpVersion = CapnpVersion { major = 0, minor = 6, micro = 1 }
                , nodes = []
                , requestedFiles =
                    [ CodeGeneratorRequest'RequestedFile
                        4
                        "hello.capnp"
                        [CodeGeneratorRequest'RequestedFile'Import 2 "std"]
                    ]
                }
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
          , Node'
                7
                "foo:MyType"
                4
                2
                [Node'NestedNode "theName" 321]
                [Annotation 2 (Value'bool True) (Brand [])]
                [Node'Parameter "theName" ]
                True
                unionVal
          )
        | (unionText, unionVal) <-
            [ ("file = void", Node'file)
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
                          , discriminantValue = 7
                          , group = (typeId = 4)
                          , ordinal = (implicit = void)
                          )
                        ]
                    )
                |]
              , Node'struct
                    { dataWordCount = 3
                    , pointerCount = 2
                    , preferredListEncoding = ElementSize'inlineComposite
                    , isGroup = False
                    , discriminantCount = 4
                    , discriminantOffset = 2
                    , fields =
                        [ Field'
                            "fieldName"
                            3
                            [ Annotation
                                2
                                (Value'bool True)
                                (Brand [])
                            ]
                            7
                            Field'ordinal'implicit
                            (Field'group 4)
                        ]
                    }
              )
            , ( "enum = (enumerants = [(name = \"blue\", codeOrder = 2, annotations = [])])"
              , Node'enum [ Enumerant "blue" 2 [] ]
              )
            , ( "interface = (methods = [], superclasses = [(id = 0, brand = (scopes = []))])"
              , Node'interface [] [Superclass 0 (Brand [])]
              )
            , ( "const = (type = (bool = void), value = (bool = false))"
              , Node'const Type'bool (Value'bool False)
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
              , Node'annotation
                    Type'bool
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
        [ ("(name = \"theName\")", Node'Parameter "theName" )
        ]
    , decodeTests "Node.NestedNode"
        [ ("(name = \"theName\", id = 321)", Node'NestedNode "theName" 321)
        ]
    , decodeTests "Value"
        [ ("(bool = true)", Value'bool True)
        , ("(bool = false)", Value'bool False)
        , ("(int8 = -4)", Value'int8 (-4))
        , ("(int8 = -128)", Value'int8 (-128))
        , ("(int8 = 127)", Value'int8 127)
        , ("(uint8 = 23)", Value'uint8 23)
        , ("(uint8 = 255)", Value'uint8 255)
        , ("(int16 = 1012)", Value'int16 1012)
        , ("(uint16 = 40000)", Value'uint16 40000)
        , ("(uint32 = 1000100)", Value'uint32 1000100)
        , ("(int32 = 1000100)", Value'int32 1000100)
        , ("(uint64 = 1234567890123456)", Value'uint64 1234567890123456)
        , ("(int64 = 12345678901234567)", Value'int64 12345678901234567)
        , ("(float32 = 17.32)", Value'float32 17.32)
        , ("(float64 = 13.99)", Value'float64 13.99)
        , ("(data = \"beep boop.\")", Value'data_ "beep boop.")
        , ("(text = \"Hello, World!\")", Value'text "Hello, World!")
        , ("(enum = 2313)", Value'enum 2313)
        , ("(interface = void)", Value'interface)
        -- TODO: It would be nice to test list, struct, and anyPointer
        -- variants, but I(zenhack) haven't figured out how to specify
        -- an AnyPointer in the input to capnp encode. Maybe capnp eval
        -- can do something like this? will have to investigate.
        ]
    , decodeTests "Annotation"
        [ ( "(id = 323, brand = (scopes = []), value = (bool = true))"
          , Annotation 323 (Value'bool True) (Brand [])
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
          , Field'
                "fieldName"
                3
                [Annotation 2 (Value'bool True) (Brand [])]
                3
                Field'ordinal'implicit
                (Field'group 4)
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
          , Field'
                "fieldName"
                3
                [Annotation 2 (Value'bool True) (Brand [])]
                3
                (Field'ordinal'explicit 7)
                (Field'slot
                    3
                    Type'bool
                    (Value'bool False)
                    True)
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
          , Enumerant "red" 4 [Annotation 23 (Value'uint8 3) (Brand [])]
          )
        ]
    , decodeTests "Superclass"
        [ ("(id = 34, brand = (scopes = []))", Superclass 34 (Brand []))
        ]
    , decodeTests "Type"
        [ ("(bool = void)", Type'bool)
        , ("(int8 = void)", Type'int8)
        , ("(int16 = void)", Type'int16)
        , ("(int32 = void)", Type'int32)
        , ("(int64 = void)", Type'int64)
        , ("(uint8 = void)", Type'uint8)
        , ("(uint16 = void)", Type'uint16)
        , ("(uint32 = void)", Type'uint32)
        , ("(uint64 = void)", Type'uint64)
        , ("(float32 = void)", Type'float32)
        , ("(float64 = void)", Type'float64)
        , ("(text = void)", Type'text)
        , ("(data = void)", Type'data_)
        , ( "(list = (elementType = (list = (elementType = (text = void)))))"
          , Type'list $ Type'list Type'text
          )
        , ( "(enum = (typeId = 4, brand = (scopes = [])))"
          , Type'enum 4 (Brand [])
          )
        , ( "(struct = (typeId = 7, brand = (scopes = [])))"
          , Type'struct 7 (Brand [])
          )
        , ( "(interface = (typeId = 1, brand = (scopes = [])))"
          , Type'interface 1 (Brand [])
          )
        , ( "(anyPointer = (unconstrained = (anyKind = void)))"
          , Type'anyPointer $ Type'anyPointer'unconstrained $ Type'anyPointer'unconstrained'anyKind
          )
        , ( "(anyPointer = (unconstrained = (struct = void)))"
          , Type'anyPointer $ Type'anyPointer'unconstrained $ Type'anyPointer'unconstrained'struct
          )
        , ( "(anyPointer = (unconstrained = (list = void)))"
          , Type'anyPointer $ Type'anyPointer'unconstrained $ Type'anyPointer'unconstrained'list
          )
        , ( "(anyPointer = (unconstrained = (capability = void)))"
          , Type'anyPointer $ Type'anyPointer'unconstrained Type'anyPointer'unconstrained'capability
          )
        , ( "(anyPointer = (parameter = (scopeId = 4, parameterIndex = 2)))"
          , Type'anyPointer $ Type'anyPointer'parameter 4 2
          )
        , ( "(anyPointer = (implicitMethodParameter = (parameterIndex = 7)))"
          , Type'anyPointer $ Type'anyPointer'implicitMethodParameter 7
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
                [ Brand'Scope' 32 Brand'Scope'inherit
                , Brand'Scope' 23 $ Brand'Scope'bind
                    [ Brand'Binding'unbound
                    , Brand'Binding'type_ Type'bool
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
