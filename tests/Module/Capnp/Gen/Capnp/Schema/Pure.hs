{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NegativeLiterals      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Module.Capnp.Gen.Capnp.Schema.Pure (pureSchemaTests) where

import Data.Proxy
import Test.Hspec

import Control.Exception.Safe    (bracket)
import Control.Monad             (when)
import Data.Default              (Default(..))
import Data.Foldable             (traverse_)
import System.Directory          (removeFile)
import System.IO
    (IOMode(ReadMode, WriteMode), hClose, openBinaryTempFile, withBinaryFile)
import Test.QuickCheck           (Arbitrary, Property, property)
import Test.QuickCheck.IO        (propertyIO)
import Test.QuickCheck.Instances ()
import Text.Heredoc              (here)
import Text.Show.Pretty          (ppShow)

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LBS

import Capnp.Gen.Capnp.Schema.New
import Util

import Instances ()

import Capnp.Mutability (freeze)
import Capnp.New
    ( IsStruct
    , Mutability(..)
    , Parse(..)
    , Raw(..)
    , defaultLimit
    , evalLimitT
    , hGetParsed
    , hPutParsed
    , msgToParsed
    )

import qualified Capnp.Message as M
import qualified Capnp.Untyped as U

pureSchemaTests :: Spec
pureSchemaTests = describe "Tests for generated high-level modules." $ do
    decodeTests
    decodeDefaultTests
    encodeTests
    propTests

encodeTests :: Spec
encodeTests = describe "schema encode tests" $
    testCase
        ( "Node.Parameter"
        , Node'Parameter { name = "Bob" }
        , "(name = \"Bob\")\n"
        )
  where
    testCase ::
        ( IsStruct a
        , Parse a pa
        , Show pa
        , Eq pa
        ) => (String, pa, String) -> Spec
    testCase (name, expectedValue, expectedText) = describe "cerialize" $
        it ("Should agree with capnp decode (with name = " ++ name ++ ")") $ do
            msg <- evalLimitT maxBound $ do
                -- TODO: add some helpers for all this.
                msg <- M.newMessage Nothing
                Raw cerialOut <- encode msg expectedValue
                U.setRoot cerialOut
                freeze msg
            let builder = M.encode msg
            actualText <- capnpDecode
                (LBS.toStrict $ BB.toLazyByteString builder)
                (MsgMetaData schemaSchemaSrc name)
            actualText `shouldBe` expectedText
            actualValue <- evalLimitT maxBound $ msgToParsed msg
            actualValue `shouldBe` expectedValue

decodeTests :: Spec
decodeTests = describe "schema decode tests" $ sequence_ $
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
                , sourceInfo = []
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
          , Node
                7
                "foo:MyType"
                4
                2
                [Node'NestedNode "theName" 321]
                [Annotation 2 (Value $ Value'bool True) (Brand [])]
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
              , Node'struct Node'struct'
                    { dataWordCount = 3
                    , pointerCount = 2
                    , preferredListEncoding = ElementSize'inlineComposite
                    , isGroup = False
                    , discriminantCount = 4
                    , discriminantOffset = 2
                    , fields =
                        [ Field
                            "fieldName"
                            3
                            [ Annotation
                                2
                                (Value $ Value'bool True)
                                (Brand [])
                            ]
                            7
                            (Field'ordinal' Field'ordinal'implicit)
                            (Field'group $ Field'group' 4)
                        ]
                    }
              )
            , ( "enum = (enumerants = [(name = \"blue\", codeOrder = 2, annotations = [])])"
              , Node'enum $ Node'enum' [ Enumerant "blue" 2 [] ]
              )
            , ( "interface = (methods = [], superclasses = [(id = 0, brand = (scopes = []))])"
              , Node'interface $ Node'interface' [] [Superclass 0 (Brand [])]
              )
            , ( "const = (type = (bool = void), value = (bool = false))"
              , Node'const $ Node'const' (Type Type'bool) (Value $ Value'bool False)
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
              , Node'annotation $ Node'annotation'
                    (Type Type'bool)
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
          , Annotation 323 (Value $ Value'bool True) (Brand [])
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
                [Annotation 2 (Value $ Value'bool True) (Brand [])]
                3
                (Field'ordinal' Field'ordinal'implicit)
                (Field'group $ Field'group' 4)
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
                [Annotation 2 (Value $ Value'bool True) (Brand [])]
                3
                (Field'ordinal' $ Field'ordinal'explicit 7)
                (Field'slot $ Field'slot'
                    3
                    (Type Type'bool)
                    (Value $ Value'bool False)
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
          , Enumerant "red" 4 [Annotation 23 (Value $ Value'uint8 3) (Brand [])]
          )
        ]
    , decodeTests "Superclass"
        [ ("(id = 34, brand = (scopes = []))", Superclass 34 (Brand []))
        ]
    , decodeTests "Type"
        [ ("(bool = void)", Type Type'bool)
        , ("(int8 = void)", Type Type'int8)
        , ("(int16 = void)", Type Type'int16)
        , ("(int32 = void)", Type Type'int32)
        , ("(int64 = void)", Type Type'int64)
        , ("(uint8 = void)", Type Type'uint8)
        , ("(uint16 = void)", Type Type'uint16)
        , ("(uint32 = void)", Type Type'uint32)
        , ("(uint64 = void)", Type Type'uint64)
        , ("(float32 = void)", Type Type'float32)
        , ("(float64 = void)", Type Type'float64)
        , ("(text = void)", Type Type'text)
        , ("(data = void)", Type Type'data_)
        , ( "(list = (elementType = (list = (elementType = (text = void)))))"
          , Type $ Type'list $ Type'list' $ Type $ Type'list $ Type'list' $ Type Type'text
          )
        , ( "(enum = (typeId = 4, brand = (scopes = [])))"
          , Type $ Type'enum $ Type'enum' 4 (Brand [])
          )
        , ( "(struct = (typeId = 7, brand = (scopes = [])))"
          , Type $ Type'struct $ Type'struct' 7 (Brand [])
          )
        , ( "(interface = (typeId = 1, brand = (scopes = [])))"
          , Type $ Type'interface $ Type'interface' 1 (Brand [])
          )
        , ( "(anyPointer = (unconstrained = (anyKind = void)))"
          , Type $ Type'anyPointer $ Type'anyPointer' $ Type'anyPointer'unconstrained $
                Type'anyPointer'unconstrained' Type'anyPointer'unconstrained'anyKind
          )
        , ( "(anyPointer = (unconstrained = (struct = void)))"
          , Type $ Type'anyPointer $ Type'anyPointer' $ Type'anyPointer'unconstrained $
                Type'anyPointer'unconstrained' Type'anyPointer'unconstrained'struct
          )
        , ( "(anyPointer = (unconstrained = (list = void)))"
          , Type $ Type'anyPointer $ Type'anyPointer' $ Type'anyPointer'unconstrained $
                Type'anyPointer'unconstrained' Type'anyPointer'unconstrained'list
          )
        , ( "(anyPointer = (unconstrained = (capability = void)))"
          , Type $ Type'anyPointer $ Type'anyPointer' $ Type'anyPointer'unconstrained $
                Type'anyPointer'unconstrained' Type'anyPointer'unconstrained'capability
          )
        , ( "(anyPointer = (parameter = (scopeId = 4, parameterIndex = 2)))"
          , Type $ Type'anyPointer $ Type'anyPointer' $
                Type'anyPointer'parameter $ Type'anyPointer'parameter' 4 2
          )
        , ( "(anyPointer = (implicitMethodParameter = (parameterIndex = 7)))"
          , Type $ Type'anyPointer $ Type'anyPointer' $ Type'anyPointer'implicitMethodParameter $
                Type'anyPointer'implicitMethodParameter' 7
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
                [ Brand'Scope 32 Brand'Scope'inherit
                , Brand'Scope 23 $ Brand'Scope'bind
                    [ Brand'Binding Brand'Binding'unbound
                    , Brand'Binding $ Brand'Binding'type_ $ Type Type'bool
                    ]
                ]
          )
        ]
    ] `asProxyTypeOf` (Proxy :: Proxy [Spec])
  where
    -- TODO: rename this: it's confusing to have both the top level and helper
    -- have the same name; not sure how that happened in the first place.
    decodeTests ::
        ( IsStruct a
        , Parse a pa
        , Eq pa
        , Show pa
        ) => String -> [(String, pa)] -> Spec
    decodeTests typename cases =
        describe ("Decode " ++ typename) $ traverse_ (testCase typename) cases

    testCase typename (capnpText, expected) =
        specify ("should agree with `capnp encode` on " ++ capnpText) $ do
            msg <- encodeValue schemaSchemaSrc typename capnpText
            actual <- evalLimitT 128 $ msgToParsed msg
            ppAssertEqual actual expected

decodeDefaultTests :: Spec
decodeDefaultTests = describe "Decoding default values" $ do
    decodeDefault @Type "Type"
    decodeDefault @Value "Value"
    decodeDefault @Node "Node"

decodeDefault ::
    forall a pa.
    ( IsStruct a
    , Parse a pa
    , Show pa
    , Eq pa
    , Default pa
    ) => String -> Spec
decodeDefault typename =
    specify ("The empty struct decodes to the default value for " ++ typename) $ do
        actual <- evalLimitT defaultLimit (msgToParsed @a M.empty)
        ppAssertEqual actual def

ppAssertEqual :: (Show a, Eq a) => a -> a -> IO ()
ppAssertEqual actual expected =
    when (actual /= expected) $ error $
        "Expected:\n\n" ++ ppShow expected ++ "\n\nbut got:\n\n" ++ ppShow actual

propTests :: Spec
propTests = describe "Various quickcheck properties" $ do
    propCase "Node" (Proxy :: Proxy Node)
    propCase "Node.Parameter" (Proxy :: Proxy Node'Parameter)
    propCase "Node.NestedNode" (Proxy :: Proxy Node'NestedNode)
    propCase "Field" (Proxy :: Proxy Field)
    propCase "Enumerant" (Proxy :: Proxy Enumerant)
    propCase "Superclass" (Proxy :: Proxy Superclass)
    propCase "Method" (Proxy :: Proxy Method)
    propCase "Type" (Proxy :: Proxy Type)
    propCase "Brand" (Proxy :: Proxy Brand)
    propCase "Brand.Scope" (Proxy :: Proxy Brand'Scope)
    propCase "Brand.Binding" (Proxy :: Proxy Brand'Binding)
    propCase "Value" (Proxy :: Proxy Value)
    propCase "Annotation" (Proxy :: Proxy Annotation)
    propCase "CapnpVersion" (Proxy :: Proxy CapnpVersion)
    propCase "CodeGeneratorRequest" (Proxy :: Proxy CodeGeneratorRequest)
    propCase "CodeGeneratorRequest.RequestedFile"
        (Proxy :: Proxy CodeGeneratorRequest'RequestedFile)
    propCase "CodeGeneratorRequest.RequestedFile.Import"
        (Proxy :: Proxy CodeGeneratorRequest'RequestedFile'Import)

propCase ::
    ( IsStruct a
    , Parse a pa
    , Arbitrary pa
    , Show pa
    , Eq pa
    ) => String -> Proxy a -> Spec
propCase name proxy = describe ("...for " ++ name) $ do
    specify "cerialize and decerialize are inverses." $
        property (prop_encodeParseInverses proxy)
    specify "hPutValue and hGetValue are inverses." $
        property (prop_hGetPutInverses proxy)

prop_hGetPutInverses ::
    ( IsStruct a
    , Parse a pa
    , Show pa
    , Eq pa
    ) => Proxy a -> pa -> Property
prop_hGetPutInverses _proxy expected = propertyIO $ do
    -- This is a little more complicated than I'd like due to resource
    -- management issues. We create a temporary file, then immediately
    -- close the handle to it, and open it again in a separate call to
    -- bracket. This allows us to decouple the lifetimes of the file and
    -- the handle.
    actual <- bracket
        (do
            (filename, handle) <- openBinaryTempFile "/tmp" "hPutParsed-output"
            hClose handle
            pure filename)
        removeFile
        (\filename -> do
            withBinaryFile filename WriteMode
                (`hPutParsed` expected)
            withBinaryFile filename ReadMode $ \h ->
                hGetParsed h defaultLimit)
    ppAssertEqual actual expected

prop_encodeParseInverses ::
    ( IsStruct a
    , Parse a pa
    , Eq pa
    , Show pa
    ) => Proxy a -> pa -> Property
prop_encodeParseInverses _proxy expected = propertyIO $ do
    actual <- evalLimitT maxBound $ do
        -- TODO: add some helpers for all this.
        msg <- M.newMessage Nothing
        Raw cerialOut <- encode msg expected
        U.setRoot cerialOut
        constMsg :: M.Message 'Const <- freeze msg
        msgToParsed constMsg
    ppAssertEqual actual expected
