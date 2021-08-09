{-|
Module: Instances
Description: Instances of Arbitrary for various types.

In particular, stuff from:

* "Capnp.Untyped.Pure"
* "Capnp.Gen.Capnp.Schema.Pure"
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RecordWildCards       #-}
module Instances () where

import Data.Word
import Test.QuickCheck

import Test.QuickCheck.Instances ()

import qualified Data.Vector as V

import Capnp.New (Which)

import Capnp.Gen.Capnp.Schema.New

import qualified Capnp.New.Basics as B

-- Generate an arbitrary "unknown" tag, i.e. one with a value unassigned
-- by the schema. The parameter is the number of tags assigned by the schema.
arbitraryTag :: Word16 -> Gen Word16
arbitraryTag numTags = max numTags <$> arbitrary

instance Arbitrary (Parsed Node) where
    shrink = genericShrink
    arbitrary = do
        id <- arbitrary
        displayName <- arbitrary
        displayNamePrefixLength <- arbitrary
        scopeId <- arbitrary
        parameters <- arbitrarySmallerVec
        isGeneric <- arbitrary
        nestedNodes <- arbitrarySmallerVec
        annotations <- arbitrarySmallerVec
        union' <- arbitrary
        pure Node{..}

instance Arbitrary (Parsed Node'SourceInfo) where
    shrink = genericShrink
    arbitrary = do
        id <- arbitrary
        docComment <- arbitrary
        members <- arbitrarySmallerVec
        pure Node'SourceInfo{..}

instance Arbitrary (Parsed Node'SourceInfo'Member) where
    shrink = genericShrink
    arbitrary = Node'SourceInfo'Member <$> arbitrary

instance Arbitrary (Parsed (Which Node)) where
    shrink = genericShrink
    arbitrary = oneof
        [ pure Node'file
        , Node'struct <$> arbitrary
        , Node'enum <$> arbitrary
        , Node'interface <$> arbitrary
        , Node'const <$> arbitrary
        , Node'annotation <$> arbitrary
        , Node'unknown' <$> arbitraryTag 6
        ]

instance Arbitrary (Parsed Node'enum) where
    shrink = genericShrink
    arbitrary = Node'enum' <$> arbitrarySmallerVec

instance Arbitrary (Parsed Node'struct) where
    shrink = genericShrink
    arbitrary = do
        dataWordCount <- arbitrary
        pointerCount <- arbitrary
        preferredListEncoding <- arbitrary
        isGroup <- arbitrary
        discriminantCount <- arbitrary
        discriminantOffset <- arbitrary
        fields <- arbitrarySmallerVec
        pure Node'struct'{..}

instance Arbitrary (Parsed Node'interface) where
    shrink = genericShrink
    arbitrary = Node'interface' <$> arbitrarySmallerVec <*> arbitrarySmallerVec

instance Arbitrary (Parsed Node'const) where
    shrink = genericShrink
    arbitrary = Node'const' <$> arbitrary <*> arbitrary

instance Arbitrary (Parsed Node'annotation) where
    shrink = genericShrink
    arbitrary = do
        type_ <- arbitrary
        targetsFile <- arbitrary
        targetsConst <- arbitrary
        targetsEnum <- arbitrary
        targetsEnumerant <- arbitrary
        targetsStruct <- arbitrary
        targetsField <- arbitrary
        targetsUnion <- arbitrary
        targetsGroup <- arbitrary
        targetsInterface <- arbitrary
        targetsMethod <- arbitrary
        targetsParam <- arbitrary
        targetsAnnotation <- arbitrary
        pure Node'annotation'{..}

instance Arbitrary (Parsed Node'NestedNode) where
    shrink = genericShrink
    arbitrary = Node'NestedNode
        <$> arbitrary
        <*> arbitrary

instance Arbitrary (Parsed Field) where
    shrink = genericShrink
    arbitrary = do
        name <- arbitrary
        codeOrder <- arbitrary
        annotations <- arbitrary
        discriminantValue <- arbitrary
        union' <- arbitrary
        ordinal <- arbitrary
        pure Field{..}

instance Arbitrary (Parsed (Which Field)) where
    shrink = genericShrink
    arbitrary = oneof
        [ Field'slot <$> arbitrary
        , Field'group <$> arbitrary
        ]

instance Arbitrary (Parsed Field'slot) where
    shrink = genericShrink
    arbitrary = do
        offset <- arbitrary
        type_ <- arbitrary
        defaultValue <- arbitrary
        hadExplicitDefault <- arbitrary
        pure Field'slot'{..}

instance Arbitrary (Parsed Field'group) where
    shrink = genericShrink
    arbitrary = Field'group' <$> arbitrary

instance Arbitrary (Parsed Field'ordinal) where
    shrink = genericShrink
    arbitrary = Field'ordinal' <$> arbitrary

instance Arbitrary (Parsed (Which Field'ordinal)) where
    shrink = genericShrink
    arbitrary = oneof
        [ pure Field'ordinal'implicit
        , Field'ordinal'explicit <$> arbitrary
        ]

instance Arbitrary (Parsed Enumerant) where
    shrink = genericShrink
    arbitrary = Enumerant
        <$> arbitrary
        <*> arbitrary
        <*> arbitrarySmallerVec

instance Arbitrary (Parsed Superclass) where
    shrink = genericShrink
    arbitrary = Superclass
        <$> arbitrary
        <*> arbitrary

instance Arbitrary (Parsed Method) where
    shrink = genericShrink
    arbitrary = do
        name <- arbitrary
        codeOrder <- arbitrary
        implicitParameters <- arbitrary
        paramStructType <- arbitrary
        paramBrand <- arbitrary
        resultStructType <- arbitrary
        resultBrand <- arbitrary
        annotations <- arbitrary
        pure Method{..}

instance Arbitrary (Parsed CapnpVersion) where
    shrink = genericShrink
    arbitrary = CapnpVersion
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary (Parsed Node'Parameter) where
    shrink = genericShrink
    arbitrary = Node'Parameter <$> arbitrary

instance Arbitrary (Parsed Brand) where
    shrink = genericShrink
    arbitrary = Brand <$> arbitrarySmallerVec

instance Arbitrary (Parsed Brand'Scope) where
    shrink = genericShrink
    arbitrary = Brand'Scope
        <$> arbitrary
        <*> arbitrary

instance Arbitrary (Parsed (Which Brand'Scope)) where
    shrink = genericShrink
    arbitrary = oneof
        [ Brand'Scope'bind <$> arbitrarySmallerVec
        , pure Brand'Scope'inherit
        , Brand'Scope'unknown' <$> arbitraryTag 2
        ]

instance Arbitrary (Parsed Brand'Binding) where
    shrink = genericShrink
    arbitrary = Brand'Binding <$> arbitrary

instance Arbitrary (Parsed (Which Brand'Binding)) where
    shrink = genericShrink
    arbitrary = oneof
        [ pure Brand'Binding'unbound
        , Brand'Binding'type_ <$> arbitrary
        , Brand'Binding'unknown' <$> arbitraryTag 2
        ]

instance Arbitrary (Parsed Value) where
    shrink = genericShrink
    arbitrary = Value <$> arbitrary

instance Arbitrary (Parsed (Which Value)) where
    shrink = genericShrink
    arbitrary = oneof
        [ pure Value'void
        , Value'bool <$> arbitrary
        , Value'int8 <$> arbitrary
        , Value'int16 <$> arbitrary
        , Value'int32 <$> arbitrary
        , Value'int64 <$> arbitrary
        , Value'uint8 <$> arbitrary
        , Value'uint16 <$> arbitrary
        , Value'uint32 <$> arbitrary
        , Value'uint64 <$> arbitrary
        , Value'float32 <$> arbitrary
        , Value'float64 <$> arbitrary
        , Value'text <$> arbitrary
        , Value'data_ <$> arbitrary
        , Value'list <$> arbitrary
        , Value'enum <$> arbitrary
        , Value'struct <$> arbitrary
        , pure Value'interface
        , Value'anyPointer <$> arbitrary
        , Value'unknown' <$> arbitraryTag 19
        ]

instance Arbitrary (Parsed Annotation) where
    shrink = genericShrink
    arbitrary = Annotation
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary ElementSize where
    shrink = genericShrink
    arbitrary = oneof
        [ pure ElementSize'empty
        , pure ElementSize'bit
        , pure ElementSize'byte
        , pure ElementSize'twoBytes
        , pure ElementSize'fourBytes
        , pure ElementSize'eightBytes
        , pure ElementSize'pointer
        , pure ElementSize'inlineComposite
        , ElementSize'unknown' <$> arbitraryTag 8
        ]

instance Arbitrary (Parsed Type'anyPointer) where
    shrink = genericShrink
    arbitrary = Type'anyPointer' <$> arbitrary

instance Arbitrary (Parsed (Which Type'anyPointer)) where
    shrink = genericShrink
    arbitrary = oneof
        [ Type'anyPointer'unconstrained <$> arbitrary
        , Type'anyPointer'parameter <$> arbitrary
        , Type'anyPointer'implicitMethodParameter <$> arbitrary
        ]

instance Arbitrary (Parsed Type'anyPointer'unconstrained) where
    shrink = genericShrink
    arbitrary = Type'anyPointer'unconstrained' <$> arbitrary

instance Arbitrary (Parsed (Which Type'anyPointer'unconstrained)) where
    shrink = genericShrink
    arbitrary = oneof
        [ pure Type'anyPointer'unconstrained'anyKind
        , pure Type'anyPointer'unconstrained'struct
        , pure Type'anyPointer'unconstrained'list
        , pure Type'anyPointer'unconstrained'capability
        ]

instance Arbitrary (Parsed Type'anyPointer'parameter) where
    shrink = genericShrink
    arbitrary =
        Type'anyPointer'parameter' <$> arbitrary <*> arbitrary

instance Arbitrary (Parsed Type'anyPointer'implicitMethodParameter) where
    shrink = genericShrink
    arbitrary =
        Type'anyPointer'implicitMethodParameter' <$> arbitrary

instance Arbitrary (Parsed Type) where
    shrink = genericShrink
    arbitrary = Type <$> arbitrary

instance Arbitrary (Parsed (Which Type)) where
    shrink = genericShrink
    arbitrary = oneof
        [ pure Type'void
        , pure Type'bool
        , pure Type'int8
        , pure Type'int16
        , pure Type'int32
        , pure Type'int64
        , pure Type'uint8
        , pure Type'uint16
        , pure Type'uint32
        , pure Type'uint64
        , pure Type'float32
        , pure Type'float64
        , pure Type'text
        , pure Type'data_
        , Type'list <$> arbitrary
        , Type'enum <$> arbitrary
        , Type'struct <$> arbitrary
        , Type'interface <$> arbitrary
        , Type'anyPointer <$> arbitrary
        , Type'unknown' <$> arbitraryTag 21
        ]

instance Arbitrary (Parsed Type'list) where
    shrink = genericShrink
    arbitrary = Type'list' <$> arbitrary

instance Arbitrary (Parsed Type'enum) where
    shrink = genericShrink
    arbitrary = Type'enum' <$> arbitrary <*> arbitrary

instance Arbitrary (Parsed Type'struct) where
    shrink = genericShrink
    arbitrary = Type'struct' <$> arbitrary <*> arbitrary

instance Arbitrary (Parsed Type'interface) where
    shrink = genericShrink
    arbitrary = Type'interface' <$> arbitrary <*> arbitrary

instance Arbitrary (Parsed CodeGeneratorRequest) where
    shrink = genericShrink
    arbitrary = do
        capnpVersion <- arbitrary
        nodes <- arbitrarySmallerVec
        requestedFiles <- arbitrarySmallerVec
        sourceInfo <- arbitrarySmallerVec
        pure CodeGeneratorRequest{..}

instance Arbitrary (Parsed CodeGeneratorRequest'RequestedFile) where
    shrink = genericShrink
    arbitrary = CodeGeneratorRequest'RequestedFile
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary (Parsed CodeGeneratorRequest'RequestedFile'Import) where
    shrink = genericShrink
    arbitrary = CodeGeneratorRequest'RequestedFile'Import
        <$> arbitrary
        <*> arbitrary

arbitrarySmallerVec :: Arbitrary a => Gen (V.Vector a)
arbitrarySmallerVec = sized $ \size -> do
    -- Make sure the elements are scaled down relative to
    -- the size of the vector:
    vec <- arbitrary :: Gen (V.Vector ())
    let gen = resize (size `div` V.length vec) arbitrary
    traverse (const gen) vec

instance Arbitrary (Parsed B.AnyStruct) where
    shrink = genericShrink
    arbitrary = sized $ \_ -> B.Struct
        <$> arbitrary
        <*> arbitrary

instance Arbitrary (Parsed B.AnyList) where
    shrink = genericShrink
    arbitrary = oneof
        [ B.List0 <$> arbitrarySmallerVec
        , B.List1 <$> arbitrarySmallerVec
        , B.List8 <$> arbitrarySmallerVec
        , B.List16 <$> arbitrarySmallerVec
        , B.List32 <$> arbitrarySmallerVec
        , B.List64 <$> arbitrarySmallerVec
        , B.ListPtr <$> arbitrarySmallerVec
        , B.ListStruct <$> arbitrarySmallerVec
        ]

instance Arbitrary (Parsed B.AnyPointer) where
    shrink (B.PtrStruct s) = B.PtrStruct <$> shrink s
    shrink (B.PtrList   l) = B.PtrList   <$> shrink l
    shrink (B.PtrCap    _) = []
    arbitrary = oneof
        [ B.PtrStruct <$> arbitrary
        , B.PtrList <$> arbitrary
        -- We never generate capabilites, as we can't marshal Clients back in,
        -- so many of the invariants we check don't hold for caps.
        ]
