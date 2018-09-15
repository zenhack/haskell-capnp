{-|
Module: Instances
Description: Instances of Arbitrary for various types.

In particular, stuff from:

* "Data.Capnp.Untyped.Pure"
* "Capnp.Capnp.Schema.Pure"
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
module Instances () where

import Data.Word
import Test.QuickCheck

import Test.QuickCheck.Instances ()

import qualified Data.Vector as V

import Capnp.Capnp.Schema.Pure

import qualified Data.Capnp.Untyped.Pure as PU

-- Generate an arbitrary "unknown" tag, i.e. one with a value unassigned
-- by the schema. The parameter is the number of tags assigned by the schema.
arbitraryTag :: Word16 -> Gen Word16
arbitraryTag numTags = max numTags <$> arbitrary

instance Arbitrary Node where
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

instance Arbitrary Node' where
    shrink = genericShrink
    arbitrary = oneof
        [ pure Node'file
        , do
            dataWordCount <- arbitrary
            pointerCount <- arbitrary
            preferredListEncoding <- arbitrary
            isGroup <- arbitrary
            discriminantCount <- arbitrary
            discriminantOffset <- arbitrary
            fields <- arbitrarySmallerVec
            pure Node'struct{..}
        , Node'enum <$> arbitrarySmallerVec
        , Node'interface <$> arbitrarySmallerVec <*> arbitrarySmallerVec
        , Node'const <$> arbitrary <*> arbitrary
        , do
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
            pure Node'annotation{..}
        , Node'unknown' <$> arbitraryTag 6
        ]

instance Arbitrary Node'NestedNode where
    shrink = genericShrink
    arbitrary = Node'NestedNode
        <$> arbitrary
        <*> arbitrary

instance Arbitrary Field where
    shrink = genericShrink
    arbitrary = do
        name <- arbitrary
        codeOrder <- arbitrary
        annotations <- arbitrary
        discriminantValue <- arbitrary
        union' <- arbitrary
        ordinal <- arbitrary
        pure Field{..}

instance Arbitrary Field' where
    shrink = genericShrink
    arbitrary = oneof
        [ do
            offset <- arbitrary
            type_ <- arbitrary
            defaultValue <- arbitrary
            hadExplicitDefault <- arbitrary
            pure Field'slot{..}
        , Field'group <$> arbitrary
        ]

instance Arbitrary Field'ordinal where
    shrink = genericShrink
    arbitrary = oneof
        [ pure Field'ordinal'implicit
        , Field'ordinal'explicit <$> arbitrary
        ]

instance Arbitrary Enumerant where
    shrink = genericShrink
    arbitrary = Enumerant
        <$> arbitrary
        <*> arbitrary
        <*> arbitrarySmallerVec

instance Arbitrary Superclass where
    shrink = genericShrink
    arbitrary = Superclass
        <$> arbitrary
        <*> arbitrary

instance Arbitrary Method where
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

instance Arbitrary CapnpVersion where
    shrink = genericShrink
    arbitrary = CapnpVersion
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Node'Parameter where
    shrink = genericShrink
    arbitrary = Node'Parameter <$> arbitrary

instance Arbitrary Brand where
    shrink = genericShrink
    arbitrary = Brand <$> arbitrarySmallerVec

instance Arbitrary Brand'Scope where
    shrink = genericShrink
    arbitrary = Brand'Scope
        <$> arbitrary
        <*> arbitrary

instance Arbitrary Brand'Scope' where
    shrink = genericShrink
    arbitrary = oneof
        [ Brand'Scope'bind <$> arbitrarySmallerVec
        , pure Brand'Scope'inherit
        , Brand'Scope'unknown' <$> arbitraryTag 2
        ]

instance Arbitrary Brand'Binding where
    shrink = genericShrink
    arbitrary = oneof
        [ pure Brand'Binding'unbound
        , Brand'Binding'type_ <$> arbitrary
        , Brand'Binding'unknown' <$> arbitraryTag 2
        ]

instance Arbitrary Value where
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

instance Arbitrary Annotation where
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

instance Arbitrary Type'anyPointer where
    shrink = genericShrink
    arbitrary = oneof
        [ Type'anyPointer'unconstrained <$> arbitrary
        , Type'anyPointer'parameter <$> arbitrary <*> arbitrary
        , Type'anyPointer'implicitMethodParameter <$> arbitrary
        ]

instance Arbitrary Type'anyPointer'unconstrained where
    shrink = genericShrink
    arbitrary = oneof
        [ pure Type'anyPointer'unconstrained'anyKind
        , pure Type'anyPointer'unconstrained'struct
        , pure Type'anyPointer'unconstrained'list
        , pure Type'anyPointer'unconstrained'capability
        ]

instance Arbitrary Type where
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
        , Type'enum <$> arbitrary <*> arbitrary
        , Type'interface <$> arbitrary <*> arbitrary
        , Type'anyPointer <$> arbitrary
        , Type'unknown' <$> arbitraryTag 21
        ]

instance Arbitrary CodeGeneratorRequest where
    shrink = genericShrink
    arbitrary = do
        capnpVersion <- arbitrary
        nodes <- arbitrarySmallerVec
        requestedFiles <- arbitrarySmallerVec
        pure CodeGeneratorRequest{..}

instance Arbitrary CodeGeneratorRequest'RequestedFile where
    shrink = genericShrink
    arbitrary = CodeGeneratorRequest'RequestedFile
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary CodeGeneratorRequest'RequestedFile'Import where
    shrink = genericShrink
    arbitrary = CodeGeneratorRequest'RequestedFile'Import
        <$> arbitrary
        <*> arbitrary

instance Arbitrary a => Arbitrary (PU.Slice a) where
    shrink = genericShrink
    arbitrary = PU.Slice <$> arbitrarySmallerVec

arbitrarySmallerVec :: Arbitrary a => Gen (V.Vector a)
arbitrarySmallerVec = sized $ \size -> do
    -- Make sure the elements are scaled down relative to
    -- the size of the vector:
    vec <- arbitrary :: Gen (V.Vector ())
    let gen = resize (size `div` V.length vec) arbitrary
    traverse (const gen) vec

instance Arbitrary PU.Struct where
    shrink = genericShrink
    arbitrary = sized $ \size -> PU.Struct
        <$> arbitrary
        <*> arbitrary

instance Arbitrary PU.List where
    shrink = genericShrink
    arbitrary = oneof
        [ PU.List0 <$> arbitrarySmallerVec
        , PU.List1 <$> arbitrarySmallerVec
        , PU.List8 <$> arbitrarySmallerVec
        , PU.List16 <$> arbitrarySmallerVec
        , PU.List32 <$> arbitrarySmallerVec
        , PU.List64 <$> arbitrarySmallerVec
        , PU.ListPtr <$> arbitrarySmallerVec
        , PU.ListStruct <$> arbitrarySmallerVec
        ]

instance Arbitrary PU.PtrType where
    shrink (PU.PtrStruct s) = PU.PtrStruct <$> shrink s
    shrink (PU.PtrList   l) = PU.PtrList   <$> shrink l
    shrink (PU.PtrCap    c) = []
    arbitrary = oneof
        [ PU.PtrStruct <$> arbitrary
        , PU.PtrList <$> arbitrary
        -- We never generate capabilites, as we can't marshal Clients back in,
        -- so many of the invariants we check don't hold for caps.
        ]
