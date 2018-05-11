{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Data.Capnp.Core.Schema
    (
    -- re-export the generated code:
    module Data.Capnp.ById.Xa93fc509624c72d9.Pure

    ------ Stuff we don't generate yet:

    -- type aliases
    , Id(..)

    -- constants
    , field'noDiscriminant
    )
    where

import Prelude hiding (id)

import Data.Capnp.ById.Xa93fc509624c72d9.Pure

import Codec.Capnp (Decerialize(..))

import Data.ReinterpretCast (wordToDouble, wordToFloat)

import Data.Bits
import Data.Capnp.Untyped.Pure
import Data.Word

type Id = Word64

field'noDiscriminant :: Word16
field'noDiscriminant = 0xffff

instance Decerialize Struct Node where
    decerialize (Struct words ptrs) = Node'
        <$> pure (sliceIndex 0 words)
        <*> (list8 (sliceIndex 0 ptrs) >>= decerialize)
        <*> pure (fromIntegral $ sliceIndex 1 words)
        <*> pure (sliceIndex 2 words)
        <*> (listStruct (sliceIndex 1 ptrs) >>= traverse decerialize)
        <*> (listStruct (sliceIndex 2 ptrs) >>= traverse decerialize)
        <*> (listStruct (sliceIndex 5 ptrs) >>= traverse decerialize)
        <*> pure (((sliceIndex 4 words `shiftR` 32) .&. 1) == 1)
        <*> case fromIntegral (sliceIndex 1 words `shiftR` 32) :: Word16 of
                0 -> pure Node'file
                1 -> Node'struct
                        (fromIntegral $ sliceIndex 1 words `shiftR` 48)
                        (fromIntegral $ sliceIndex 3 words)
                        <$> decerialize (fromIntegral (sliceIndex 3 words `shiftR` 16) :: Word16)
                        <*> pure (((sliceIndex 3 words `shiftR` 32) .&. 1) == 1)
                        <*> pure (fromIntegral (sliceIndex 3 words `shiftR` 48))
                        <*> pure (fromIntegral (sliceIndex 4 words))
                        <*> (listStruct (sliceIndex 3 ptrs) >>= traverse decerialize)
                2 -> Node'enum <$> (listStruct (sliceIndex 3 ptrs) >>= traverse decerialize)
                3 -> Node'interface
                        <$> (listStruct (sliceIndex 3 ptrs) >>= traverse decerialize)
                        <*> (listStruct (sliceIndex 4 ptrs) >>= traverse decerialize)
                4 -> Node'const
                        <$> (ptrStruct (sliceIndex 3 ptrs) >>= decerialize)
                        <*> (ptrStruct (sliceIndex 4 ptrs) >>= decerialize)
                5 -> Node'annotation
                        <$> (ptrStruct (sliceIndex 3 ptrs) >>= decerialize)
                        <*> pure (((sliceIndex 1 words `shiftR` 48) .&. 1) == 1)
                        <*> pure (((sliceIndex 1 words `shiftR` 49) .&. 1) == 1)
                        <*> pure (((sliceIndex 1 words `shiftR` 50) .&. 1) == 1)
                        <*> pure (((sliceIndex 1 words `shiftR` 51) .&. 1) == 1)
                        <*> pure (((sliceIndex 1 words `shiftR` 52) .&. 1) == 1)
                        <*> pure (((sliceIndex 1 words `shiftR` 53) .&. 1) == 1)
                        <*> pure (((sliceIndex 1 words `shiftR` 54) .&. 1) == 1)
                        <*> pure (((sliceIndex 1 words `shiftR` 55) .&. 1) == 1)
                        <*> pure (((sliceIndex 1 words `shiftR` 56) .&. 1) == 1)
                        <*> pure (((sliceIndex 1 words `shiftR` 57) .&. 1) == 1)
                        <*> pure (((sliceIndex 1 words `shiftR` 58) .&. 1) == 1)
                        <*> pure (((sliceIndex 1 words `shiftR` 59) .&. 1) == 1)
                tag -> pure $ Node'unknown' tag

instance Decerialize Struct Node'Parameter where
    decerialize (Struct _ ptrs) =
        Node'Parameter <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)

instance Decerialize Struct CodeGeneratorRequest where
    decerialize (Struct words ptrs) = CodeGeneratorRequest
        <$> (listStruct (sliceIndex 0 ptrs) >>= traverse decerialize)
        <*> (listStruct (sliceIndex 1 ptrs) >>= traverse decerialize)
        <*> (ptrStruct (sliceIndex 2 ptrs) >>= decerialize)

instance Decerialize Struct CodeGeneratorRequest'RequestedFile where
    decerialize (Struct words ptrs) = CodeGeneratorRequest'RequestedFile
        (sliceIndex 0 words)
        <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)
        <*> (listStruct (sliceIndex 1 ptrs) >>= traverse decerialize)

instance Decerialize Struct CodeGeneratorRequest'RequestedFile'Import where
    decerialize (Struct words ptrs) = CodeGeneratorRequest'RequestedFile'Import
        (sliceIndex 0 words)
        <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)

instance Decerialize Struct Node'NestedNode where
    decerialize (Struct words ptrs) = Node'NestedNode
        <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)
        <*> pure (sliceIndex 0 words)

instance Decerialize Struct Field where
    decerialize struct@(Struct words ptrs) = Field'
        <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)
        <*> pure (fromIntegral (sliceIndex 0 words) :: Word16)
        <*> (listStruct (sliceIndex 1 ptrs) >>= traverse decerialize)
        <*> pure (field'noDiscriminant `xor` fromIntegral (sliceIndex 0 words `shiftR` 16))
        <*> case fromIntegral (sliceIndex 1 words `shiftR` 16) :: Word16 of
                0 -> pure (Field'ordinal'implicit)
                1 -> pure $ Field'ordinal'explicit $ fromIntegral $ sliceIndex 1 words `shiftR` 32
                tag -> pure $ Field'ordinal'unknown' tag
        <*> case fromIntegral (sliceIndex 1 words) :: Word16 of
                0 -> Field'slot
                        (fromIntegral $ sliceIndex 0 words `shiftR` 32)
                        <$> (ptrStruct (sliceIndex 2 ptrs) >>= decerialize)
                        <*> (ptrStruct (sliceIndex 3 ptrs) >>= decerialize)
                        <*> pure ((sliceIndex 2 words .&. 1) == 1)
                1 -> pure $ Field'group (sliceIndex 2 words)
                tag -> pure $ Field'unknown' tag

instance Decerialize Struct Value where
    decerialize (Struct words ptrs) =
        let tag = fromIntegral (sliceIndex 0 words) :: Word16
        in case tag of
            0 -> pure Value'void
            1 -> pure $ Value'bool $ ((sliceIndex 0 words `shiftR` 16) .&. 1) == 1
            2 -> pure $ Value'int8 $ fromIntegral (sliceIndex 0 words `shiftR` 16)
            3 -> pure $ Value'int16 $ fromIntegral (sliceIndex 0 words `shiftR` 16)
            4 -> pure $ Value'int32 $ fromIntegral (sliceIndex 0 words `shiftR` 32)
            5 -> pure $ Value'int64 $ fromIntegral $ sliceIndex 1 words
            6 -> pure $ Value'uint8 $ fromIntegral (sliceIndex 0 words `shiftR` 16)
            7 -> pure $ Value'uint16 $ fromIntegral (sliceIndex 0 words `shiftR` 16)
            8 -> pure $ Value'uint32 $ fromIntegral (sliceIndex 0 words `shiftR` 32)
            9 -> pure $ Value'uint64 $ fromIntegral $ sliceIndex 1 words
            10 -> pure $ Value'float32 $ wordToFloat $ fromIntegral (sliceIndex 0 words `shiftR` 32)
            11 -> pure $ Value'float64 $ wordToDouble $ sliceIndex 1 words
            12 -> Value'text <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)
            13 -> Value'data_ <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)
            14 -> pure $ Value'list (sliceIndex 0 ptrs)
            15 -> pure $ Value'enum $ fromIntegral (sliceIndex 0 words `shiftR` 16)
            16 -> pure $ Value'struct (sliceIndex 0 ptrs)
            17 -> pure Value'interface
            18 -> pure $ Value'anyPointer (sliceIndex 0 ptrs)
            _ -> pure $ Value'unknown' tag

instance Decerialize Struct Brand where
    decerialize (Struct _ ptrs) = Brand <$>
        (listStruct (sliceIndex 0 ptrs) >>= traverse decerialize)

instance Decerialize Struct Brand'Scope where
    decerialize (Struct words ptrs) = Brand'Scope' (sliceIndex 0 words) <$>
        case fromIntegral (sliceIndex 1 words) :: Word16 of
            0 -> Brand'Scope'bind <$>
                    (listStruct (sliceIndex 0 ptrs)
                    >>= traverse decerialize)
            1 -> pure Brand'Scope'inherit
            tag -> pure $ Brand'Scope'unknown' tag

instance Decerialize Struct Brand'Binding where
    decerialize (Struct words ptrs) =
        case fromIntegral (sliceIndex 0 words) :: Word16 of
            0 -> pure Brand'Binding'unbound
            1 -> Brand'Binding'type_ <$> (ptrStruct (sliceIndex 0 ptrs) >>= decerialize)
            tag -> pure $ Brand'Binding'unknown' tag

instance Decerialize Struct Enumerant where
    decerialize (Struct words ptrs) = Enumerant
        <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)
        <*> pure (fromIntegral $ sliceIndex 0 words)
        <*> (listStruct (sliceIndex 1 ptrs) >>= traverse decerialize)

instance Decerialize Struct Superclass where
    decerialize (Struct words ptrs) = Superclass
        (sliceIndex 0 words)
        <$> (ptrStruct (sliceIndex 0 ptrs) >>= decerialize)

instance Decerialize Struct Type'anyPointer where
    decerialize (Struct words _) = pure $
        case fromIntegral (sliceIndex 1 words) :: Word16 of
            0 -> Type'anyPointer'unconstrained $
                case fromIntegral (sliceIndex 1 words `shiftR` 16) :: Word16 of
                    0   -> Type'anyPointer'unconstrained'anyKind
                    1   -> Type'anyPointer'unconstrained'struct
                    2   -> Type'anyPointer'unconstrained'list
                    3   -> Type'anyPointer'unconstrained'capability
                    tag -> Type'anyPointer'unconstrained'unknown' tag
            1 -> Type'anyPointer'parameter
                    (sliceIndex 2 words)
                    (fromIntegral $ sliceIndex 1 words `shiftR` 16)
            2 -> Type'anyPointer'implicitMethodParameter
                    (fromIntegral $ sliceIndex 1 words `shiftR` 16)
            tag -> Type'anyPointer'unknown' tag

instance Decerialize Struct Type where
    decerialize struct@(Struct words ptrs) =
        case fromIntegral (sliceIndex 0 words) :: Word16 of
            0 -> pure Type'void
            1 -> pure Type'bool
            2 -> pure Type'int8
            3 -> pure Type'int16
            4 -> pure Type'int32
            5 -> pure Type'int64
            6 -> pure Type'uint8
            7 -> pure Type'uint16
            8 -> pure Type'uint32
            9 -> pure Type'uint64
            10 -> pure Type'float32
            11 -> pure Type'float64
            12 -> pure Type'text
            13 -> pure Type'data_
            14 -> Type'list <$> (ptrStruct (sliceIndex 0 ptrs) >>= decerialize)
            15 -> Type'enum (sliceIndex 1 words) <$>
                    (ptrStruct (sliceIndex 0 ptrs) >>= decerialize)
            16 -> Type'struct (sliceIndex 1 words) <$>
                    (ptrStruct (sliceIndex 0 ptrs) >>= decerialize)
            17 -> Type'interface (sliceIndex 1 words) <$>
                    (ptrStruct (sliceIndex 0 ptrs) >>= decerialize)
            18 -> Type'anyPointer <$> decerialize struct
            tag -> pure $ Type'unknown' tag

instance Decerialize Struct CapnpVersion where
    decerialize (Struct words _) = pure $ CapnpVersion
        (fromIntegral $ sliceIndex 0 words)
        (fromIntegral $ sliceIndex 0 words `shiftR` 16)
        (fromIntegral $ sliceIndex 0 words `shiftR` 24)

instance Decerialize Struct Annotation where
    decerialize (Struct words ptrs) = Annotation
        (sliceIndex 0 words)
        <$> (ptrStruct (sliceIndex 0 ptrs) >>= decerialize)
        <*> (ptrStruct (sliceIndex 1 ptrs) >>= decerialize)

instance Decerialize Struct Method where
    decerialize (Struct words ptrs) = Method
        <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)
        <*> pure (fromIntegral $ sliceIndex 0 words)
        <*> pure (sliceIndex 1 words)
        <*> pure (sliceIndex 2 words)
        <*> (listStruct (sliceIndex 1 ptrs) >>= traverse decerialize)
        <*> (ptrStruct (sliceIndex 3 ptrs) >>= decerialize)
        <*> (ptrStruct (sliceIndex 2 ptrs) >>= decerialize)
        <*> (listStruct (sliceIndex 4 ptrs) >>= traverse decerialize)

instance Decerialize Word16 ElementSize where
    decerialize n = pure $ case n of
        0 -> ElementSize'empty
        1 -> ElementSize'bit
        2 -> ElementSize'byte
        3 -> ElementSize'twoBytes
        4 -> ElementSize'fourBytes
        5 -> ElementSize'eightBytes
        6 -> ElementSize'pointer
        7 -> ElementSize'inlineComposite
        _ -> ElementSize'unknown' n
