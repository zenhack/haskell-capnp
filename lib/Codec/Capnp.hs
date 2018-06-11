{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Codec.Capnp where

import Data.Bits
import Data.Int
import Data.Word

import Control.Monad.Catch     (MonadThrow(throwM))
import Data.Capnp.BuiltinTypes (Data, Text)
import Data.Capnp.Errors       (Error(SchemaViolationError))
import Data.Capnp.Untyped
    ( List(..)
    , ListOf
    , Ptr(..)
    , ReadCtx
    , Struct
    , extractElts
    , getData
    , messageDefault
    )
import Data.ReinterpretCast
    (doubleToWord, floatToWord, wordToDouble, wordToFloat)

import qualified Data.Capnp.BuiltinTypes as BuiltinTypes
import qualified Data.Capnp.Message      as M

class Decerialize from to where
    decerialize :: ReadCtx m => from -> m to

-- | Types that can be converted to and from a 64-bit word.
--
-- This is mostly a helper for generated code, which uses it to interact
-- with the data sections of structs.
class IsWord a where
    fromWord :: Word64 -> a
    toWord :: a -> Word64

-- | Types that can be extracted from an untyped pointer.
--
-- Similarly to IsWord, this is mostly used in generated code, to interact
-- with the pointer section of structs.
class IsPtr a where
    fromPtr :: ReadCtx m => M.Message -> Maybe Ptr -> m a

-- | Types that can be extracted from a struct.
class IsStruct a where
    fromStruct :: ReadCtx m => Struct -> m a

expected :: MonadThrow m => String -> m a
expected msg = throwM $ SchemaViolationError $ "expected " ++ msg

-- | @'getWordField' struct index offset def@ fetches a field from the
-- struct's data section. @index@ is the index of the 64-bit word in the data
-- section in which the field resides. @offset@ is the offset in bits from the
-- start of that word to the field. @def@ is the default value for this field.
getWordField :: (ReadCtx m, IsWord a) => Struct -> Int -> Int -> Word64 -> m a
getWordField struct idx offset def = fmap
    ( fromWord
    . xor def
    . (`shiftR` offset)
    )
    (getData idx struct)

instance Decerialize Bool Bool where
    decerialize = pure
instance Decerialize Word8 Word8 where
    decerialize = pure
instance Decerialize Word16 Word16 where
    decerialize = pure
instance Decerialize Word32 Word32 where
    decerialize = pure
instance Decerialize Word64 Word64 where
    decerialize = pure
instance Decerialize Int8 Int8 where
    decerialize = pure
instance Decerialize Int16 Int16 where
    decerialize = pure
instance Decerialize Int32 Int32 where
    decerialize = pure
instance Decerialize Int64 Int64 where
    decerialize = pure
instance Decerialize Float Float where
    decerialize = pure
instance Decerialize Double Double where
    decerialize = pure

instance IsWord Bool where
    fromWord n = (n .&. 1) == 1
    toWord True  = 1
    toWord False = 0

-- IsWord instances for integral types; they're all the same.
instance IsWord Int8 where
    fromWord = fromIntegral
    toWord = fromIntegral
instance IsWord Int16 where
    fromWord = fromIntegral
    toWord = fromIntegral
instance IsWord Int32 where
    fromWord = fromIntegral
    toWord = fromIntegral
instance IsWord Int64 where
    fromWord = fromIntegral
    toWord = fromIntegral
instance IsWord Word8 where
    fromWord = fromIntegral
    toWord = fromIntegral
instance IsWord Word16 where
    fromWord = fromIntegral
    toWord = fromIntegral
instance IsWord Word32 where
    fromWord = fromIntegral
    toWord = fromIntegral
instance IsWord Word64 where
    fromWord = fromIntegral
    toWord = fromIntegral

instance IsWord Float where
    fromWord = wordToFloat . fromIntegral
    toWord = fromIntegral . floatToWord
instance IsWord Double where
    fromWord = wordToDouble
    toWord = doubleToWord

-- IsPtr instance for lists of Void/().
instance IsPtr (ListOf ()) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List0 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 0"

-- IsPtr instances for lists of unsigned integers.
instance IsPtr (ListOf Word8) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List8 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 8"
instance IsPtr (ListOf Word16) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List16 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 16"
instance IsPtr (ListOf Word32) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List32 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 32"
instance IsPtr (ListOf Word64) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List64 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 64"

-- | IsPtr instance for pointers -- this is just the identity.
instance IsPtr (Maybe Ptr) where
    fromPtr _ = pure

-- IsPtr instance for composite lists.
instance IsPtr (ListOf Struct) where
    fromPtr msg Nothing                            = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (ListStruct list))) = pure list
    fromPtr _ _ = expected "pointer to list of structs"

-- | IsPtr instances for lists of floating point numbers.
--
-- These just wrap the unsigned integer instances of the appropriate size.
instance IsPtr (ListOf Float) where
    fromPtr msg = fmap (fmap wordToFloat) . fromPtr msg
instance IsPtr (ListOf Double) where
    fromPtr msg = fmap (fmap wordToDouble) . fromPtr msg

-- IsPtr instances for lists of signed instances. These just shell out to the
-- unsigned instances.
instance IsPtr (ListOf Int8) where
    fromPtr msg = fmap (fmap (fromIntegral :: Word8 -> Int8)) . fromPtr msg
instance IsPtr (ListOf Int16) where
    fromPtr msg = fmap (fmap (fromIntegral :: Word16 -> Int16)) . fromPtr msg
instance IsPtr (ListOf Int32) where
    fromPtr msg = fmap (fmap (fromIntegral :: Word32 -> Int32)) . fromPtr msg
instance IsPtr (ListOf Int64) where
    fromPtr msg = fmap (fmap (fromIntegral :: Word64 -> Int64)) . fromPtr msg

-- IsPtr instances for Text and Data. These wrap lists of bytes.
instance IsPtr Data where
    fromPtr msg ptr = fromPtr msg ptr >>= BuiltinTypes.getData
instance IsPtr Text where
    fromPtr msg ptr = case ptr of
        Just _ ->
            fromPtr msg ptr >>= BuiltinTypes.getText
        Nothing -> do
            -- getText expects and strips off a NUL byte at the end of the
            -- string. In the case of a null pointer we just want to return
            -- the empty string, so we bypass it here.
            BuiltinTypes.Data bytes <- fromPtr msg ptr
            pure $ BuiltinTypes.Text bytes

nestedListPtr :: (ReadCtx m, IsPtr a) => M.Message -> Maybe Ptr -> m (ListOf a)
nestedListPtr msg ptr = extractElts (fromPtr msg) <$> fromPtr msg ptr

structListPtr :: (ReadCtx m, IsStruct a) => M.Message -> Maybe Ptr -> m (ListOf a)
structListPtr msg ptr = extractElts fromStruct <$> fromPtr msg ptr

structPtr :: (ReadCtx m, IsStruct a) => M.Message -> Maybe Ptr -> m a
structPtr msg ptr = structFromPtr msg ptr >>= fromStruct
  where
    structFromPtr :: ReadCtx m => M.Message -> Maybe Ptr -> m Struct
    structFromPtr = fromPtr

instance IsPtr (ListOf a) => IsPtr (ListOf (ListOf a)) where
    fromPtr = nestedListPtr
instance IsPtr (ListOf (Maybe Ptr)) where
    fromPtr = nestedListPtr

-- IsStruct instance for Struct; just the identity.
instance IsStruct Struct where
    fromStruct = pure

instance IsPtr Struct where
    fromPtr msg Nothing              = fromStruct (messageDefault msg :: Struct)
    fromPtr msg (Just (PtrStruct s)) = fromStruct s
    fromPtr _ _                      = expected "pointer to struct"
