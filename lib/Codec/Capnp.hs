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
    , flatten
    , getData
    , messageDefault
    )
import Data.ReinterpretCast
    (doubleToWord, floatToWord, wordToDouble, wordToFloat)

import qualified Data.Capnp.BuiltinTypes as BuiltinTypes
import qualified Data.Capnp.Message      as M

class Decerialize m from to where
    decerialize :: from -> m to

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
class ReadCtx m b => IsPtr m a b where
    fromPtr :: M.Message b -> Maybe (Ptr m b) -> m a

-- | Types that can be extracted from a struct.
class IsStruct m a b where
    fromStruct :: Struct m b -> m a

expected :: MonadThrow m => String -> m a
expected msg = throwM $ SchemaViolationError $ "expected " ++ msg

-- | @'getWordField' struct index offset def@ fetches a field from the
-- struct's data section. @index@ is the index of the 64-bit word in the data
-- section in which the field resides. @offset@ is the offset in bits from the
-- start of that word to the field. @def@ is the default value for this field.
getWordField :: (ReadCtx m b, IsWord a) => Struct m b -> Int -> Int -> Word64 -> m a
getWordField struct idx offset def = fmap
    ( fromWord
    . xor def
    . (`shiftR` offset)
    )
    (getData idx struct)

instance Monad m => Decerialize m Bool Bool where
    decerialize = pure
instance Monad m => Decerialize m Word8 Word8 where
    decerialize = pure
instance Monad m => Decerialize m Word16 Word16 where
    decerialize = pure
instance Monad m => Decerialize m Word32 Word32 where
    decerialize = pure
instance Monad m => Decerialize m Word64 Word64 where
    decerialize = pure
instance Monad m => Decerialize m Int8 Int8 where
    decerialize = pure
instance Monad m => Decerialize m Int16 Int16 where
    decerialize = pure
instance Monad m => Decerialize m Int32 Int32 where
    decerialize = pure
instance Monad m => Decerialize m Int64 Int64 where
    decerialize = pure
instance Monad m => Decerialize m Float Float where
    decerialize = pure
instance Monad m => Decerialize m Double Double where
    decerialize = pure

-- IsWord instance for booleans.
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
instance ReadCtx m b => IsPtr m (ListOf m b ()) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List0 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 0"

-- IsPtr instances for lists of unsigned integers.
instance ReadCtx m b => IsPtr m (ListOf m b Word8) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List8 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 8"
instance ReadCtx m b => IsPtr m (ListOf m b Word16) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List16 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 16"
instance ReadCtx m b => IsPtr m (ListOf m b Word32) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List32 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 32"
instance ReadCtx m b => IsPtr m (ListOf m b Word64) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List64 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 64"

-- | IsPtr instance for pointers -- this is just the identity.
instance ReadCtx m b => IsPtr m (Maybe (Ptr m b)) b where
    fromPtr _ = pure

-- IsPtr instance for composite lists.
instance ReadCtx m b => IsPtr m (ListOf m b (Struct m b)) b where
    fromPtr msg Nothing                            = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (ListStruct list))) = pure list
    fromPtr _ _ = expected "pointer to list of structs"

-- | IsPtr instances for lists of floating point numbers.
--
-- These just wrap the unsigned integer instances of the appropriate size.
instance ReadCtx m b => IsPtr m (ListOf m b Float) b where
    fromPtr msg = fmap (fmap wordToFloat) . fromPtr msg
instance ReadCtx m b => IsPtr m (ListOf m b Double) b where
    fromPtr msg = fmap (fmap wordToDouble) . fromPtr msg

-- IsPtr instances for lists of signed instances. These just shell out to the
-- unsigned instances.
instance ReadCtx m b => IsPtr m (ListOf m b Int8) b where
    fromPtr msg = fmap (fmap (fromIntegral :: Word8 -> Int8)) . fromPtr msg
instance ReadCtx m b => IsPtr m (ListOf m b Int16) b where
    fromPtr msg = fmap (fmap (fromIntegral :: Word16 -> Int16)) . fromPtr msg
instance ReadCtx m b => IsPtr m (ListOf m b Int32) b where
    fromPtr msg = fmap (fmap (fromIntegral :: Word32 -> Int32)) . fromPtr msg
instance ReadCtx m b => IsPtr m (ListOf m b Int64) b where
    fromPtr msg = fmap (fmap (fromIntegral :: Word64 -> Int64)) . fromPtr msg

-- IsPtr instances for Text and Data. These wrap lists of bytes.
instance ReadCtx m b => IsPtr m (Data b) b where
    fromPtr msg ptr = fromPtr msg ptr >>= BuiltinTypes.getData
instance ReadCtx m b => IsPtr m (Text b) b where
    fromPtr msg ptr = fromPtr msg ptr >>= BuiltinTypes.getText

nestedListPtr :: (ReadCtx m b, IsPtr m a b) => M.Message b -> Maybe (Ptr m b) -> m (ListOf m b a)
nestedListPtr msg ptr = flatten . fmap (fromPtr msg) <$> fromPtr msg ptr

structListPtr :: (ReadCtx m b, IsStruct m a b) => M.Message b -> Maybe (Ptr m b) -> m (ListOf m b a)
structListPtr msg ptr =
    flatten . fmap fromStruct <$> structListFromPtr msg ptr
  where
    structListFromPtr :: ReadCtx m b => M.Message b -> Maybe (Ptr m b) -> m (ListOf m b (Struct m b))
    structListFromPtr = fromPtr

structPtr :: (ReadCtx m b, IsStruct m a b) => M.Message b -> Maybe (Ptr m b) -> m a
structPtr msg ptr = structFromPtr msg ptr >>= fromStruct
  where
    structFromPtr :: ReadCtx m b => M.Message b -> Maybe (Ptr m b) -> m (Struct m b)
    structFromPtr = fromPtr

instance (ReadCtx m b, IsPtr m (ListOf m b a) b) => IsPtr m (ListOf m b (ListOf m b a)) b where
    fromPtr = nestedListPtr
    -- fromPtr msg ptr = flatten . fmap (fromPtr msg) <$> fromPtr msg ptr
instance (ReadCtx m b) => IsPtr m (ListOf m b (Maybe (Ptr m b))) b where
    fromPtr = nestedListPtr
    -- fromPtr msg ptr = flatten . fmap (fromPtr msg) <$> fromPtr msg ptr

-- IsStruct instance for Struct; just the identity.
instance ReadCtx m b => IsStruct m (Struct m b) b where
    fromStruct = pure

instance ReadCtx m b => IsPtr m (Struct m b) b where
    fromPtr msg Nothing              = fromStruct (messageDefault msg :: Struct m b)
    fromPtr msg (Just (PtrStruct s)) = fromStruct s
    fromPtr _ _                      = expected "pointer to struct"
