{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Codec.Capnp where

import Data.Bits
import Data.Int
import Data.Word

import Control.Monad.Catch     (MonadThrow(throwM))
import Data.Capnp.BuiltinTypes (Data, Text)
import Data.Capnp.Errors       (Error(SchemaViolationError))
import Data.Capnp.Untyped
    (List(..), ListOf, Ptr(..), ReadCtx, Struct, getData, messageDefault)
import Data.ReinterpretCast    (wordToDouble, wordToFloat)

import qualified Data.Capnp.BuiltinTypes as BuiltinTypes
import qualified Data.Capnp.Message      as M

class Decerialize from to where
    decerialize :: MonadThrow m => from -> m to

expected :: MonadThrow m => String -> m a
expected msg = throwM $ SchemaViolationError $ "expected " ++ msg

-- | @'getWordField' struct index offset def@ fetches a field from the
-- struct's data section. @index@ is the index of the 64-bit word in the data
-- section in which the field resides. @offset@ is the offset in bits from the
-- start of that word to the field. @def@ is the default value for this field.
getWordField :: (ReadCtx m b, IsWord a) => Struct b -> Int -> Int -> Word64 -> m a
getWordField struct idx offset def = fmap
    ( fromWord
    . xor def
    . (`shiftR` offset)
    )
    (getData idx struct)

-- | Types that can be converted to and from a 64-bit word.
--
-- This is mostly a helper for generated code, which uses it to interact
-- with the data sections of structs.
class IsWord a where
    fromWord :: Word64 -> a
    toWord :: a -> Word64

instance IsWord Bool where
    fromWord n = (n .&. 1) == 1
    toWord True  = 1
    toWord False = 0

-- Integral types all have the same implementation:
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

-- | Types that can be extracted from an untyped pointer.
--
-- Similarly to IsWord, this is mostly used in generated code, to interact
-- with the pointer section of structs.
class IsPtr a b where
    fromPtr :: ReadCtx m b => M.Message b -> Maybe (Ptr b) -> m a

instance IsPtr (ListOf b ()) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List0 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 0"
instance IsPtr (ListOf b Word8) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List8 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 8"
instance IsPtr (ListOf b Word16) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List16 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 16"
instance IsPtr (ListOf b Word32) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List32 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 32"
instance IsPtr (ListOf b Word64) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List64 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 64"
instance IsPtr (ListOf b (Maybe (Ptr b))) b where
    fromPtr msg Nothing                         = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (ListPtr list))) = pure list
    fromPtr _ _ = expected "pointer to list of pointers"
instance IsPtr (ListOf b (Struct b)) b where
    fromPtr msg Nothing                            = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (ListStruct list))) = pure list
    fromPtr _ _ = expected "pointer to list of structs"
instance IsPtr (Struct b) b where
    fromPtr msg Nothing              = pure $ messageDefault msg
    fromPtr msg (Just (PtrStruct s)) = pure s
    fromPtr _ _                      = expected "pointer to struct"
instance IsPtr (Maybe (Ptr b)) b where
    fromPtr _ = pure

instance IsPtr (ListOf b Float) b where
    fromPtr msg = fmap (fmap wordToFloat) . fromPtr msg
instance IsPtr (ListOf b Double) b where
    fromPtr msg = fmap (fmap wordToDouble) . fromPtr msg

instance IsPtr (ListOf b Int8) b where
    fromPtr msg = fmap (fmap (fromIntegral :: Word8 -> Int8)) . fromPtr msg
instance IsPtr (ListOf b Int16) b where
    fromPtr msg = fmap (fmap (fromIntegral :: Word16 -> Int16)) . fromPtr msg
instance IsPtr (ListOf b Int32) b where
    fromPtr msg = fmap (fmap (fromIntegral :: Word32 -> Int32)) . fromPtr msg
instance IsPtr (ListOf b Int64) b where
    fromPtr msg = fmap (fmap (fromIntegral :: Word64 -> Int64)) . fromPtr msg

instance IsPtr (Data b) b where
    fromPtr msg ptr = fromPtr msg ptr >>= BuiltinTypes.getData
instance IsPtr (Text b) b where
    fromPtr msg ptr = fromPtr msg ptr >>= BuiltinTypes.getText
