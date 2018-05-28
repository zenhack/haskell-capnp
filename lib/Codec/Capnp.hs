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
import Data.ReinterpretCast    (wordToDouble, wordToFloat)

import qualified Data.Capnp.BuiltinTypes as BuiltinTypes
import qualified Data.Capnp.Message      as M

class Decerialize from to where
    decerialize :: MonadThrow m => from -> m to

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

-- IsPtr instance for lists of Void/().
instance ReadCtx m b => IsPtr m (ListOf m b ()) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List0 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 0"

-- IsPtr instances for lists of unsigned integers.
--
-- For some reason GHC is telling me that these overlap with the instance
-- defined for IsPtr a b => IsPtr (List b a) b. This makes no sense to me,
-- because e.g. there's no instance for IsPtr Word8 b. But for now we add
-- the OVERLAPS pragmas; I'll figure it out later.
instance {-# OVERLAPS #-} ReadCtx m b => IsPtr m (ListOf m b Word8) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List8 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 8"
instance {-# OVERLAPS #-} ReadCtx m b => IsPtr m (ListOf m b Word16) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List16 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 16"
instance {-# OVERLAPS #-} ReadCtx m b => IsPtr m (ListOf m b Word32) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List32 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 32"
instance {-# OVERLAPS #-} ReadCtx m b => IsPtr m (ListOf m b Word64) b where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (List64 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 64"

-- | IsPtr instance for pointers -- this is just the identity.
instance {-# OVERLAPS #-} ReadCtx m b => IsPtr m (Maybe (Ptr m b)) b where
    fromPtr _ = pure

-- IsPtr instance for composite lists.
instance {-# OVERLAPS #-} ReadCtx m b => IsPtr m (ListOf m b (Struct m b)) b where
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
instance {-# OVERLAPS #-} ReadCtx m b => IsPtr m (Text b) b where
    -- I don't understand why but without the OVERLAPS pragma, we get an error
    -- about overlapping instances of IsPtr for Text when building one of the
    -- schema. The other matching instance it mentions is the one for
    -- IsStruct => IsPtr, which makes no sense, because there's no IsStruct
    -- instance for Text. Indeed, if we comment out this instance, it
    -- complains that there is no IsStruct instance for Text. Need to figure
    -- this one out at some point.
    fromPtr msg ptr = fromPtr msg ptr >>= BuiltinTypes.getText

-- IsPtr instance for lists of pointer types.
instance {-# OVERLAPS #-} (ReadCtx m b, IsPtr m a b) => IsPtr m (ListOf m b a) b where
    fromPtr msg ptr = flatten . fmap (fromPtr msg) <$> fromPtr msg ptr

-- IsPtr instances for structs.
instance (ReadCtx m b, IsStruct m a b) => IsPtr m a b where
    fromPtr msg Nothing              = fromStruct (messageDefault msg :: Struct m b)
    fromPtr msg (Just (PtrStruct s)) = fromStruct s
    fromPtr _ _                      = expected "pointer to struct"

-- IsStruct instance for Struct; just the identity.
instance ReadCtx m b => IsStruct m (Struct m b) b where
    fromStruct = pure
