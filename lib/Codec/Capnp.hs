{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Codec.Capnp
    ( IsWord(..)
    , ListElem(..)
    , MutListElem(..)
    , IsPtr(..)
    , IsStruct(..)
    , Allocate(..)
    , Cerialize(..)
    , Decerialize(..)
    , getWordField
    , setWordField
    , expected
    ) where

import Data.Bits
import Data.Int
import Data.ReinterpretCast
import Data.Word

import Control.Monad.Catch (MonadThrow(throwM))
import Data.Capnp.Bits     (Word1(..), replaceBits)
import Data.Capnp.Errors   (Error(SchemaViolationError))
import Data.Capnp.Untyped
    (ListOf, Ptr(..), ReadCtx, Struct, getData, messageDefault, setData)

import qualified Data.Capnp.Message as M
import qualified Data.Capnp.Untyped as U

-- | Types that can be converted to and from a 64-bit word.
--
-- This is mostly a helper for generated code, which uses it to interact
-- with the data sections of structs.
class IsWord a where
    fromWord :: Word64 -> a
    toWord :: a -> Word64

class ListElem msg e where
    data List msg e
    length :: List msg e -> Int
    index :: U.ReadCtx m msg => Int -> List msg e -> m e

class MutListElem s e where
    setIndex :: U.RWCtx m s => e -> Int -> List (M.MutMsg s) e -> m ()
    newList :: M.WriteCtx m s => M.MutMsg s -> Int -> m (List (M.MutMsg s) e)

class Allocate s e where
    new :: M.WriteCtx m s => M.MutMsg s -> m e

class Decerialize from to where
    decerialize :: U.ReadCtx m M.ConstMsg => from -> m to

class Cerialize s from to where
    cerialize :: U.RWCtx m s => M.MutMsg s -> from -> m to
    marshalInto :: U.RWCtx m s => to -> from -> m ()

-- | Types that can be converted to and from an untyped pointer.
--
-- Similarly to 'IsWord', this is mostly used in generated code, to interact
-- with the pointer section of structs.
class IsPtr msg a where
    fromPtr :: ReadCtx m msg => msg -> Maybe (Ptr msg) -> m a
    toPtr :: a -> Maybe (Ptr msg)

-- | Types that can be extracted from a struct.
class IsStruct msg a where
    fromStruct :: ReadCtx m msg => Struct msg -> m a

expected :: MonadThrow m => String -> m a
expected msg = throwM $ SchemaViolationError $ "expected " ++ msg

-- | @'getWordField' struct index offset def@ fetches a field from the
-- struct's data section. @index@ is the index of the 64-bit word in the data
-- section in which the field resides. @offset@ is the offset in bits from the
-- start of that word to the field. @def@ is the default value for this field.
getWordField :: (ReadCtx m msg, IsWord a) => Struct msg -> Int -> Int -> Word64 -> m a
getWordField struct idx offset def = fmap
    ( fromWord
    . xor def
    . (`shiftR` offset)
    )
    (getData idx struct)

-- | @'setWordField' struct value index offset def@ sets a field in the
-- struct's data section. The meaning of the parameters are as in
-- 'getWordField', with @value@ being the value to set. The width of the
-- value is inferred from its type.
setWordField ::
    ( U.RWCtx m s
    , Bounded a, Integral a, IsWord a, Bits a
    )
    => Struct (M.MutMsg s) -> a -> Int -> Int -> a -> m ()
setWordField struct value idx offset def = do
    old <- getData idx struct
    let new = replaceBits (value `xor` def) old offset
    setData new idx struct

instance Decerialize () () where
    decerialize = pure
instance Cerialize s () () where
    cerialize _ = pure

instance IsWord Bool where
    fromWord n = (n .&. 1) == 1
    toWord True  = 1
    toWord False = 0

instance IsWord Word1 where
    fromWord = Word1 . fromWord
    toWord = toWord . word1ToBool

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
instance IsPtr msg (ListOf msg ()) where
    fromPtr msg Nothing                         = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (U.List0 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 0"
    toPtr = Just . PtrList . U.List0

-- IsPtr instances for lists of unsigned integers.
instance IsPtr msg (ListOf msg Word8) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (U.List8 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 8"
    toPtr = Just . PtrList . U.List8
instance IsPtr msg (ListOf msg Word16) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (U.List16 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 16"
    toPtr = Just . PtrList . U.List16
instance IsPtr msg (ListOf msg Word32) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (U.List32 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 32"
    toPtr = Just . PtrList . U.List32
instance IsPtr msg (ListOf msg Word64) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (U.List64 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 64"
    toPtr = Just . PtrList . U.List64


instance IsPtr msg (ListOf msg Bool) where
    fromPtr msg Nothing = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (U.List1 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 1."
    toPtr = Just . PtrList . U.List1

-- | IsPtr instance for pointers -- this is just the identity.
instance IsPtr msg (Maybe (Ptr msg)) where
    fromPtr _ = pure
    toPtr = id

-- IsPtr instance for composite lists.
instance IsPtr msg (ListOf msg (Struct msg)) where
    fromPtr msg Nothing                            = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (U.ListStruct list))) = pure list
    fromPtr _ _ = expected "pointer to list of structs"
    toPtr = Just . PtrList . U.ListStruct

-- IsStruct instance for Struct; just the identity.
instance IsStruct msg (Struct msg) where
    fromStruct = pure

instance IsPtr msg (Struct msg) where
    fromPtr msg Nothing              = fromStruct (go msg) where
        -- the type checker needs a bit of help inferring the type here.
        go :: msg -> Struct msg
        go = messageDefault
    fromPtr msg (Just (PtrStruct s)) = fromStruct s
    fromPtr _ _                      = expected "pointer to struct"
    toPtr = Just . PtrStruct
