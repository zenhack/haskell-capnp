{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Codec.Capnp.Generic
    ( IsWord(..)
    , ListElem(..)
    , MutListElem(..)
    , IsPtr(..)
    , IsStruct(..)
    , getWordField
    ) where

import Codec.Capnp (IsWord(..), ListElem(..), MutListElem(..))

import Data.Bits
import Data.Word

import Control.Monad.Catch        (MonadThrow(throwM))
import Data.Capnp.Basics.Generic  (Data, Text)
import Data.Capnp.Errors          (Error(SchemaViolationError))
import Data.Capnp.Untyped.Generic
    (ListOf, Ptr(..), ReadCtx, Struct, getData, messageDefault)

import qualified Data.Capnp.Basics.Generic  as Basics
import qualified Data.Capnp.Untyped.Generic as U

-- | Types that can be extracted from an untyped pointer.
--
-- Similarly to 'IsWord', this is mostly used in generated code, to interact
-- with the pointer section of structs.
class IsPtr msg a where
    fromPtr :: ReadCtx m msg => msg -> Maybe (Ptr msg) -> m a

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

-- IsPtr instance for lists of Void/().
instance IsPtr msg (ListOf msg ()) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (U.List0 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 0"

-- IsPtr instances for lists of unsigned integers.
instance IsPtr msg (ListOf msg Word8) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (U.List8 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 8"
instance IsPtr msg (ListOf msg Word16) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (U.List16 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 16"
instance IsPtr msg (ListOf msg Word32) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (U.List32 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 32"
instance IsPtr msg (ListOf msg Word64) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (U.List64 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 64"

-- | IsPtr instance for pointers -- this is just the identity.
instance IsPtr msg (Maybe (Ptr msg)) where
    fromPtr _ = pure

-- IsPtr instance for composite lists.
instance IsPtr msg (ListOf msg (Struct msg)) where
    fromPtr msg Nothing                            = pure $ messageDefault msg
    fromPtr msg (Just (PtrList (U.ListStruct list))) = pure list
    fromPtr _ _ = expected "pointer to list of structs"

-- IsPtr instances for Text and Data. These wrap lists of bytes.
instance IsPtr msg (Data msg) where
    fromPtr msg ptr = fromPtr msg ptr >>= Basics.getData
instance IsPtr msg (Text msg) where
    fromPtr msg ptr = case ptr of
        Just _ ->
            fromPtr msg ptr >>= Basics.getText
        Nothing -> do
            -- getText expects and strips off a NUL byte at the end of the
            -- string. In the case of a null pointer we just want to return
            -- the empty string, so we bypass it here.
            Basics.Data bytes <- fromPtr msg ptr
            pure $ Basics.Text bytes

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
