{-# LANGUAGE MultiParamTypeClasses #-}
module Codec.Capnp where

import Data.Int
import Data.Word

import Control.Monad.Catch (MonadThrow(throwM))
import Data.Bits           ((.&.))
import Data.Capnp.Errors   (Error(SchemaViolationError))
import Data.Capnp.Untyped
    (List(..), ListOf, Ptr(..), ReadCtx, Struct, messageDefault)

import qualified Data.Capnp.Message as M

class Decerialize from to where
    decerialize :: MonadThrow m => from -> m to

expected :: MonadThrow m => String -> m a
expected msg = throwM $ SchemaViolationError $ "expected " ++ msg

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

getList0 ::  ReadCtx m b => M.Message b -> Maybe (Ptr b) -> m (ListOf b ())
getList0 msg Nothing                       = pure $ messageDefault msg
getList0 msg (Just (PtrList (List0 list))) = pure list
getList0 _ _ = expected "pointer to list with element size 0"

getList8 ::  ReadCtx m b => M.Message b -> Maybe (Ptr b) -> m (ListOf b Word8)
getList8 msg Nothing                       = pure $ messageDefault msg
getList8 msg (Just (PtrList (List8 list))) = pure list
getList8 _ _ = expected "pointer to list with element size 8"

getList16 ::  ReadCtx m b => M.Message b -> Maybe (Ptr b) -> m (ListOf b Word16)
getList16 msg Nothing                        = pure $ messageDefault msg
getList16 msg (Just (PtrList (List16 list))) = pure list
getList16 _ _ = expected "pointer to list with element size 16"

getList32 ::  ReadCtx m b => M.Message b -> Maybe (Ptr b) -> m (ListOf b Word32)
getList32 msg Nothing                        = pure $ messageDefault msg
getList32 msg (Just (PtrList (List32 list))) = pure list
getList32 _ _ = expected "pointer to list with element size 32"

getList64 ::  ReadCtx m b => M.Message b -> Maybe (Ptr b) -> m (ListOf b Word64)
getList64 msg Nothing                        = pure $ messageDefault msg
getList64 msg (Just (PtrList (List64 list))) = pure list
getList64 _ _ = expected "pointer to list with element size 64"

getListPtr ::  ReadCtx m b => M.Message b -> Maybe (Ptr b) -> m (ListOf b (Maybe (Ptr b)))
getListPtr msg Nothing                         = pure $ messageDefault msg
getListPtr msg (Just (PtrList (ListPtr list))) = pure list
getListPtr _ _ = expected "pointer to list of pointers"

getListStruct ::  ReadCtx m b => M.Message b -> Maybe (Ptr b) -> m (ListOf b (Struct b))
getListStruct msg Nothing                            = pure $ messageDefault msg
getListStruct msg (Just (PtrList (ListStruct list))) = pure list
getListStruct _ _ = expected "pointer to list of structs"

getStruct ::  ReadCtx m b => M.Message b -> Maybe (Ptr b) -> m (Struct b)
getStruct msg Nothing              = pure $ messageDefault msg
getStruct msg (Just (PtrStruct s)) = pure s
getStruct _ _                      = expected "pointer to struct"
