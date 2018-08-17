{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Codec.Capnp
    ( IsWord(..)
    , ListElem(..)
    , MutListElem(..)
    , IsPtr(..)
    , FromStruct(..)
    , ToStruct(..)
    , Allocate(..)
    , Marshal(..)
    , Cerialize(..)
    , Decerialize(..)
    , expected
    , newRoot
    , setRoot
    , getRoot
    , hGetValue
    , getValue
    , hPutValue
    , putValue
    ) where

import Data.Bits
import Data.Int
import Data.ReinterpretCast
import Data.Word

import Control.Monad.Catch     (MonadThrow(throwM))
import Control.Monad.Primitive (RealWorld)
import System.IO               (Handle, stdin, stdout)

import Data.Capnp.Bits           (Word1(..))
import Data.Capnp.Errors         (Error(SchemaViolationError))
import Data.Capnp.TraversalLimit (evalLimitT)
import Data.Capnp.Untyped
    (ListOf, Ptr(..), ReadCtx, Struct, messageDefault)

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BB

import qualified Data.Capnp.Message as M
import qualified Data.Capnp.Untyped as U

hGetValue :: FromStruct M.ConstMsg a => Int -> Handle -> IO a
hGetValue limit handle = do
    contents <- BS.hGetContents handle
    msg <- M.decode contents
    evalLimitT limit (getRoot msg)

getValue :: FromStruct M.ConstMsg a => Int -> IO a
getValue limit = hGetValue limit stdin

hPutValue :: (Cerialize RealWorld a, ToStruct (M.MutMsg RealWorld) (Cerial (M.MutMsg RealWorld) a))
    => Handle -> a -> IO ()
hPutValue handle value = do
    msg <- M.newMessage
    root <- evalLimitT maxBound $ cerialize msg value
    setRoot root
    constMsg <- M.freeze msg
    bytes <- M.encode constMsg
    BB.hPutBuilder handle bytes

putValue :: (Cerialize RealWorld a, ToStruct (M.MutMsg RealWorld) (Cerial (M.MutMsg RealWorld) a))
    => a -> IO ()
putValue = hPutValue stdout

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

class (ListElem (M.MutMsg s) e) => MutListElem s e where
    setIndex :: U.RWCtx m s => e -> Int -> List (M.MutMsg s) e -> m ()
    newList :: M.WriteCtx m s => M.MutMsg s -> Int -> m (List (M.MutMsg s) e)

class Allocate s e where
    new :: M.WriteCtx m s => M.MutMsg s -> m e

class Decerialize a where
    type Cerial msg a
    decerialize :: U.ReadCtx m M.ConstMsg => Cerial M.ConstMsg a -> m a

class Decerialize a => Marshal a where
    -- | Marshal a value into the pre-allocated object of type @to@.
    marshalInto :: U.RWCtx m s => Cerial (M.MutMsg s) a -> a -> m ()

class Decerialize a => Cerialize s a where
    -- | Cerialize a value into the supplied message, returning the result.
    cerialize :: U.RWCtx m s => M.MutMsg s -> a -> m (Cerial (M.MutMsg s) a)

    default cerialize :: (U.RWCtx m s, Marshal a, Allocate s (Cerial (M.MutMsg s) a))
        => M.MutMsg s -> a -> m (Cerial (M.MutMsg s) a)
    cerialize msg value = do
        raw <- new msg
        marshalInto raw value
        pure raw

expected :: MonadThrow m => String -> m a
expected msg = throwM $ SchemaViolationError $ "expected " ++ msg

-- | 'newRoot' allocates and returns a new value inside the message, setting
-- it as the root object of the message.
newRoot :: (ToStruct (M.MutMsg s) a, Allocate s a, M.WriteCtx m s)
    => M.MutMsg s -> m a
newRoot msg = do
    ret <- new msg
    setRoot ret
    pure ret

-- | 'setRoot' sets its argument to be the root object in its message.
setRoot :: (ToStruct (M.MutMsg s) a, M.WriteCtx m s) => a -> m ()
setRoot = U.setRoot . toStruct

-- | 'getRoot' returns the root object of a message.
getRoot :: (FromStruct msg a, U.ReadCtx m msg) => msg -> m a
getRoot msg = U.rootPtr msg >>= fromStruct

-- | Types that can be converted to and from an untyped pointer.
--
-- Similarly to 'IsWord', this is mostly used in generated code, to interact
-- with the pointer section of structs.
class IsPtr msg a where
    fromPtr :: ReadCtx m msg => msg -> Maybe (Ptr msg) -> m a
    toPtr :: a -> Maybe (Ptr msg)

-- | Types that can be extracted from a struct.
class FromStruct msg a where
    fromStruct :: ReadCtx m msg => Struct msg -> m a

-- | Types that can be converted to a struct.
class ToStruct msg a where
    toStruct :: a -> Struct msg

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

-- FromStruct instance for Struct; just the identity.
instance FromStruct msg (Struct msg) where
    fromStruct = pure

instance ToStruct msg (Struct msg) where
    toStruct = id

instance IsPtr msg (Struct msg) where
    fromPtr msg Nothing              = fromStruct (go msg) where
        -- the type checker needs a bit of help inferring the type here.
        go :: msg -> Struct msg
        go = messageDefault
    fromPtr msg (Just (PtrStruct s)) = fromStruct s
    fromPtr _ _                      = expected "pointer to struct"
    toPtr = Just . PtrStruct
