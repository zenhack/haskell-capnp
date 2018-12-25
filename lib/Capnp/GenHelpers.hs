{- |
Module: Capnp.GenHelpers
Description: Misc. helpers for generated code.

This module provides various helpers used by generated code; developers
are not expected to invoke them directly.

These helpers are used by the low-level api. "Capnp.GenHelpers.Pure"
defines helpers used by high-level api.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Capnp.GenHelpers where

import Data.Bits
import Data.Word

import Data.Maybe (fromJust)

import qualified Data.ByteString as BS

import Capnp.Bits

import Capnp (bsToMsg, evalLimitT)

import qualified Capnp.Classes as C
import qualified Capnp.Message as M
import qualified Capnp.Untyped as U

-- | @'getWordField' struct index offset def@ fetches a field from the
-- struct's data section. @index@ is the index of the 64-bit word in the data
-- section in which the field resides. @offset@ is the offset in bits from the
-- start of that word to the field. @def@ is the default value for this field.
getWordField :: (U.ReadCtx m msg, C.IsWord a) => U.Struct msg -> Int -> Int -> Word64 -> m a
getWordField struct idx offset def = fmap
    ( C.fromWord
    . xor def
    . (`shiftR` offset)
    )
    (U.getData idx struct)

-- | @'setWordField' struct value index offset def@ sets a field in the
-- struct's data section. The meaning of the parameters are as in
-- 'getWordField', with @value@ being the value to set. The width of the
-- value is inferred from its type.
setWordField ::
    ( U.RWCtx m s
    , Bounded a, Integral a, C.IsWord a, Bits a
    )
    => U.Struct (M.MutMsg s) -> a -> Int -> Int -> Word64 -> m ()
setWordField struct value idx offset def = do
    old <- U.getData idx struct
    let new = replaceBits (value `xor` C.fromWord def) old offset
    U.setData new idx struct

embedCapPtr :: M.WriteCtx m s => M.MutMsg s -> M.Client -> m (Maybe (U.Ptr (M.MutMsg s)))
embedCapPtr msg client =
    Just . U.PtrCap <$> U.appendCap msg client

-- | Get a pointer from a ByteString, where the root object is a struct with
-- one pointer, which is the pointer we will retrieve. This is only safe for
-- trusted inputs; it reads the message with a traversal limit of 'maxBound'
-- (and so is suseptable to denial of service attacks), and it calls 'error'
-- if decoding is not successful.
--
-- The purpose of this is for defining constants of pointer type from a schema.
getPtrConst :: C.FromPtr M.ConstMsg a => BS.ByteString -> a
getPtrConst bytes = fromJust $ do
    msg <- bsToMsg bytes
    evalLimitT maxBound $ U.rootPtr msg >>= U.getPtr 0 >>= C.fromPtr msg


getTag :: U.ReadCtx m msg => U.Struct msg -> Int -> m Word16
getTag struct offset = do
    word <- U.getData (offset `div` 4) struct
    pure $ fromIntegral $ word `shiftR` ((offset `mod` 4) * 16)
