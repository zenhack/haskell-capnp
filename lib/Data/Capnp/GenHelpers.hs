{- |
Module: Data.Capnp.GenHelpers
Description: Misc. helpers for generated code.

This module provides various helpers used by generated code; developers
are not expected to invoke them directly.

These helpers are used by the low-level api. "Data.Capnp.GenHelpers.Pure"
defines helpers used by high-level api.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Data.Capnp.GenHelpers where

import Data.Bits
import Data.Word

import Data.Capnp.Bits

import qualified Data.Capnp.Classes as C
import qualified Data.Capnp.Message as M
import qualified Data.Capnp.Untyped as U

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
    => U.Struct (M.MutMsg s) -> a -> Int -> Int -> a -> m ()
setWordField struct value idx offset def = do
    old <- U.getData idx struct
    let new = replaceBits (value `xor` def) old offset
    U.setData new idx struct

embedCapPtr :: M.WriteCtx m s => M.MutMsg s -> M.Client -> m (Maybe (U.Ptr (M.MutMsg s)))
embedCapPtr msg client =
    Just . U.PtrCap <$> U.appendCap msg client
