{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{- |
Module: Data.Capnp.GenHelpers.Pure
Description: Misc. helpers for generated code.

This module provides various helpers used by generated code; developers
are not expected to invoke them directly.

These helpers are only used by the high-level api. 'Data.Capnp.GenHelpers'
defines helpers used by the low-level api.
-}
module Data.Capnp.GenHelpers.Pure where

import Data.Text.Encoding (encodeUtf8)

import qualified Data.ByteString as BS
import qualified Data.Text       as T

import Data.Capnp.Basics.Pure ()

import qualified Codec.Capnp        as C
import qualified Data.Capnp.Basics  as B
import qualified Data.Capnp.Message as M
import qualified Data.Capnp.Untyped as U

marshalText :: (U.RWCtx m s, U.HasMessage parent (M.MutMsg s))
    => parent -> T.Text -> m (B.Text (M.MutMsg s))
marshalText msg text = do
    let bytes = encodeUtf8 text
    ret@(B.Text buffer) <- B.newText (U.message msg) (BS.length bytes)
    C.marshalInto (B.Data buffer) bytes
    pure ret
