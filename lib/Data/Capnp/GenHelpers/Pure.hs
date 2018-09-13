{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{- |
Module: Data.Capnp.GenHelpers.Pure
Description: Misc. helpers for generated code.

This module provides various helpers used by generated code; developers
are not expected to invoke them directly.

These helpers are only used by the high-level api. "Data.Capnp.GenHelpers"
defines helpers used by the low-level api.
-}
module Data.Capnp.GenHelpers.Pure (defaultStruct, encodeV) where

import Data.Maybe (fromJust)

import Codec.Capnp               (encodeV)
import Data.Capnp.TraversalLimit (evalLimitT)

import qualified Data.Capnp.Classes as C
import qualified Data.Capnp.Message as M
import qualified Data.Capnp.Untyped as U

-- | A valid implementation for 'Data.Default.Default' for any type that meets
-- the given constraints.
defaultStruct :: (C.Decerialize a, C.FromStruct M.ConstMsg (C.Cerial M.ConstMsg a)) => a
defaultStruct =
    fromJust $
    evalLimitT maxBound $
        U.rootPtr M.empty >>= C.fromStruct >>= C.decerialize


-- | Convert a low-level constant to a high-level constant. This trusts the
-- input, using maxBound as the traversal limit and calling 'error' on
-- decoding failures. It's purpose is defining constants the high-level
-- modules.
toPurePtrConst :: C.Decerialize a => C.Cerial M.ConstMsg a -> a
toPurePtrConst = fromJust . evalLimitT maxBound . C.decerialize
