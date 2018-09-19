{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{- |
Module: Data.Capnp.GenHelpers.Pure
Description: Misc. helpers for generated code.

This module provides various helpers used by generated code; developers
are not expected to invoke them directly.

These helpers are only used by the high-level api. "Data.Capnp.GenHelpers"
defines helpers used by the low-level api.
-}
module Data.Capnp.GenHelpers.Pure (defaultStruct, convertValue) where

import Data.Maybe (fromJust)

import Data.Capnp.TraversalLimit (evalLimitT)
import Data.Mutable              (freeze)

import qualified Data.Capnp.Classes as C
import qualified Data.Capnp.Convert as Convert
import qualified Data.Capnp.Message as M
import qualified Data.Capnp.Untyped as U

-- | A valid implementation for 'Data.Default.Default' for any type that meets
-- the given constraints.
defaultStruct :: (C.Decerialize a, C.FromStruct M.ConstMsg (C.Cerial M.ConstMsg a)) => a
defaultStruct =
    fromJust $
    evalLimitT maxBound $
        U.rootPtr M.empty >>= C.fromStruct >>= C.decerialize

convertValue ::
    ( U.RWCtx m s
    , M.Message m M.ConstMsg
    , C.Cerialize s a
    , C.ToStruct (M.MutMsg s) (C.Cerial (M.MutMsg s) a)
    , C.Decerialize b
    , C.FromStruct M.ConstMsg (C.Cerial M.ConstMsg b)
    ) => a -> m b
convertValue from = do
    constMsg :: M.ConstMsg <- Convert.valueToMsg from >>= freeze
    Convert.msgToValue constMsg >>= C.decerialize
