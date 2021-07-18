{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{- |
Module: Capnp.GenHelpers.Pure
Description: Misc. helpers for generated code.

This module provides various helpers used by generated code; developers
are not expected to invoke them directly.

These helpers are only used by the high-level api. "Capnp.GenHelpers"
defines helpers used by the low-level api.
-}
module Capnp.GenHelpers.Pure
    ( defaultStruct
    , convertValue
    , getRoot
    , createPure
    , toPurePtrConst
    , cerializeBasicVec
    , cerializeCompositeVec
    ) where

import Data.Maybe (fromJust)

import Capnp.Classes        (cerializeBasicVec, cerializeCompositeVec)
import Capnp.Message        (Mutability(..))
import Capnp.TraversalLimit (evalLimitT)
import Codec.Capnp          (getRoot)

import Data.Mutable       (freeze)
import Internal.BuildPure (createPure)

import qualified Capnp.Classes as C
import qualified Capnp.Convert as Convert
import qualified Capnp.Message as M
import qualified Capnp.Untyped as U

-- | A valid implementation for 'Data.Default.Default' for any type that meets
-- the given constraints.
defaultStruct :: (C.Decerialize a, C.FromStruct 'Const (C.Cerial 'Const a)) => a
defaultStruct =
    fromJust $
    evalLimitT maxBound $
        U.rootPtr M.empty >>= C.fromStruct >>= C.decerialize

convertValue ::
    ( U.RWCtx m s
    , M.MonadReadMessage 'Const m
    , C.Cerialize s a
    , C.ToStruct ('Mut s) (C.Cerial ('Mut s) a)
    , C.Decerialize b
    , C.FromStruct 'Const (C.Cerial 'Const b)
    ) => a -> m b
convertValue from = do
    constMsg :: M.Message 'Const <- Convert.valueToMsg from >>= freeze
    Convert.msgToValue constMsg >>= C.decerialize

-- | convert a low-level value to a high-level one. This is not safe against
-- malicious or invalid input; it is used for declaring top-level constants.
toPurePtrConst :: C.Decerialize a => C.Cerial 'Const a -> a
toPurePtrConst = fromJust . evalLimitT maxBound . C.decerialize
