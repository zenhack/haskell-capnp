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

import Capnp.TraversalLimit (evalLimitT)

import qualified Data.Vector as V

import Codec.Capnp        (getRoot)
import Data.Foldable      (for_)
import Data.Mutable       (freeze)
import Internal.BuildPure (createPure)

import qualified Capnp.Classes as C
import qualified Capnp.Convert as Convert
import qualified Capnp.Message as M
import qualified Capnp.Untyped as U

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
    , C.Cerialize a
    , C.ToStruct (M.MutMsg s) (C.Cerial (M.MutMsg s) a)
    , C.Decerialize b
    , C.FromStruct M.ConstMsg (C.Cerial M.ConstMsg b)
    ) => a -> m b
convertValue from = do
    constMsg :: M.ConstMsg <- Convert.valueToMsg from >>= freeze
    Convert.msgToValue constMsg >>= C.decerialize

-- | convert a low-level value to a high-level one. This is not safe against
-- malicious or invalid input; it is used for declaring top-level constants.
toPurePtrConst :: C.Decerialize a => C.Cerial M.ConstMsg a -> a
toPurePtrConst = fromJust . evalLimitT maxBound . C.decerialize

-- | A valid implementation of 'cerialize', which just cerializes the
-- elements of a list individually and puts them in the list.
--
-- Note that while this is *correct* for composite lists, it is inefficient,
-- since it will separately allocate the elements and then copy them into
-- the list, doing extra work and leaking space. See 'cerializeCompositeVec'.
cerializeBasicVec ::
    ( U.RWCtx m s
    , C.MutListElem s (C.Cerial (M.MutMsg s) a)
    , C.Cerialize a
    )
    => M.MutMsg s
    -> V.Vector a
    -> m (C.List (M.MutMsg s) (C.Cerial (M.MutMsg s) a))
cerializeBasicVec msg vec = do
    list <- C.newList msg (V.length vec)
    for_ [0..V.length vec - 1] $ \i -> do
        e <- C.cerialize msg (vec V.! i)
        C.setIndex e i list
    pure list

-- | A valid implementation of 'cerialize', which allocates a list of the
-- correct size and then marshals the elements of a vector into the elements
-- of the list. This is more efficient for composite types than
-- 'cerializeBasicVec', hence the name.
cerializeCompositeVec ::
    ( U.RWCtx m s
    , C.MutListElem s (C.Cerial (M.MutMsg s) a)
    , C.Marshal a
    )
    => M.MutMsg s
    -> V.Vector a
    -> m (C.List (M.MutMsg s) (C.Cerial (M.MutMsg s) a))
cerializeCompositeVec msg vec = do
    list <- C.newList msg (V.length vec)
    for_ [0..V.length vec - 1] $ \i -> do
        targ <- C.index i list
        C.marshalInto targ (vec V.! i)
    pure list
