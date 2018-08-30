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
module Data.Capnp.GenHelpers.Pure where

import Control.Monad.Catch.Pure (runCatchT)
import Data.Either              (fromRight)
import Data.Functor.Identity    (runIdentity)

import Data.Capnp.TraversalLimit (evalLimitT)

import qualified Data.Capnp.Classes as C
import qualified Data.Capnp.Message as M
import qualified Data.Capnp.Untyped as U

-- | A valid implementation for 'Data.Default.Default' for any type that meets
-- the given constraints.
defaultStruct :: (C.Decerialize a, C.FromStruct M.ConstMsg (C.Cerial M.ConstMsg a)) => a
defaultStruct =
    fromRight (error "impossible") $
    runIdentity $
    runCatchT $
    evalLimitT maxBound $
        U.rootPtr M.empty >>= C.fromStruct >>= C.decerialize
