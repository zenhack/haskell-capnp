{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{- |
Module: Capnp.Capnp.Persistent.Pure
Description: High-level generated module for capnp/persistent.capnp

This module is the generated code for capnp/persistent.capnp,
for the high-level api.
-}
module Capnp.Capnp.Persistent.Pure where

-- Code generated by capnpc-haskell. DO NOT EDIT.
-- Generated from schema file: capnp/persistent.capnp

import Data.Int
import Data.Word

import Data.Capnp.Untyped.Pure (List)
import Data.Capnp.Basics.Pure (Data, Text)
import Control.Monad.Catch (MonadThrow)
import Data.Capnp.TraversalLimit (MonadLimit)

import qualified Data.Capnp.Message as M'
import qualified Data.Capnp.Untyped.Pure as PU'
import qualified Codec.Capnp as C'

import qualified Capnp.ById.Xb8630836983feed7
import qualified Capnp.ById.Xbdf87d7bb8304e81.Pure
import qualified Capnp.ById.Xbdf87d7bb8304e81

data Persistent'SaveParams
    = Persistent'SaveParams
        { sealFor :: Maybe (PU'.PtrType)
        }
    deriving(Show, Read, Eq)

instance C'.Decerialize (Capnp.ById.Xb8630836983feed7.Persistent'SaveParams M'.ConstMsg) Persistent'SaveParams where
    decerialize raw = Persistent'SaveParams
                <$> (Capnp.ById.Xb8630836983feed7.get_Persistent'SaveParams'sealFor raw >>= C'.decerialize)

instance C'.IsStruct M'.ConstMsg Persistent'SaveParams where
    fromStruct struct = do
        raw <- C'.fromStruct struct
        C'.decerialize (raw :: Capnp.ById.Xb8630836983feed7.Persistent'SaveParams M'.ConstMsg)

instance C'.Cerialize s Persistent'SaveParams (Capnp.ById.Xb8630836983feed7.Persistent'SaveParams (M'.MutMsg s)) where
    cerialize msg value_ = do
        raw <- C'.new msg
        pure raw
data Persistent'SaveResults
    = Persistent'SaveResults
        { sturdyRef :: Maybe (PU'.PtrType)
        }
    deriving(Show, Read, Eq)

instance C'.Decerialize (Capnp.ById.Xb8630836983feed7.Persistent'SaveResults M'.ConstMsg) Persistent'SaveResults where
    decerialize raw = Persistent'SaveResults
                <$> (Capnp.ById.Xb8630836983feed7.get_Persistent'SaveResults'sturdyRef raw >>= C'.decerialize)

instance C'.IsStruct M'.ConstMsg Persistent'SaveResults where
    fromStruct struct = do
        raw <- C'.fromStruct struct
        C'.decerialize (raw :: Capnp.ById.Xb8630836983feed7.Persistent'SaveResults M'.ConstMsg)

instance C'.Cerialize s Persistent'SaveResults (Capnp.ById.Xb8630836983feed7.Persistent'SaveResults (M'.MutMsg s)) where
    cerialize msg value_ = do
        raw <- C'.new msg
        pure raw
