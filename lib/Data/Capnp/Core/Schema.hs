{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Data.Capnp.Core.Schema
    (
    -- re-export the generated code:
    module Data.Capnp.ById.Xa93fc509624c72d9.Pure

    ------ Stuff we don't generate yet:

    -- constants
    , field'noDiscriminant
    )
    where

import Prelude hiding (id)

import Data.Bits
import Data.Capnp.ById.Xa93fc509624c72d9.Pure
import Data.Capnp.Untyped.Pure
import Data.Word

import Codec.Capnp               (Decerialize(..))
import Control.Monad.Catch       (MonadThrow)
import Data.Capnp.TraversalLimit (MonadLimit)
import Data.ReinterpretCast      (wordToDouble, wordToFloat)

field'noDiscriminant :: Word16
field'noDiscriminant = 0xffff
