{- |
Module: Capnp
Description: The most commonly used functionality in the capnp package.

This module re-exports the most commonly used functionality from other modules in the
library.

Users getting acquainted with the library are *strongly* encouraged to read the
"Capnp.Tutorial" module before anything else.
-}
module Capnp
    (
    -- * Working with capnproto lists
      Classes.ListElem(..)
    , Classes.MutListElem(..)

    -- * Working with capnproto Text and Data values.
    , Basics.Data
    , Basics.dataBytes
    , Basics.Text
    , Basics.textBytes

    -- * Working with messages
    , Message.ConstMsg
    , Message.Message(..)
    , Message.MutMsg
    , Message.newMessage

    -- * Canonicalizing messages
    , canonicalize

    -- * Manipulating the root object of a message
    , Codec.getRoot
    , Codec.newRoot
    , Codec.setRoot

    -- * Marshalling data into and out of messages
    , Classes.Decerialize(..)
    , Classes.Cerialize(..)

    -- * IO
    , module Capnp.IO

    -- * Type aliases for common contexts
    , Message.WriteCtx
    , Untyped.ReadCtx
    , Untyped.RWCtx

    -- * Converting between messages, Cap'N Proto values, and raw bytes
    , module Capnp.Convert

    -- * Managing resource limits
    , module Capnp.TraversalLimit

    -- * Freezing and thawing values
    , module Data.Mutable

    -- * Building messages in pure code
    , PureBuilder
    , createPure

    -- * Re-exported from "Data.Default", for convienence.
    , def
    ) where

import Data.Default (def)

import Capnp.Convert
import Capnp.IO
import Capnp.TraversalLimit
import Data.Mutable

import Capnp.Canonicalize (canonicalize)
import Internal.BuildPure (PureBuilder, createPure)

import qualified Capnp.Basics  as Basics
import qualified Capnp.Classes as Classes
import qualified Capnp.Message as Message
import qualified Capnp.Untyped as Untyped
import qualified Codec.Capnp   as Codec



-- Just for instances:
import Capnp.Basics.Pure ()
