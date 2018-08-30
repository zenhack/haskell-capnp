{- |
Module: Data.Capnp.Pure
Description: The most commonly used functionality from the high-level API.

This module re-exports the most commonly used functionality from the high-level
API. See also "Data.Capnp", which does the same for the low-level API.

Users getting acquainted with the library are *strongly* encouraged to read the
"Data.Capnp.Tutorial" module before anything else.
-}
module Data.Capnp.Pure
    (
    -- * Reading and writing values
      hPutValue
    , Capnp.hGetValue
    , putValue
    , Capnp.getValue


    -- * Working directly with messages
    , Capnp.decodeMessage
    , Capnp.encodeMessage
    , Capnp.ConstMsg
    , Capnp.Message(..)

    -- * Getting values in and out of messages
    , Capnp.getRoot
    , Classes.Decerialize(..)
    , Classes.Cerialize(..)

    -- * Managing resource limits
    , module Data.Capnp.TraversalLimit


    -- * Aliases for built-in capnproto types.
    , Basics.Text(..)
    , Basics.Data(..)

    -- * Re-exported from data-default
    , def
    ) where


import Data.Default (def)

import Data.Capnp.TraversalLimit

import Data.Capnp.IO (hPutValue, putValue)

import qualified Data.Capnp             as Capnp
import qualified Data.Capnp.Basics.Pure as Basics
import qualified Data.Capnp.Classes     as Classes
