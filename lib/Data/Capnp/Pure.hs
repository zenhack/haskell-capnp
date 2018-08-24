{- |
Module: Data.Capnp.Pure
Description: The most commonly used functionality from the high-level API.
-}
module Data.Capnp.Pure
    ( Basics.Text(..)
    , Basics.Data(..)

    , Capnp.ConstMsg
    , Capnp.Message(..)
    , Capnp.decodeMessage
    , Capnp.encodeMessage
    , Capnp.getRoot
    , Capnp.getValue
    , Capnp.hGetValue
    , putValue
    , hPutValue

    , Codec.Decerialize(..)
    , Codec.Cerialize(..)

    , module Data.Capnp.TraversalLimit

    , def
    ) where


import Data.Default (def)

import Data.Capnp.TraversalLimit

import Data.Capnp.IO (hPutValue, putValue)

import qualified Codec.Capnp            as Codec
import qualified Data.Capnp             as Capnp
import qualified Data.Capnp.Basics.Pure as Basics
