{- |
Module: Data.Capnp.Pure
Description: The most commonly used functionality from the high-level API.
-}
module Data.Capnp.Pure
    ( Basics.Text(..)
    , Basics.Data(..)

    , Codec.getValue
    , Codec.hGetValue
    , Codec.putValue
    , Codec.hPutValue

    , Capnp.ConstMsg
    , Capnp.Message(..)
    , Capnp.decodeMessage
    , Capnp.encodeMessage
    , Capnp.getRoot

    , Codec.Decerialize(..)
    , Codec.Cerialize(..)

    , module Data.Capnp.TraversalLimit

    , def
    ) where


import Data.Default (def)

import Data.Capnp.TraversalLimit

import qualified Codec.Capnp            as Codec
import qualified Data.Capnp             as Capnp
import qualified Data.Capnp.Basics.Pure as Basics
