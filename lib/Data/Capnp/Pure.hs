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

    , Codec.Decerialize(..)
    , Codec.Cerialize(..)
    ) where

import qualified Codec.Capnp            as Codec
import qualified Data.Capnp             as Capnp
import qualified Data.Capnp.Basics.Pure as Basics
