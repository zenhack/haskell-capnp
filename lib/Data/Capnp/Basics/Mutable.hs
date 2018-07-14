{-|
Module: Data.Capnp.Basics.Mutable
Description: basic types backed by mutable messages
-}
module Data.Capnp.Basics.Mutable
    ( Text(..)
    , Data(..)
    , List(..)

    -- re-exported from Data.Capnp.Basics.
    , B.getText
    , B.getData
    , B.dataBytes
    , B.textBytes
    ) where

import qualified Data.Capnp.Basics          as B
import qualified Data.Capnp.Message.Mutable as MM

type Text s = B.Text (MM.Message s)
type Data s = B.Data (MM.Message s)
type List s a = B.List (MM.Message s) a
