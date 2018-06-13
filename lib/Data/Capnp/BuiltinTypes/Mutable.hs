{-|
Module: Data.Capnp.BuitlinTypes.Mutable
Description: built-in types backed by mutable messages.
-}
module Data.Capnp.BuiltinTypes.Mutable
    ( Text(..)
    , Data(..)
    , List(..)

    -- re-exported from Generic.
    , GB.map
    , GB.getText
    , GB.getData
    , GB.getList
    , GB.dataBytes
    , GB.textBytes
    ) where

import qualified Data.Capnp.BuiltinTypes.Generic as GB
import qualified Data.Capnp.Message.Mutable      as MM

type Text s = GB.Text (MM.Message s)
type Data s = GB.Data (MM.Message s)
type List s a = GB.List (MM.Message s) a
