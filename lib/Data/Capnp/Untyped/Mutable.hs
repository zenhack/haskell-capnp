module Data.Capnp.Untyped.Mutable
    (
    -- * Special-case type aliases
    Ptr(..), List(..), Struct(..), ListOf(..)
    -- * Re-exported from 'Data.Capnp.Untyped.Generic':
    , GU.dataSection, GU.ptrSection
    , GU.getData, GU.getPtr
    , GU.get, GU.index, GU.length
    , GU.take
    , GU.rootPtr
    , GU.rawBytes
    , GU.ReadCtx
    , GU.HasMessage(..), GU.MessageDefault(..)
    ) where

import Data.Word

import qualified Data.Capnp.Message.Mutable as MM
import qualified Data.Capnp.Untyped.Generic as GU

type Ptr s = GU.Ptr (MM.Message s)
type List s = GU.List (MM.Message s)
type Struct s = GU.Struct (MM.Message s)
type ListOf s a  = GU.ListOf (MM.Message s) a
