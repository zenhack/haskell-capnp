{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
module Data.Capnp.ById.Xb8630836983feed7 where

-- Code generated by capnpc-haskell. DO NOT EDIT.
-- Generated from schema file: /usr/include/capnp/persistent.capnp

import Data.Int
import Data.Word
import qualified Data.Bits

import qualified Codec.Capnp
import qualified Data.Capnp.BuiltinTypes
import qualified Data.Capnp.TraversalLimit
import qualified Data.Capnp.Untyped

import qualified Data.Capnp.ById.Xbdf87d7bb8304e81

newtype Persistent'SaveResults (m :: * -> *) b = Persistent'SaveResults (Data.Capnp.Untyped.Struct m b)

instance Data.Capnp.Untyped.ReadCtx m b => Codec.Capnp.IsStruct m (Persistent'SaveResults m b) b where
    fromStruct = pure . Persistent'SaveResults
get_Persistent'SaveResults'sturdyRef :: Data.Capnp.Untyped.ReadCtx m b => Persistent'SaveResults m b -> m (Maybe (Data.Capnp.Untyped.Ptr m b))
get_Persistent'SaveResults'sturdyRef (Persistent'SaveResults struct) =
    Data.Capnp.Untyped.getPtr 0 struct
    >>= Codec.Capnp.fromPtr (Data.Capnp.Untyped.message struct)

newtype Persistent'SaveParams (m :: * -> *) b = Persistent'SaveParams (Data.Capnp.Untyped.Struct m b)

instance Data.Capnp.Untyped.ReadCtx m b => Codec.Capnp.IsStruct m (Persistent'SaveParams m b) b where
    fromStruct = pure . Persistent'SaveParams
get_Persistent'SaveParams'sealFor :: Data.Capnp.Untyped.ReadCtx m b => Persistent'SaveParams m b -> m (Maybe (Data.Capnp.Untyped.Ptr m b))
get_Persistent'SaveParams'sealFor (Persistent'SaveParams struct) =
    Data.Capnp.Untyped.getPtr 0 struct
    >>= Codec.Capnp.fromPtr (Data.Capnp.Untyped.message struct)
