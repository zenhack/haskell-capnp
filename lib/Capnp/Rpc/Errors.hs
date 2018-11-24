{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Capnp.Rpc.Errors
Description: helpers for constructing capnproto exceptions.
-}
module Capnp.Rpc.Errors
    ( eMethodUnimplemented
    , eDisconnected
    , eFailed
    ) where

import Data.Default (Default(def))
import Data.Text    (Text)

import Capnp.Gen.Capnp.Rpc.Pure (Exception(..), Exception'Type(..))

-- | Construct an exception with a type field of failed and the
-- given text as its reason.
eFailed :: Text -> Exception
eFailed reason = def
    { type_ = Exception'Type'failed
    , reason = reason
    }

-- | An exception with type = disconnected
eDisconnected :: Exception
eDisconnected = def
    { type_ = Exception'Type'disconnected
    , reason = "Disconnected"
    }

-- | An exception indicating an unimplemented method.
eMethodUnimplemented :: Exception
eMethodUnimplemented = def
    { type_ = Exception'Type'unimplemented
    , reason = "Method unimplemented"
    }
