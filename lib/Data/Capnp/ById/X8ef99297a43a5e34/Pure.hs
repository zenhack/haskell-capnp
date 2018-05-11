{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.Capnp.ById.X8ef99297a43a5e34.Pure where

-- generated from /usr/include/capnp/json.capnp

import Data.Int
import Data.Word

import Data.Capnp.Untyped.Pure (Text, Data, List)

import qualified Data.Capnp.Untyped.Pure
import qualified Codec.Capnp

import qualified Data.Capnp.ById.Xbdf87d7bb8304e81.Pure

data JsonValue
    = JsonValue
        { union' :: JsonValue'
        }
    deriving(Show, Read, Eq)

data JsonValue'
    = JsonValue'null
    | JsonValue'boolean (Bool)
    | JsonValue'number (Double)
    | JsonValue'string (Data.Capnp.Untyped.Pure.Text)
    | JsonValue'array (Data.Capnp.Untyped.Pure.List (JsonValue))
    | JsonValue'object (Data.Capnp.Untyped.Pure.List (JsonValue'Field))
    | JsonValue'call (JsonValue'Call)
    deriving(Show, Read, Eq)

data JsonValue'Call
    = JsonValue'Call
        { function :: Data.Capnp.Untyped.Pure.Text
        , params :: Data.Capnp.Untyped.Pure.List (JsonValue)
        }
    deriving(Show, Read, Eq)

data JsonValue'Field
    = JsonValue'Field
        { name :: Data.Capnp.Untyped.Pure.Text
        , value :: JsonValue
        }
    deriving(Show, Read, Eq)

