{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.Capnp.ById.X8ef99297a43a5e34.Pure where

-- generated from /usr/include/capnp/json.capnp

import Data.Int
import Data.Word

import Data.Capnp.Untyped.Pure (List)
import Data.Capnp.BuiltinTypes.Pure (Data, Text)

import qualified Data.Capnp.Untyped.Pure
import qualified Codec.Capnp

import qualified Data.Capnp.ById.Xbdf87d7bb8304e81.Pure

data JsonValue
    = JsonValue'null
    | JsonValue'boolean (Bool)
    | JsonValue'number (Double)
    | JsonValue'string (Text)
    | JsonValue'array (List (JsonValue))
    | JsonValue'object (List (JsonValue'Field))
    | JsonValue'call (JsonValue'Call)
    | JsonValue'unknown' (Word16)
    deriving(Show, Read, Eq)

data JsonValue'Call
    = JsonValue'Call
        { function :: Text
        , params :: List (JsonValue)
        }
    deriving(Show, Read, Eq)

data JsonValue'Field
    = JsonValue'Field
        { name :: Text
        , value :: JsonValue
        }
    deriving(Show, Read, Eq)

