{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.Capnp.ById.X8ef99297a43a5e34 where

-- generated from /usr/include/capnp/json.capnp

import Data.Int
import Data.Word

import qualified Data.Capnp.BuiltinTypes
import qualified Data.Capnp.Untyped

import qualified Data.Capnp.ById.Xbdf87d7bb8304e81

data JsonValue b
    = JsonValue'null
    | JsonValue'boolean Bool
    | JsonValue'number Double
    | JsonValue'string (Data.Capnp.BuiltinTypes.Text b)
    | JsonValue'array (Data.Capnp.Untyped.ListOf b (JsonValue b))
    | JsonValue'object (Data.Capnp.Untyped.ListOf b (JsonValue'Field b))
    | JsonValue'call (JsonValue'Call b)
    | JsonValue'unknown' Word16








newtype JsonValue'Call b = JsonValue'Call (Data.Capnp.Untyped.Struct b)

get_JsonValue'Call'function :: Data.Capnp.Untyped.ReadCtx m b => JsonValue'Call b -> m (Data.Capnp.BuiltinTypes.Text b)
get_JsonValue'Call'function = undefined -- TODO: generate accessor values.

get_JsonValue'Call'params :: Data.Capnp.Untyped.ReadCtx m b => JsonValue'Call b -> m (Data.Capnp.Untyped.ListOf b (JsonValue b))
get_JsonValue'Call'params = undefined -- TODO: generate accessor values.

newtype JsonValue'Field b = JsonValue'Field (Data.Capnp.Untyped.Struct b)

get_JsonValue'Field'name :: Data.Capnp.Untyped.ReadCtx m b => JsonValue'Field b -> m (Data.Capnp.BuiltinTypes.Text b)
get_JsonValue'Field'name = undefined -- TODO: generate accessor values.

get_JsonValue'Field'value :: Data.Capnp.Untyped.ReadCtx m b => JsonValue'Field b -> m (JsonValue b)
get_JsonValue'Field'value = undefined -- TODO: generate accessor values.
