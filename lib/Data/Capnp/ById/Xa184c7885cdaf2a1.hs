{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.Capnp.ById.Xa184c7885cdaf2a1 where

-- generated from /usr/include/capnp/rpc-twoparty.capnp

import Data.Int
import Data.Word

import qualified Data.Capnp.BuiltinTypes
import qualified Data.Capnp.Untyped

import qualified Data.Capnp.ById.Xbdf87d7bb8304e81

newtype JoinKeyPart b = JoinKeyPart (Data.Capnp.Untyped.Struct b)

get_JoinKeyPart'joinId :: Data.Capnp.Untyped.ReadCtx m b => JoinKeyPart b -> m Word32
get_JoinKeyPart'joinId = undefined -- TODO: generate accessor values.

get_JoinKeyPart'partCount :: Data.Capnp.Untyped.ReadCtx m b => JoinKeyPart b -> m Word16
get_JoinKeyPart'partCount = undefined -- TODO: generate accessor values.

get_JoinKeyPart'partNum :: Data.Capnp.Untyped.ReadCtx m b => JoinKeyPart b -> m Word16
get_JoinKeyPart'partNum = undefined -- TODO: generate accessor values.

newtype JoinResult b = JoinResult (Data.Capnp.Untyped.Struct b)

get_JoinResult'joinId :: Data.Capnp.Untyped.ReadCtx m b => JoinResult b -> m Word32
get_JoinResult'joinId = undefined -- TODO: generate accessor values.

get_JoinResult'succeeded :: Data.Capnp.Untyped.ReadCtx m b => JoinResult b -> m Bool
get_JoinResult'succeeded = undefined -- TODO: generate accessor values.

get_JoinResult'cap :: Data.Capnp.Untyped.ReadCtx m b => JoinResult b -> m (Maybe (Data.Capnp.Untyped.Ptr b))
get_JoinResult'cap = undefined -- TODO: generate accessor values.

data Side b
    = Side'server
    | Side'client
    | Side'unknown' Word16
newtype ProvisionId b = ProvisionId (Data.Capnp.Untyped.Struct b)

get_ProvisionId'joinId :: Data.Capnp.Untyped.ReadCtx m b => ProvisionId b -> m Word32
get_ProvisionId'joinId = undefined -- TODO: generate accessor values.

newtype VatId b = VatId (Data.Capnp.Untyped.Struct b)

get_VatId'side :: Data.Capnp.Untyped.ReadCtx m b => VatId b -> m (Side b)
get_VatId'side = undefined -- TODO: generate accessor values.
