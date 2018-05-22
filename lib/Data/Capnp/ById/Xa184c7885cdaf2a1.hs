{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.Capnp.ById.Xa184c7885cdaf2a1 where

-- generated from /usr/include/capnp/rpc-twoparty.capnp

import Data.Int
import Data.Word
import qualified Data.Bits

import qualified Data.Capnp.BuiltinTypes
import qualified Data.Capnp.TraversalLimit
import qualified Data.Capnp.Untyped

import qualified Data.Capnp.ById.Xbdf87d7bb8304e81

newtype JoinKeyPart b = JoinKeyPart (Data.Capnp.Untyped.Struct b)

get_JoinKeyPart'joinId :: Data.Capnp.Untyped.ReadCtx m b => JoinKeyPart b -> m Word32
get_JoinKeyPart'joinId (JoinKeyPart struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_JoinKeyPart'partCount :: Data.Capnp.Untyped.ReadCtx m b => JoinKeyPart b -> m Word16
get_JoinKeyPart'partCount (JoinKeyPart struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 32)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_JoinKeyPart'partNum :: Data.Capnp.Untyped.ReadCtx m b => JoinKeyPart b -> m Word16
get_JoinKeyPart'partNum (JoinKeyPart struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 48)
    )
    (Data.Capnp.Untyped.getData 0 struct)

newtype JoinResult b = JoinResult (Data.Capnp.Untyped.Struct b)

get_JoinResult'joinId :: Data.Capnp.Untyped.ReadCtx m b => JoinResult b -> m Word32
get_JoinResult'joinId (JoinResult struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_JoinResult'succeeded :: Data.Capnp.Untyped.ReadCtx m b => JoinResult b -> m Bool
get_JoinResult'succeeded (JoinResult struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 32)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_JoinResult'cap :: Data.Capnp.Untyped.ReadCtx m b => JoinResult b -> m (Maybe (Data.Capnp.Untyped.Ptr b))
get_JoinResult'cap (JoinResult struct) = undefined -- TODO: handle pointer fields
data Side b
    = Side'server
    | Side'client
    | Side'unknown' Word16
instance Enum (Side b) where
    toEnum = Data.Capnp.BuiltinTypes.fromWord . fromIntegral
    fromEnum = fromIntegral . Data.Capnp.BuiltinTypes.toWord


instance Data.Capnp.BuiltinTypes.IsWord (Side b) where
    fromWord 1 = Side'client
    fromWord 0 = Side'server
    fromWord tag = Side'unknown' (fromIntegral tag)
    toWord Side'client = 1
    toWord Side'server = 0
    toWord (Side'unknown' tag) = fromIntegral tag

newtype ProvisionId b = ProvisionId (Data.Capnp.Untyped.Struct b)

get_ProvisionId'joinId :: Data.Capnp.Untyped.ReadCtx m b => ProvisionId b -> m Word32
get_ProvisionId'joinId (ProvisionId struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)

newtype VatId b = VatId (Data.Capnp.Untyped.Struct b)

get_VatId'side :: Data.Capnp.Untyped.ReadCtx m b => VatId b -> m (Side b)
get_VatId'side (VatId struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)
