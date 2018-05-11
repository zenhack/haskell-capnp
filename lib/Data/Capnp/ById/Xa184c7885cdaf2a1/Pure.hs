{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.Capnp.ById.Xa184c7885cdaf2a1.Pure where

-- generated from /usr/include/capnp/rpc-twoparty.capnp

import Data.Int
import Data.Word

import Data.Capnp.Untyped.Pure (Text, Data, List)

import qualified Data.Capnp.Untyped.Pure
import qualified Codec.Capnp

import qualified Data.Capnp.ById.Xbdf87d7bb8304e81.Pure

data RecipientId
    = RecipientId
    deriving(Show, Read, Eq)

data JoinKeyPart
    = JoinKeyPart
        { joinId :: Word32
        , partCount :: Word16
        , partNum :: Word16
        }
    deriving(Show, Read, Eq)

data JoinResult
    = JoinResult
        { joinId :: Word32
        , succeeded :: Bool
        , cap :: Maybe (Data.Capnp.Untyped.Pure.PtrType)
        }
    deriving(Show, Read, Eq)

data Side
    = Side'server
    | Side'client
    | Side'unknown' (Word16)
    deriving(Show, Read, Eq)

data ThirdPartyCapId
    = ThirdPartyCapId
    deriving(Show, Read, Eq)

data ProvisionId
    = ProvisionId
        { joinId :: Word32
        }
    deriving(Show, Read, Eq)

data VatId
    = VatId
        { side :: Side
        }
    deriving(Show, Read, Eq)

