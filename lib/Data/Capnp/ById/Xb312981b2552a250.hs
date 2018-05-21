{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.Capnp.ById.Xb312981b2552a250 where

-- generated from /usr/include/capnp/rpc.capnp

import Data.Int
import Data.Word

import qualified Data.Capnp.BuiltinTypes
import qualified Data.Capnp.Untyped

import qualified Data.Capnp.ById.Xbdf87d7bb8304e81

newtype Call b = Call (Data.Capnp.Untyped.Struct b)

data CapDescriptor b
    = CapDescriptor'none
    | CapDescriptor'senderHosted Word32
    | CapDescriptor'senderPromise Word32
    | CapDescriptor'receiverHosted Word32
    | CapDescriptor'receiverAnswer (PromisedAnswer b)
    | CapDescriptor'thirdPartyHosted (ThirdPartyCapDescriptor b)
    | CapDescriptor'unknown' Word16







data Message b
    = Message'unimplemented (Message b)
    | Message'abort (Exception b)
    | Message'call (Call b)
    | Message'return (Return b)
    | Message'finish (Finish b)
    | Message'resolve (Resolve b)
    | Message'release (Release b)
    | Message'obsoleteSave (Maybe (Data.Capnp.Untyped.Ptr b))
    | Message'bootstrap (Bootstrap b)
    | Message'obsoleteDelete (Maybe (Data.Capnp.Untyped.Ptr b))
    | Message'provide (Provide b)
    | Message'accept (Accept b)
    | Message'join (Join b)
    | Message'disembargo (Disembargo b)
    | Message'unknown' Word16















data MessageTarget b
    = MessageTarget'importedCap Word32
    | MessageTarget'promisedAnswer (PromisedAnswer b)
    | MessageTarget'unknown' Word16



newtype Payload b = Payload (Data.Capnp.Untyped.Struct b)

newtype Provide b = Provide (Data.Capnp.Untyped.Struct b)

newtype Return b = Return (Data.Capnp.Untyped.Struct b)

data Return' b
    = Return'results (Payload b)
    | Return'exception (Exception b)
    | Return'canceled
    | Return'resultsSentElsewhere
    | Return'takeFromOtherQuestion Word32
    | Return'acceptFromThirdParty (Maybe (Data.Capnp.Untyped.Ptr b))
    | Return'unknown' Word16







newtype Release b = Release (Data.Capnp.Untyped.Struct b)


newtype Resolve b = Resolve (Data.Capnp.Untyped.Struct b)

data Resolve' b
    = Resolve'cap (CapDescriptor b)
    | Resolve'exception (Exception b)
    | Resolve'unknown' Word16



newtype ThirdPartyCapDescriptor b = ThirdPartyCapDescriptor (Data.Capnp.Untyped.Struct b)

newtype Finish b = Finish (Data.Capnp.Untyped.Struct b)

newtype Accept b = Accept (Data.Capnp.Untyped.Struct b)

data Disembargo'context b
    = Disembargo'context'senderLoopback Word32
    | Disembargo'context'receiverLoopback Word32
    | Disembargo'context'accept
    | Disembargo'context'provide Word32
    | Disembargo'context'unknown' Word16





newtype Exception b = Exception (Data.Capnp.Untyped.Struct b)

newtype PromisedAnswer b = PromisedAnswer (Data.Capnp.Untyped.Struct b)

data Call'sendResultsTo b
    = Call'sendResultsTo'caller
    | Call'sendResultsTo'yourself
    | Call'sendResultsTo'thirdParty (Maybe (Data.Capnp.Untyped.Ptr b))
    | Call'sendResultsTo'unknown' Word16




newtype Bootstrap b = Bootstrap (Data.Capnp.Untyped.Struct b)

data PromisedAnswer'Op b
    = PromisedAnswer'Op'noop
    | PromisedAnswer'Op'getPointerField Word16
    | PromisedAnswer'Op'unknown' Word16



newtype Disembargo b = Disembargo (Data.Capnp.Untyped.Struct b)

newtype Join b = Join (Data.Capnp.Untyped.Struct b)
