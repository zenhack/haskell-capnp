{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.Capnp.ById.Xb312981b2552a250.Pure where

-- generated from /usr/include/capnp/rpc.capnp

import Data.Int
import Data.Word

import Data.Capnp.Untyped.Pure (Text, Data, List)

import qualified Data.Capnp.Untyped.Pure
import qualified Codec.Capnp

import qualified Data.Capnp.ById.Xbdf87d7bb8304e81.Pure

data Call
    = Call
        { questionId :: Word32
        , target :: MessageTarget
        , interfaceId :: Word64
        , methodId :: Word16
        , params :: Payload
        , sendResultsTo :: Call'sendResultsTo
        , allowThirdPartyTailCall :: Bool
        }
    deriving(Show, Read, Eq)

data CapDescriptor
    = CapDescriptor
        { union' :: CapDescriptor'
        }
    deriving(Show, Read, Eq)

data CapDescriptor'
    = CapDescriptor'none
    | CapDescriptor'senderHosted (Word32)
    | CapDescriptor'senderPromise (Word32)
    | CapDescriptor'receiverHosted (Word32)
    | CapDescriptor'receiverAnswer (PromisedAnswer)
    | CapDescriptor'thirdPartyHosted (ThirdPartyCapDescriptor)
    deriving(Show, Read, Eq)

data Message
    = Message
        { union' :: Message'
        }
    deriving(Show, Read, Eq)

data Message'
    = Message'unimplemented (Message)
    | Message'abort (Exception)
    | Message'call (Call)
    | Message'return (Return)
    | Message'finish (Finish)
    | Message'resolve (Resolve)
    | Message'release (Release)
    | Message'obsoleteSave (Maybe (Data.Capnp.Untyped.Pure.PtrType))
    | Message'bootstrap (Bootstrap)
    | Message'obsoleteDelete (Maybe (Data.Capnp.Untyped.Pure.PtrType))
    | Message'provide (Provide)
    | Message'accept (Accept)
    | Message'join (Join)
    | Message'disembargo (Disembargo)
    deriving(Show, Read, Eq)

data MessageTarget
    = MessageTarget
        { union' :: MessageTarget'
        }
    deriving(Show, Read, Eq)

data MessageTarget'
    = MessageTarget'importedCap (Word32)
    | MessageTarget'promisedAnswer (PromisedAnswer)
    deriving(Show, Read, Eq)

data Payload
    = Payload
        { content :: Maybe (Data.Capnp.Untyped.Pure.PtrType)
        , capTable :: Data.Capnp.Untyped.Pure.List (CapDescriptor)
        }
    deriving(Show, Read, Eq)

data Provide
    = Provide
        { questionId :: Word32
        , target :: MessageTarget
        , recipient :: Maybe (Data.Capnp.Untyped.Pure.PtrType)
        }
    deriving(Show, Read, Eq)

data Return
    = Return
        { answerId :: Word32
        , releaseParamCaps :: Bool
        , union' :: Return'
        }
    deriving(Show, Read, Eq)

data Return'
    = Return'results (Payload)
    | Return'exception (Exception)
    | Return'canceled
    | Return'resultsSentElsewhere
    | Return'takeFromOtherQuestion (Word32)
    | Return'acceptFromThirdParty (Maybe (Data.Capnp.Untyped.Pure.PtrType))
    deriving(Show, Read, Eq)

data Release
    = Release
        { id :: Word32
        , referenceCount :: Word32
        }
    deriving(Show, Read, Eq)

data Exception'Type
    = Exception'Type'failed
    | Exception'Type'overloaded
    | Exception'Type'disconnected
    | Exception'Type'unimplemented
    | Exception'Type'unknown' (Word16)
    deriving(Show, Read, Eq)

data Resolve
    = Resolve
        { promiseId :: Word32
        , union' :: Resolve'
        }
    deriving(Show, Read, Eq)

data Resolve'
    = Resolve'cap (CapDescriptor)
    | Resolve'exception (Exception)
    deriving(Show, Read, Eq)

data ThirdPartyCapDescriptor
    = ThirdPartyCapDescriptor
        { id :: Maybe (Data.Capnp.Untyped.Pure.PtrType)
        , vineId :: Word32
        }
    deriving(Show, Read, Eq)

data Finish
    = Finish
        { questionId :: Word32
        , releaseResultCaps :: Bool
        }
    deriving(Show, Read, Eq)

data Accept
    = Accept
        { questionId :: Word32
        , provision :: Maybe (Data.Capnp.Untyped.Pure.PtrType)
        , embargo :: Bool
        }
    deriving(Show, Read, Eq)

data Disembargo'context
    = Disembargo'context
        { union' :: Disembargo'context'
        }
    deriving(Show, Read, Eq)

data Disembargo'context'
    = Disembargo'context'senderLoopback (Word32)
    | Disembargo'context'receiverLoopback (Word32)
    | Disembargo'context'accept
    | Disembargo'context'provide (Word32)
    deriving(Show, Read, Eq)

data Exception
    = Exception
        { reason :: Data.Capnp.Untyped.Pure.Text
        , obsoleteIsCallersFault :: Bool
        , obsoleteDurability :: Word16
        , type_ :: Exception'Type
        }
    deriving(Show, Read, Eq)

data PromisedAnswer
    = PromisedAnswer
        { questionId :: Word32
        , transform :: Data.Capnp.Untyped.Pure.List (PromisedAnswer'Op)
        }
    deriving(Show, Read, Eq)

data Call'sendResultsTo
    = Call'sendResultsTo
        { union' :: Call'sendResultsTo'
        }
    deriving(Show, Read, Eq)

data Call'sendResultsTo'
    = Call'sendResultsTo'caller
    | Call'sendResultsTo'yourself
    | Call'sendResultsTo'thirdParty (Maybe (Data.Capnp.Untyped.Pure.PtrType))
    deriving(Show, Read, Eq)

data Bootstrap
    = Bootstrap
        { questionId :: Word32
        , deprecatedObjectId :: Maybe (Data.Capnp.Untyped.Pure.PtrType)
        }
    deriving(Show, Read, Eq)

data PromisedAnswer'Op
    = PromisedAnswer'Op
        { union' :: PromisedAnswer'Op'
        }
    deriving(Show, Read, Eq)

data PromisedAnswer'Op'
    = PromisedAnswer'Op'noop
    | PromisedAnswer'Op'getPointerField (Word16)
    deriving(Show, Read, Eq)

data Disembargo
    = Disembargo
        { target :: MessageTarget
        , context :: Disembargo'context
        }
    deriving(Show, Read, Eq)

data Join
    = Join
        { questionId :: Word32
        , target :: MessageTarget
        , keyPart :: Maybe (Data.Capnp.Untyped.Pure.PtrType)
        }
    deriving(Show, Read, Eq)

