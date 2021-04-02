{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.Rpc.New where
import qualified Capnp.Repr as R
import qualified Capnp.Fields as F
import qualified Capnp.New.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers.New as GH
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Message 
type instance (R.ReprFor Message) = (R.Ptr (Std_.Just R.Struct))
instance (F.HasUnion (Message)) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ (Message)
instance (OL.IsLabel "unimplemented" (F.Variant F.Slot (Message) Message)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 0)
instance (F.HasVariant "unimplemented" F.Slot (Message) Message)
instance (OL.IsLabel "abort" (F.Variant F.Slot (Message) Exception)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 1)
instance (F.HasVariant "abort" F.Slot (Message) Exception)
instance (OL.IsLabel "call" (F.Variant F.Slot (Message) Call)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 2)
instance (F.HasVariant "call" F.Slot (Message) Call)
instance (OL.IsLabel "return" (F.Variant F.Slot (Message) Return)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 3)
instance (F.HasVariant "return" F.Slot (Message) Return)
instance (OL.IsLabel "finish" (F.Variant F.Slot (Message) Finish)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 4)
instance (F.HasVariant "finish" F.Slot (Message) Finish)
instance (OL.IsLabel "resolve" (F.Variant F.Slot (Message) Resolve)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 5)
instance (F.HasVariant "resolve" F.Slot (Message) Resolve)
instance (OL.IsLabel "release" (F.Variant F.Slot (Message) Release)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 6)
instance (F.HasVariant "release" F.Slot (Message) Release)
instance (OL.IsLabel "obsoleteSave" (F.Variant F.Slot (Message) Basics.AnyPointer)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 7)
instance (F.HasVariant "obsoleteSave" F.Slot (Message) Basics.AnyPointer)
instance (OL.IsLabel "bootstrap" (F.Variant F.Slot (Message) Bootstrap)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 8)
instance (F.HasVariant "bootstrap" F.Slot (Message) Bootstrap)
instance (OL.IsLabel "obsoleteDelete" (F.Variant F.Slot (Message) Basics.AnyPointer)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 9)
instance (F.HasVariant "obsoleteDelete" F.Slot (Message) Basics.AnyPointer)
instance (OL.IsLabel "provide" (F.Variant F.Slot (Message) Provide)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 10)
instance (F.HasVariant "provide" F.Slot (Message) Provide)
instance (OL.IsLabel "accept" (F.Variant F.Slot (Message) Accept)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 11)
instance (F.HasVariant "accept" F.Slot (Message) Accept)
instance (OL.IsLabel "join" (F.Variant F.Slot (Message) Join)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 12)
instance (F.HasVariant "join" F.Slot (Message) Join)
instance (OL.IsLabel "disembargo" (F.Variant F.Slot (Message) Disembargo)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 13)
instance (F.HasVariant "disembargo" F.Slot (Message) Disembargo)
data Bootstrap 
type instance (R.ReprFor Bootstrap) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "questionId" (F.Field F.Slot (Bootstrap) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" F.Slot (Bootstrap) Std_.Word32)
instance (OL.IsLabel "deprecatedObjectId" (F.Field F.Slot (Bootstrap) Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "deprecatedObjectId" F.Slot (Bootstrap) Basics.AnyPointer)
data Call 
type instance (R.ReprFor Call) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "questionId" (F.Field F.Slot (Call) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" F.Slot (Call) Std_.Word32)
instance (OL.IsLabel "target" (F.Field F.Slot (Call) MessageTarget)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "target" F.Slot (Call) MessageTarget)
instance (OL.IsLabel "interfaceId" (F.Field F.Slot (Call) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 1 64 0)
instance (F.HasField "interfaceId" F.Slot (Call) Std_.Word64)
instance (OL.IsLabel "methodId" (F.Field F.Slot (Call) Std_.Word16)) where
    fromLabel  = (GH.dataField 32 0 16 0)
instance (F.HasField "methodId" F.Slot (Call) Std_.Word16)
instance (OL.IsLabel "params" (F.Field F.Slot (Call) Payload)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "params" F.Slot (Call) Payload)
instance (OL.IsLabel "sendResultsTo" (F.Field F.Group (Call) Call'sendResultsTo)) where
    fromLabel  = GH.groupField
instance (F.HasField "sendResultsTo" F.Group (Call) Call'sendResultsTo)
instance (OL.IsLabel "allowThirdPartyTailCall" (F.Field F.Slot (Call) Std_.Bool)) where
    fromLabel  = (GH.dataField 0 2 1 0)
instance (F.HasField "allowThirdPartyTailCall" F.Slot (Call) Std_.Bool)
data Call'sendResultsTo 
type instance (R.ReprFor Call'sendResultsTo) = (R.Ptr (Std_.Just R.Struct))
instance (F.HasUnion (Call'sendResultsTo)) where
    unionField  = (GH.dataField 3 0 16 0)
    data RawWhich mut_ (Call'sendResultsTo)
instance (OL.IsLabel "caller" (F.Variant F.Slot (Call'sendResultsTo) ())) where
    fromLabel  = (F.Variant GH.voidField 0)
instance (F.HasVariant "caller" F.Slot (Call'sendResultsTo) ())
instance (OL.IsLabel "yourself" (F.Variant F.Slot (Call'sendResultsTo) ())) where
    fromLabel  = (F.Variant GH.voidField 1)
instance (F.HasVariant "yourself" F.Slot (Call'sendResultsTo) ())
instance (OL.IsLabel "thirdParty" (F.Variant F.Slot (Call'sendResultsTo) Basics.AnyPointer)) where
    fromLabel  = (F.Variant (GH.ptrField 2) 2)
instance (F.HasVariant "thirdParty" F.Slot (Call'sendResultsTo) Basics.AnyPointer)
data Return 
type instance (R.ReprFor Return) = (R.Ptr (Std_.Just R.Struct))
instance (F.HasUnion (Return)) where
    unionField  = (GH.dataField 3 0 16 0)
    data RawWhich mut_ (Return)
instance (OL.IsLabel "results" (F.Variant F.Slot (Return) Payload)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 0)
instance (F.HasVariant "results" F.Slot (Return) Payload)
instance (OL.IsLabel "exception" (F.Variant F.Slot (Return) Exception)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 1)
instance (F.HasVariant "exception" F.Slot (Return) Exception)
instance (OL.IsLabel "canceled" (F.Variant F.Slot (Return) ())) where
    fromLabel  = (F.Variant GH.voidField 2)
instance (F.HasVariant "canceled" F.Slot (Return) ())
instance (OL.IsLabel "resultsSentElsewhere" (F.Variant F.Slot (Return) ())) where
    fromLabel  = (F.Variant GH.voidField 3)
instance (F.HasVariant "resultsSentElsewhere" F.Slot (Return) ())
instance (OL.IsLabel "takeFromOtherQuestion" (F.Variant F.Slot (Return) Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 0 1 32 0) 4)
instance (F.HasVariant "takeFromOtherQuestion" F.Slot (Return) Std_.Word32)
instance (OL.IsLabel "acceptFromThirdParty" (F.Variant F.Slot (Return) Basics.AnyPointer)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 5)
instance (F.HasVariant "acceptFromThirdParty" F.Slot (Return) Basics.AnyPointer)
instance (OL.IsLabel "answerId" (F.Field F.Slot (Return) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "answerId" F.Slot (Return) Std_.Word32)
instance (OL.IsLabel "releaseParamCaps" (F.Field F.Slot (Return) Std_.Bool)) where
    fromLabel  = (GH.dataField 32 0 1 1)
instance (F.HasField "releaseParamCaps" F.Slot (Return) Std_.Bool)
data Finish 
type instance (R.ReprFor Finish) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "questionId" (F.Field F.Slot (Finish) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" F.Slot (Finish) Std_.Word32)
instance (OL.IsLabel "releaseResultCaps" (F.Field F.Slot (Finish) Std_.Bool)) where
    fromLabel  = (GH.dataField 32 0 1 1)
instance (F.HasField "releaseResultCaps" F.Slot (Finish) Std_.Bool)
data Resolve 
type instance (R.ReprFor Resolve) = (R.Ptr (Std_.Just R.Struct))
instance (F.HasUnion (Resolve)) where
    unionField  = (GH.dataField 2 0 16 0)
    data RawWhich mut_ (Resolve)
instance (OL.IsLabel "cap" (F.Variant F.Slot (Resolve) CapDescriptor)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 0)
instance (F.HasVariant "cap" F.Slot (Resolve) CapDescriptor)
instance (OL.IsLabel "exception" (F.Variant F.Slot (Resolve) Exception)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 1)
instance (F.HasVariant "exception" F.Slot (Resolve) Exception)
instance (OL.IsLabel "promiseId" (F.Field F.Slot (Resolve) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "promiseId" F.Slot (Resolve) Std_.Word32)
data Release 
type instance (R.ReprFor Release) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field F.Slot (Release) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "id" F.Slot (Release) Std_.Word32)
instance (OL.IsLabel "referenceCount" (F.Field F.Slot (Release) Std_.Word32)) where
    fromLabel  = (GH.dataField 32 0 32 0)
instance (F.HasField "referenceCount" F.Slot (Release) Std_.Word32)
data Disembargo 
type instance (R.ReprFor Disembargo) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "target" (F.Field F.Slot (Disembargo) MessageTarget)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "target" F.Slot (Disembargo) MessageTarget)
instance (OL.IsLabel "context" (F.Field F.Group (Disembargo) Disembargo'context)) where
    fromLabel  = GH.groupField
instance (F.HasField "context" F.Group (Disembargo) Disembargo'context)
data Disembargo'context 
type instance (R.ReprFor Disembargo'context) = (R.Ptr (Std_.Just R.Struct))
instance (F.HasUnion (Disembargo'context)) where
    unionField  = (GH.dataField 2 0 16 0)
    data RawWhich mut_ (Disembargo'context)
instance (OL.IsLabel "senderLoopback" (F.Variant F.Slot (Disembargo'context) Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 0 0 32 0) 0)
instance (F.HasVariant "senderLoopback" F.Slot (Disembargo'context) Std_.Word32)
instance (OL.IsLabel "receiverLoopback" (F.Variant F.Slot (Disembargo'context) Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 0 0 32 0) 1)
instance (F.HasVariant "receiverLoopback" F.Slot (Disembargo'context) Std_.Word32)
instance (OL.IsLabel "accept" (F.Variant F.Slot (Disembargo'context) ())) where
    fromLabel  = (F.Variant GH.voidField 2)
instance (F.HasVariant "accept" F.Slot (Disembargo'context) ())
instance (OL.IsLabel "provide" (F.Variant F.Slot (Disembargo'context) Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 0 0 32 0) 3)
instance (F.HasVariant "provide" F.Slot (Disembargo'context) Std_.Word32)
data Provide 
type instance (R.ReprFor Provide) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "questionId" (F.Field F.Slot (Provide) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" F.Slot (Provide) Std_.Word32)
instance (OL.IsLabel "target" (F.Field F.Slot (Provide) MessageTarget)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "target" F.Slot (Provide) MessageTarget)
instance (OL.IsLabel "recipient" (F.Field F.Slot (Provide) Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "recipient" F.Slot (Provide) Basics.AnyPointer)
data Accept 
type instance (R.ReprFor Accept) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "questionId" (F.Field F.Slot (Accept) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" F.Slot (Accept) Std_.Word32)
instance (OL.IsLabel "provision" (F.Field F.Slot (Accept) Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "provision" F.Slot (Accept) Basics.AnyPointer)
instance (OL.IsLabel "embargo" (F.Field F.Slot (Accept) Std_.Bool)) where
    fromLabel  = (GH.dataField 32 0 1 0)
instance (F.HasField "embargo" F.Slot (Accept) Std_.Bool)
data Join 
type instance (R.ReprFor Join) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "questionId" (F.Field F.Slot (Join) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" F.Slot (Join) Std_.Word32)
instance (OL.IsLabel "target" (F.Field F.Slot (Join) MessageTarget)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "target" F.Slot (Join) MessageTarget)
instance (OL.IsLabel "keyPart" (F.Field F.Slot (Join) Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "keyPart" F.Slot (Join) Basics.AnyPointer)
data MessageTarget 
type instance (R.ReprFor MessageTarget) = (R.Ptr (Std_.Just R.Struct))
instance (F.HasUnion (MessageTarget)) where
    unionField  = (GH.dataField 2 0 16 0)
    data RawWhich mut_ (MessageTarget)
instance (OL.IsLabel "importedCap" (F.Variant F.Slot (MessageTarget) Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 0 0 32 0) 0)
instance (F.HasVariant "importedCap" F.Slot (MessageTarget) Std_.Word32)
instance (OL.IsLabel "promisedAnswer" (F.Variant F.Slot (MessageTarget) PromisedAnswer)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 1)
instance (F.HasVariant "promisedAnswer" F.Slot (MessageTarget) PromisedAnswer)
data Payload 
type instance (R.ReprFor Payload) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "content" (F.Field F.Slot (Payload) Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "content" F.Slot (Payload) Basics.AnyPointer)
instance (OL.IsLabel "capTable" (F.Field F.Slot (Payload) (R.List CapDescriptor))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "capTable" F.Slot (Payload) (R.List CapDescriptor))
data CapDescriptor 
type instance (R.ReprFor CapDescriptor) = (R.Ptr (Std_.Just R.Struct))
instance (F.HasUnion (CapDescriptor)) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ (CapDescriptor)
instance (OL.IsLabel "none" (F.Variant F.Slot (CapDescriptor) ())) where
    fromLabel  = (F.Variant GH.voidField 0)
instance (F.HasVariant "none" F.Slot (CapDescriptor) ())
instance (OL.IsLabel "senderHosted" (F.Variant F.Slot (CapDescriptor) Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 32 0 32 0) 1)
instance (F.HasVariant "senderHosted" F.Slot (CapDescriptor) Std_.Word32)
instance (OL.IsLabel "senderPromise" (F.Variant F.Slot (CapDescriptor) Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 32 0 32 0) 2)
instance (F.HasVariant "senderPromise" F.Slot (CapDescriptor) Std_.Word32)
instance (OL.IsLabel "receiverHosted" (F.Variant F.Slot (CapDescriptor) Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 32 0 32 0) 3)
instance (F.HasVariant "receiverHosted" F.Slot (CapDescriptor) Std_.Word32)
instance (OL.IsLabel "receiverAnswer" (F.Variant F.Slot (CapDescriptor) PromisedAnswer)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 4)
instance (F.HasVariant "receiverAnswer" F.Slot (CapDescriptor) PromisedAnswer)
instance (OL.IsLabel "thirdPartyHosted" (F.Variant F.Slot (CapDescriptor) ThirdPartyCapDescriptor)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 5)
instance (F.HasVariant "thirdPartyHosted" F.Slot (CapDescriptor) ThirdPartyCapDescriptor)
instance (OL.IsLabel "attachedFd" (F.Field F.Slot (CapDescriptor) Std_.Word8)) where
    fromLabel  = (GH.dataField 16 0 8 255)
instance (F.HasField "attachedFd" F.Slot (CapDescriptor) Std_.Word8)
data PromisedAnswer 
type instance (R.ReprFor PromisedAnswer) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "questionId" (F.Field F.Slot (PromisedAnswer) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" F.Slot (PromisedAnswer) Std_.Word32)
instance (OL.IsLabel "transform" (F.Field F.Slot (PromisedAnswer) (R.List PromisedAnswer'Op))) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "transform" F.Slot (PromisedAnswer) (R.List PromisedAnswer'Op))
data PromisedAnswer'Op 
type instance (R.ReprFor PromisedAnswer'Op) = (R.Ptr (Std_.Just R.Struct))
instance (F.HasUnion (PromisedAnswer'Op)) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ (PromisedAnswer'Op)
instance (OL.IsLabel "noop" (F.Variant F.Slot (PromisedAnswer'Op) ())) where
    fromLabel  = (F.Variant GH.voidField 0)
instance (F.HasVariant "noop" F.Slot (PromisedAnswer'Op) ())
instance (OL.IsLabel "getPointerField" (F.Variant F.Slot (PromisedAnswer'Op) Std_.Word16)) where
    fromLabel  = (F.Variant (GH.dataField 16 0 16 0) 1)
instance (F.HasVariant "getPointerField" F.Slot (PromisedAnswer'Op) Std_.Word16)
data ThirdPartyCapDescriptor 
type instance (R.ReprFor ThirdPartyCapDescriptor) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field F.Slot (ThirdPartyCapDescriptor) Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "id" F.Slot (ThirdPartyCapDescriptor) Basics.AnyPointer)
instance (OL.IsLabel "vineId" (F.Field F.Slot (ThirdPartyCapDescriptor) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "vineId" F.Slot (ThirdPartyCapDescriptor) Std_.Word32)
data Exception 
type instance (R.ReprFor Exception) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "reason" (F.Field F.Slot (Exception) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "reason" F.Slot (Exception) Basics.Text)
instance (OL.IsLabel "obsoleteIsCallersFault" (F.Field F.Slot (Exception) Std_.Bool)) where
    fromLabel  = (GH.dataField 0 0 1 0)
instance (F.HasField "obsoleteIsCallersFault" F.Slot (Exception) Std_.Bool)
instance (OL.IsLabel "obsoleteDurability" (F.Field F.Slot (Exception) Std_.Word16)) where
    fromLabel  = (GH.dataField 16 0 16 0)
instance (F.HasField "obsoleteDurability" F.Slot (Exception) Std_.Word16)
instance (OL.IsLabel "type_" (F.Field F.Slot (Exception) Exception'Type)) where
    fromLabel  = (GH.dataField 32 0 16 0)
instance (F.HasField "type_" F.Slot (Exception) Exception'Type)
data Exception'Type 
type instance (R.ReprFor Exception'Type) = (R.Data R.Sz16)