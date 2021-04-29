{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
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
import qualified Capnp.New.Classes as C
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Message 
type instance (R.ReprFor Message) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Message) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Message) where
    type AllocHint Message = ()
    new  = GH.newStruct
instance (F.HasUnion Message) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ Message
        = Message'unimplemented (R.Raw mut_ Message)
        | Message'abort (R.Raw mut_ Exception)
        | Message'call (R.Raw mut_ Call)
        | Message'return (R.Raw mut_ Return)
        | Message'finish (R.Raw mut_ Finish)
        | Message'resolve (R.Raw mut_ Resolve)
        | Message'release (R.Raw mut_ Release)
        | Message'obsoleteSave (R.Raw mut_ Basics.AnyPointer)
        | Message'bootstrap (R.Raw mut_ Bootstrap)
        | Message'obsoleteDelete (R.Raw mut_ Basics.AnyPointer)
        | Message'provide (R.Raw mut_ Provide)
        | Message'accept (R.Raw mut_ Accept)
        | Message'join (R.Raw mut_ Join)
        | Message'disembargo (R.Raw mut_ Disembargo)
        | Message'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Message'unimplemented <$> (GH.readVariant #unimplemented struct_))
        1 ->
            (Message'abort <$> (GH.readVariant #abort struct_))
        2 ->
            (Message'call <$> (GH.readVariant #call struct_))
        3 ->
            (Message'return <$> (GH.readVariant #return struct_))
        4 ->
            (Message'finish <$> (GH.readVariant #finish struct_))
        5 ->
            (Message'resolve <$> (GH.readVariant #resolve struct_))
        6 ->
            (Message'release <$> (GH.readVariant #release struct_))
        7 ->
            (Message'obsoleteSave <$> (GH.readVariant #obsoleteSave struct_))
        8 ->
            (Message'bootstrap <$> (GH.readVariant #bootstrap struct_))
        9 ->
            (Message'obsoleteDelete <$> (GH.readVariant #obsoleteDelete struct_))
        10 ->
            (Message'provide <$> (GH.readVariant #provide struct_))
        11 ->
            (Message'accept <$> (GH.readVariant #accept struct_))
        12 ->
            (Message'join <$> (GH.readVariant #join struct_))
        13 ->
            (Message'disembargo <$> (GH.readVariant #disembargo struct_))
        _ ->
            (Std_.pure (Message'unknown' tag_))
instance (F.HasVariant "unimplemented" F.Slot Message Message) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 0)
instance (F.HasVariant "abort" F.Slot Message Exception) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 1)
instance (F.HasVariant "call" F.Slot Message Call) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 2)
instance (F.HasVariant "return" F.Slot Message Return) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 3)
instance (F.HasVariant "finish" F.Slot Message Finish) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 4)
instance (F.HasVariant "resolve" F.Slot Message Resolve) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 5)
instance (F.HasVariant "release" F.Slot Message Release) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 6)
instance (F.HasVariant "obsoleteSave" F.Slot Message Basics.AnyPointer) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 7)
instance (F.HasVariant "bootstrap" F.Slot Message Bootstrap) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 8)
instance (F.HasVariant "obsoleteDelete" F.Slot Message Basics.AnyPointer) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 9)
instance (F.HasVariant "provide" F.Slot Message Provide) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 10)
instance (F.HasVariant "accept" F.Slot Message Accept) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 11)
instance (F.HasVariant "join" F.Slot Message Join) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 12)
instance (F.HasVariant "disembargo" F.Slot Message Disembargo) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 13)
data Bootstrap 
type instance (R.ReprFor Bootstrap) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Bootstrap) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Bootstrap) where
    type AllocHint Bootstrap = ()
    new  = GH.newStruct
instance (F.HasField "questionId" F.Slot Bootstrap Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "deprecatedObjectId" F.Slot Bootstrap Basics.AnyPointer) where
    fieldByLabel  = (GH.ptrField 0)
data Call 
type instance (R.ReprFor Call) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Call) where
    numStructWords  = 3
    numStructPtrs  = 3
instance (C.Allocate Call) where
    type AllocHint Call = ()
    new  = GH.newStruct
instance (F.HasField "questionId" F.Slot Call Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "target" F.Slot Call MessageTarget) where
    fieldByLabel  = (GH.ptrField 0)
instance (F.HasField "interfaceId" F.Slot Call Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (F.HasField "methodId" F.Slot Call Std_.Word16) where
    fieldByLabel  = (GH.dataField 32 0 16 0)
instance (F.HasField "params" F.Slot Call Payload) where
    fieldByLabel  = (GH.ptrField 1)
instance (F.HasField "sendResultsTo" F.Group Call Call'sendResultsTo) where
    fieldByLabel  = GH.groupField
instance (F.HasField "allowThirdPartyTailCall" F.Slot Call Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 2 1 0)
data Call'sendResultsTo 
type instance (R.ReprFor Call'sendResultsTo) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Call'sendResultsTo) where
    numStructWords  = 3
    numStructPtrs  = 3
instance (C.Allocate Call'sendResultsTo) where
    type AllocHint Call'sendResultsTo = ()
    new  = GH.newStruct
instance (F.HasUnion Call'sendResultsTo) where
    unionField  = (GH.dataField 48 0 16 0)
    data RawWhich mut_ Call'sendResultsTo
        = Call'sendResultsTo'caller (R.Raw mut_ ())
        | Call'sendResultsTo'yourself (R.Raw mut_ ())
        | Call'sendResultsTo'thirdParty (R.Raw mut_ Basics.AnyPointer)
        | Call'sendResultsTo'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Call'sendResultsTo'caller <$> (GH.readVariant #caller struct_))
        1 ->
            (Call'sendResultsTo'yourself <$> (GH.readVariant #yourself struct_))
        2 ->
            (Call'sendResultsTo'thirdParty <$> (GH.readVariant #thirdParty struct_))
        _ ->
            (Std_.pure (Call'sendResultsTo'unknown' tag_))
instance (F.HasVariant "caller" F.Slot Call'sendResultsTo ()) where
    variantByLabel  = (F.Variant GH.voidField 0)
instance (F.HasVariant "yourself" F.Slot Call'sendResultsTo ()) where
    variantByLabel  = (F.Variant GH.voidField 1)
instance (F.HasVariant "thirdParty" F.Slot Call'sendResultsTo Basics.AnyPointer) where
    variantByLabel  = (F.Variant (GH.ptrField 2) 2)
data Return 
type instance (R.ReprFor Return) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Return) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate Return) where
    type AllocHint Return = ()
    new  = GH.newStruct
instance (F.HasUnion Return) where
    unionField  = (GH.dataField 48 0 16 0)
    data RawWhich mut_ Return
        = Return'results (R.Raw mut_ Payload)
        | Return'exception (R.Raw mut_ Exception)
        | Return'canceled (R.Raw mut_ ())
        | Return'resultsSentElsewhere (R.Raw mut_ ())
        | Return'takeFromOtherQuestion (R.Raw mut_ Std_.Word32)
        | Return'acceptFromThirdParty (R.Raw mut_ Basics.AnyPointer)
        | Return'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Return'results <$> (GH.readVariant #results struct_))
        1 ->
            (Return'exception <$> (GH.readVariant #exception struct_))
        2 ->
            (Return'canceled <$> (GH.readVariant #canceled struct_))
        3 ->
            (Return'resultsSentElsewhere <$> (GH.readVariant #resultsSentElsewhere struct_))
        4 ->
            (Return'takeFromOtherQuestion <$> (GH.readVariant #takeFromOtherQuestion struct_))
        5 ->
            (Return'acceptFromThirdParty <$> (GH.readVariant #acceptFromThirdParty struct_))
        _ ->
            (Std_.pure (Return'unknown' tag_))
instance (F.HasVariant "results" F.Slot Return Payload) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 0)
instance (F.HasVariant "exception" F.Slot Return Exception) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 1)
instance (F.HasVariant "canceled" F.Slot Return ()) where
    variantByLabel  = (F.Variant GH.voidField 2)
instance (F.HasVariant "resultsSentElsewhere" F.Slot Return ()) where
    variantByLabel  = (F.Variant GH.voidField 3)
instance (F.HasVariant "takeFromOtherQuestion" F.Slot Return Std_.Word32) where
    variantByLabel  = (F.Variant (GH.dataField 0 1 32 0) 4)
instance (F.HasVariant "acceptFromThirdParty" F.Slot Return Basics.AnyPointer) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 5)
instance (F.HasField "answerId" F.Slot Return Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "releaseParamCaps" F.Slot Return Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 0 1 1)
data Finish 
type instance (R.ReprFor Finish) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Finish) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Finish) where
    type AllocHint Finish = ()
    new  = GH.newStruct
instance (F.HasField "questionId" F.Slot Finish Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "releaseResultCaps" F.Slot Finish Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 0 1 1)
data Resolve 
type instance (R.ReprFor Resolve) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Resolve) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Resolve) where
    type AllocHint Resolve = ()
    new  = GH.newStruct
instance (F.HasUnion Resolve) where
    unionField  = (GH.dataField 32 0 16 0)
    data RawWhich mut_ Resolve
        = Resolve'cap (R.Raw mut_ CapDescriptor)
        | Resolve'exception (R.Raw mut_ Exception)
        | Resolve'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Resolve'cap <$> (GH.readVariant #cap struct_))
        1 ->
            (Resolve'exception <$> (GH.readVariant #exception struct_))
        _ ->
            (Std_.pure (Resolve'unknown' tag_))
instance (F.HasVariant "cap" F.Slot Resolve CapDescriptor) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 0)
instance (F.HasVariant "exception" F.Slot Resolve Exception) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 1)
instance (F.HasField "promiseId" F.Slot Resolve Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
data Release 
type instance (R.ReprFor Release) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Release) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Release) where
    type AllocHint Release = ()
    new  = GH.newStruct
instance (F.HasField "id" F.Slot Release Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "referenceCount" F.Slot Release Std_.Word32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
data Disembargo 
type instance (R.ReprFor Disembargo) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Disembargo) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Disembargo) where
    type AllocHint Disembargo = ()
    new  = GH.newStruct
instance (F.HasField "target" F.Slot Disembargo MessageTarget) where
    fieldByLabel  = (GH.ptrField 0)
instance (F.HasField "context" F.Group Disembargo Disembargo'context) where
    fieldByLabel  = GH.groupField
data Disembargo'context 
type instance (R.ReprFor Disembargo'context) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Disembargo'context) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Disembargo'context) where
    type AllocHint Disembargo'context = ()
    new  = GH.newStruct
instance (F.HasUnion Disembargo'context) where
    unionField  = (GH.dataField 32 0 16 0)
    data RawWhich mut_ Disembargo'context
        = Disembargo'context'senderLoopback (R.Raw mut_ Std_.Word32)
        | Disembargo'context'receiverLoopback (R.Raw mut_ Std_.Word32)
        | Disembargo'context'accept (R.Raw mut_ ())
        | Disembargo'context'provide (R.Raw mut_ Std_.Word32)
        | Disembargo'context'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Disembargo'context'senderLoopback <$> (GH.readVariant #senderLoopback struct_))
        1 ->
            (Disembargo'context'receiverLoopback <$> (GH.readVariant #receiverLoopback struct_))
        2 ->
            (Disembargo'context'accept <$> (GH.readVariant #accept struct_))
        3 ->
            (Disembargo'context'provide <$> (GH.readVariant #provide struct_))
        _ ->
            (Std_.pure (Disembargo'context'unknown' tag_))
instance (F.HasVariant "senderLoopback" F.Slot Disembargo'context Std_.Word32) where
    variantByLabel  = (F.Variant (GH.dataField 0 0 32 0) 0)
instance (F.HasVariant "receiverLoopback" F.Slot Disembargo'context Std_.Word32) where
    variantByLabel  = (F.Variant (GH.dataField 0 0 32 0) 1)
instance (F.HasVariant "accept" F.Slot Disembargo'context ()) where
    variantByLabel  = (F.Variant GH.voidField 2)
instance (F.HasVariant "provide" F.Slot Disembargo'context Std_.Word32) where
    variantByLabel  = (F.Variant (GH.dataField 0 0 32 0) 3)
data Provide 
type instance (R.ReprFor Provide) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Provide) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Provide) where
    type AllocHint Provide = ()
    new  = GH.newStruct
instance (F.HasField "questionId" F.Slot Provide Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "target" F.Slot Provide MessageTarget) where
    fieldByLabel  = (GH.ptrField 0)
instance (F.HasField "recipient" F.Slot Provide Basics.AnyPointer) where
    fieldByLabel  = (GH.ptrField 1)
data Accept 
type instance (R.ReprFor Accept) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Accept) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Accept) where
    type AllocHint Accept = ()
    new  = GH.newStruct
instance (F.HasField "questionId" F.Slot Accept Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "provision" F.Slot Accept Basics.AnyPointer) where
    fieldByLabel  = (GH.ptrField 0)
instance (F.HasField "embargo" F.Slot Accept Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 0 1 0)
data Join 
type instance (R.ReprFor Join) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Join) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Join) where
    type AllocHint Join = ()
    new  = GH.newStruct
instance (F.HasField "questionId" F.Slot Join Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "target" F.Slot Join MessageTarget) where
    fieldByLabel  = (GH.ptrField 0)
instance (F.HasField "keyPart" F.Slot Join Basics.AnyPointer) where
    fieldByLabel  = (GH.ptrField 1)
data MessageTarget 
type instance (R.ReprFor MessageTarget) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct MessageTarget) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate MessageTarget) where
    type AllocHint MessageTarget = ()
    new  = GH.newStruct
instance (F.HasUnion MessageTarget) where
    unionField  = (GH.dataField 32 0 16 0)
    data RawWhich mut_ MessageTarget
        = MessageTarget'importedCap (R.Raw mut_ Std_.Word32)
        | MessageTarget'promisedAnswer (R.Raw mut_ PromisedAnswer)
        | MessageTarget'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (MessageTarget'importedCap <$> (GH.readVariant #importedCap struct_))
        1 ->
            (MessageTarget'promisedAnswer <$> (GH.readVariant #promisedAnswer struct_))
        _ ->
            (Std_.pure (MessageTarget'unknown' tag_))
instance (F.HasVariant "importedCap" F.Slot MessageTarget Std_.Word32) where
    variantByLabel  = (F.Variant (GH.dataField 0 0 32 0) 0)
instance (F.HasVariant "promisedAnswer" F.Slot MessageTarget PromisedAnswer) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 1)
data Payload 
type instance (R.ReprFor Payload) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Payload) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Payload) where
    type AllocHint Payload = ()
    new  = GH.newStruct
instance (F.HasField "content" F.Slot Payload Basics.AnyPointer) where
    fieldByLabel  = (GH.ptrField 0)
instance (F.HasField "capTable" F.Slot Payload (R.List CapDescriptor)) where
    fieldByLabel  = (GH.ptrField 1)
data CapDescriptor 
type instance (R.ReprFor CapDescriptor) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct CapDescriptor) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate CapDescriptor) where
    type AllocHint CapDescriptor = ()
    new  = GH.newStruct
instance (F.HasUnion CapDescriptor) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ CapDescriptor
        = CapDescriptor'none (R.Raw mut_ ())
        | CapDescriptor'senderHosted (R.Raw mut_ Std_.Word32)
        | CapDescriptor'senderPromise (R.Raw mut_ Std_.Word32)
        | CapDescriptor'receiverHosted (R.Raw mut_ Std_.Word32)
        | CapDescriptor'receiverAnswer (R.Raw mut_ PromisedAnswer)
        | CapDescriptor'thirdPartyHosted (R.Raw mut_ ThirdPartyCapDescriptor)
        | CapDescriptor'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (CapDescriptor'none <$> (GH.readVariant #none struct_))
        1 ->
            (CapDescriptor'senderHosted <$> (GH.readVariant #senderHosted struct_))
        2 ->
            (CapDescriptor'senderPromise <$> (GH.readVariant #senderPromise struct_))
        3 ->
            (CapDescriptor'receiverHosted <$> (GH.readVariant #receiverHosted struct_))
        4 ->
            (CapDescriptor'receiverAnswer <$> (GH.readVariant #receiverAnswer struct_))
        5 ->
            (CapDescriptor'thirdPartyHosted <$> (GH.readVariant #thirdPartyHosted struct_))
        _ ->
            (Std_.pure (CapDescriptor'unknown' tag_))
instance (F.HasVariant "none" F.Slot CapDescriptor ()) where
    variantByLabel  = (F.Variant GH.voidField 0)
instance (F.HasVariant "senderHosted" F.Slot CapDescriptor Std_.Word32) where
    variantByLabel  = (F.Variant (GH.dataField 32 0 32 0) 1)
instance (F.HasVariant "senderPromise" F.Slot CapDescriptor Std_.Word32) where
    variantByLabel  = (F.Variant (GH.dataField 32 0 32 0) 2)
instance (F.HasVariant "receiverHosted" F.Slot CapDescriptor Std_.Word32) where
    variantByLabel  = (F.Variant (GH.dataField 32 0 32 0) 3)
instance (F.HasVariant "receiverAnswer" F.Slot CapDescriptor PromisedAnswer) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 4)
instance (F.HasVariant "thirdPartyHosted" F.Slot CapDescriptor ThirdPartyCapDescriptor) where
    variantByLabel  = (F.Variant (GH.ptrField 0) 5)
instance (F.HasField "attachedFd" F.Slot CapDescriptor Std_.Word8) where
    fieldByLabel  = (GH.dataField 16 0 8 255)
data PromisedAnswer 
type instance (R.ReprFor PromisedAnswer) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct PromisedAnswer) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate PromisedAnswer) where
    type AllocHint PromisedAnswer = ()
    new  = GH.newStruct
instance (F.HasField "questionId" F.Slot PromisedAnswer Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "transform" F.Slot PromisedAnswer (R.List PromisedAnswer'Op)) where
    fieldByLabel  = (GH.ptrField 0)
data PromisedAnswer'Op 
type instance (R.ReprFor PromisedAnswer'Op) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct PromisedAnswer'Op) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate PromisedAnswer'Op) where
    type AllocHint PromisedAnswer'Op = ()
    new  = GH.newStruct
instance (F.HasUnion PromisedAnswer'Op) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ PromisedAnswer'Op
        = PromisedAnswer'Op'noop (R.Raw mut_ ())
        | PromisedAnswer'Op'getPointerField (R.Raw mut_ Std_.Word16)
        | PromisedAnswer'Op'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (PromisedAnswer'Op'noop <$> (GH.readVariant #noop struct_))
        1 ->
            (PromisedAnswer'Op'getPointerField <$> (GH.readVariant #getPointerField struct_))
        _ ->
            (Std_.pure (PromisedAnswer'Op'unknown' tag_))
instance (F.HasVariant "noop" F.Slot PromisedAnswer'Op ()) where
    variantByLabel  = (F.Variant GH.voidField 0)
instance (F.HasVariant "getPointerField" F.Slot PromisedAnswer'Op Std_.Word16) where
    variantByLabel  = (F.Variant (GH.dataField 16 0 16 0) 1)
data ThirdPartyCapDescriptor 
type instance (R.ReprFor ThirdPartyCapDescriptor) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct ThirdPartyCapDescriptor) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate ThirdPartyCapDescriptor) where
    type AllocHint ThirdPartyCapDescriptor = ()
    new  = GH.newStruct
instance (F.HasField "id" F.Slot ThirdPartyCapDescriptor Basics.AnyPointer) where
    fieldByLabel  = (GH.ptrField 0)
instance (F.HasField "vineId" F.Slot ThirdPartyCapDescriptor Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
data Exception 
type instance (R.ReprFor Exception) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Exception) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Exception) where
    type AllocHint Exception = ()
    new  = GH.newStruct
instance (F.HasField "reason" F.Slot Exception Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (F.HasField "obsoleteIsCallersFault" F.Slot Exception Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (F.HasField "obsoleteDurability" F.Slot Exception Std_.Word16) where
    fieldByLabel  = (GH.dataField 16 0 16 0)
instance (F.HasField "type_" F.Slot Exception Exception'Type) where
    fieldByLabel  = (GH.dataField 32 0 16 0)
data Exception'Type 
type instance (R.ReprFor Exception'Type) = (R.Data R.Sz16)