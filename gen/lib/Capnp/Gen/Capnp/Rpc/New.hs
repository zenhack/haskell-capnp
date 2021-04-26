{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
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
            (Message'unimplemented <$> (GH.readVariant (#unimplemented :: (F.Variant F.Slot _ _)) struct_))
        1 ->
            (Message'abort <$> (GH.readVariant (#abort :: (F.Variant F.Slot _ _)) struct_))
        2 ->
            (Message'call <$> (GH.readVariant (#call :: (F.Variant F.Slot _ _)) struct_))
        3 ->
            (Message'return <$> (GH.readVariant (#return :: (F.Variant F.Slot _ _)) struct_))
        4 ->
            (Message'finish <$> (GH.readVariant (#finish :: (F.Variant F.Slot _ _)) struct_))
        5 ->
            (Message'resolve <$> (GH.readVariant (#resolve :: (F.Variant F.Slot _ _)) struct_))
        6 ->
            (Message'release <$> (GH.readVariant (#release :: (F.Variant F.Slot _ _)) struct_))
        7 ->
            (Message'obsoleteSave <$> (GH.readVariant (#obsoleteSave :: (F.Variant F.Slot _ _)) struct_))
        8 ->
            (Message'bootstrap <$> (GH.readVariant (#bootstrap :: (F.Variant F.Slot _ _)) struct_))
        9 ->
            (Message'obsoleteDelete <$> (GH.readVariant (#obsoleteDelete :: (F.Variant F.Slot _ _)) struct_))
        10 ->
            (Message'provide <$> (GH.readVariant (#provide :: (F.Variant F.Slot _ _)) struct_))
        11 ->
            (Message'accept <$> (GH.readVariant (#accept :: (F.Variant F.Slot _ _)) struct_))
        12 ->
            (Message'join <$> (GH.readVariant (#join :: (F.Variant F.Slot _ _)) struct_))
        13 ->
            (Message'disembargo <$> (GH.readVariant (#disembargo :: (F.Variant F.Slot _ _)) struct_))
        _ ->
            (Std_.pure (Message'unknown' tag_))
instance (OL.IsLabel "unimplemented" (F.Variant F.Slot Message Message)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 0)
instance (F.HasVariant "unimplemented" F.Slot Message Message)
instance (OL.IsLabel "abort" (F.Variant F.Slot Message Exception)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 1)
instance (F.HasVariant "abort" F.Slot Message Exception)
instance (OL.IsLabel "call" (F.Variant F.Slot Message Call)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 2)
instance (F.HasVariant "call" F.Slot Message Call)
instance (OL.IsLabel "return" (F.Variant F.Slot Message Return)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 3)
instance (F.HasVariant "return" F.Slot Message Return)
instance (OL.IsLabel "finish" (F.Variant F.Slot Message Finish)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 4)
instance (F.HasVariant "finish" F.Slot Message Finish)
instance (OL.IsLabel "resolve" (F.Variant F.Slot Message Resolve)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 5)
instance (F.HasVariant "resolve" F.Slot Message Resolve)
instance (OL.IsLabel "release" (F.Variant F.Slot Message Release)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 6)
instance (F.HasVariant "release" F.Slot Message Release)
instance (OL.IsLabel "obsoleteSave" (F.Variant F.Slot Message Basics.AnyPointer)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 7)
instance (F.HasVariant "obsoleteSave" F.Slot Message Basics.AnyPointer)
instance (OL.IsLabel "bootstrap" (F.Variant F.Slot Message Bootstrap)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 8)
instance (F.HasVariant "bootstrap" F.Slot Message Bootstrap)
instance (OL.IsLabel "obsoleteDelete" (F.Variant F.Slot Message Basics.AnyPointer)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 9)
instance (F.HasVariant "obsoleteDelete" F.Slot Message Basics.AnyPointer)
instance (OL.IsLabel "provide" (F.Variant F.Slot Message Provide)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 10)
instance (F.HasVariant "provide" F.Slot Message Provide)
instance (OL.IsLabel "accept" (F.Variant F.Slot Message Accept)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 11)
instance (F.HasVariant "accept" F.Slot Message Accept)
instance (OL.IsLabel "join" (F.Variant F.Slot Message Join)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 12)
instance (F.HasVariant "join" F.Slot Message Join)
instance (OL.IsLabel "disembargo" (F.Variant F.Slot Message Disembargo)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 13)
instance (F.HasVariant "disembargo" F.Slot Message Disembargo)
data Bootstrap 
type instance (R.ReprFor Bootstrap) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Bootstrap) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Bootstrap) where
    type AllocHint Bootstrap = ()
    new  = GH.newStruct
instance (OL.IsLabel "questionId" (F.Field F.Slot Bootstrap Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" F.Slot Bootstrap Std_.Word32)
instance (OL.IsLabel "deprecatedObjectId" (F.Field F.Slot Bootstrap Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "deprecatedObjectId" F.Slot Bootstrap Basics.AnyPointer)
data Call 
type instance (R.ReprFor Call) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Call) where
    numStructWords  = 3
    numStructPtrs  = 3
instance (C.Allocate Call) where
    type AllocHint Call = ()
    new  = GH.newStruct
instance (OL.IsLabel "questionId" (F.Field F.Slot Call Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" F.Slot Call Std_.Word32)
instance (OL.IsLabel "target" (F.Field F.Slot Call MessageTarget)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "target" F.Slot Call MessageTarget)
instance (OL.IsLabel "interfaceId" (F.Field F.Slot Call Std_.Word64)) where
    fromLabel  = (GH.dataField 0 1 64 0)
instance (F.HasField "interfaceId" F.Slot Call Std_.Word64)
instance (OL.IsLabel "methodId" (F.Field F.Slot Call Std_.Word16)) where
    fromLabel  = (GH.dataField 32 0 16 0)
instance (F.HasField "methodId" F.Slot Call Std_.Word16)
instance (OL.IsLabel "params" (F.Field F.Slot Call Payload)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "params" F.Slot Call Payload)
instance (OL.IsLabel "sendResultsTo" (F.Field F.Group Call Call'sendResultsTo)) where
    fromLabel  = GH.groupField
instance (F.HasField "sendResultsTo" F.Group Call Call'sendResultsTo)
instance (OL.IsLabel "allowThirdPartyTailCall" (F.Field F.Slot Call Std_.Bool)) where
    fromLabel  = (GH.dataField 0 2 1 0)
instance (F.HasField "allowThirdPartyTailCall" F.Slot Call Std_.Bool)
data Call'sendResultsTo 
type instance (R.ReprFor Call'sendResultsTo) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Call'sendResultsTo) where
    numStructWords  = 3
    numStructPtrs  = 3
instance (C.Allocate Call'sendResultsTo) where
    type AllocHint Call'sendResultsTo = ()
    new  = GH.newStruct
instance (F.HasUnion Call'sendResultsTo) where
    unionField  = (GH.dataField 3 0 16 0)
    data RawWhich mut_ Call'sendResultsTo
        = Call'sendResultsTo'caller (R.Raw mut_ ())
        | Call'sendResultsTo'yourself (R.Raw mut_ ())
        | Call'sendResultsTo'thirdParty (R.Raw mut_ Basics.AnyPointer)
        | Call'sendResultsTo'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Call'sendResultsTo'caller <$> (GH.readVariant (#caller :: (F.Variant F.Slot _ _)) struct_))
        1 ->
            (Call'sendResultsTo'yourself <$> (GH.readVariant (#yourself :: (F.Variant F.Slot _ _)) struct_))
        2 ->
            (Call'sendResultsTo'thirdParty <$> (GH.readVariant (#thirdParty :: (F.Variant F.Slot _ _)) struct_))
        _ ->
            (Std_.pure (Call'sendResultsTo'unknown' tag_))
instance (OL.IsLabel "caller" (F.Variant F.Slot Call'sendResultsTo ())) where
    fromLabel  = (F.Variant GH.voidField 0)
instance (F.HasVariant "caller" F.Slot Call'sendResultsTo ())
instance (OL.IsLabel "yourself" (F.Variant F.Slot Call'sendResultsTo ())) where
    fromLabel  = (F.Variant GH.voidField 1)
instance (F.HasVariant "yourself" F.Slot Call'sendResultsTo ())
instance (OL.IsLabel "thirdParty" (F.Variant F.Slot Call'sendResultsTo Basics.AnyPointer)) where
    fromLabel  = (F.Variant (GH.ptrField 2) 2)
instance (F.HasVariant "thirdParty" F.Slot Call'sendResultsTo Basics.AnyPointer)
data Return 
type instance (R.ReprFor Return) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Return) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate Return) where
    type AllocHint Return = ()
    new  = GH.newStruct
instance (F.HasUnion Return) where
    unionField  = (GH.dataField 3 0 16 0)
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
            (Return'results <$> (GH.readVariant (#results :: (F.Variant F.Slot _ _)) struct_))
        1 ->
            (Return'exception <$> (GH.readVariant (#exception :: (F.Variant F.Slot _ _)) struct_))
        2 ->
            (Return'canceled <$> (GH.readVariant (#canceled :: (F.Variant F.Slot _ _)) struct_))
        3 ->
            (Return'resultsSentElsewhere <$> (GH.readVariant (#resultsSentElsewhere :: (F.Variant F.Slot _ _)) struct_))
        4 ->
            (Return'takeFromOtherQuestion <$> (GH.readVariant (#takeFromOtherQuestion :: (F.Variant F.Slot _ _)) struct_))
        5 ->
            (Return'acceptFromThirdParty <$> (GH.readVariant (#acceptFromThirdParty :: (F.Variant F.Slot _ _)) struct_))
        _ ->
            (Std_.pure (Return'unknown' tag_))
instance (OL.IsLabel "results" (F.Variant F.Slot Return Payload)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 0)
instance (F.HasVariant "results" F.Slot Return Payload)
instance (OL.IsLabel "exception" (F.Variant F.Slot Return Exception)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 1)
instance (F.HasVariant "exception" F.Slot Return Exception)
instance (OL.IsLabel "canceled" (F.Variant F.Slot Return ())) where
    fromLabel  = (F.Variant GH.voidField 2)
instance (F.HasVariant "canceled" F.Slot Return ())
instance (OL.IsLabel "resultsSentElsewhere" (F.Variant F.Slot Return ())) where
    fromLabel  = (F.Variant GH.voidField 3)
instance (F.HasVariant "resultsSentElsewhere" F.Slot Return ())
instance (OL.IsLabel "takeFromOtherQuestion" (F.Variant F.Slot Return Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 0 1 32 0) 4)
instance (F.HasVariant "takeFromOtherQuestion" F.Slot Return Std_.Word32)
instance (OL.IsLabel "acceptFromThirdParty" (F.Variant F.Slot Return Basics.AnyPointer)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 5)
instance (F.HasVariant "acceptFromThirdParty" F.Slot Return Basics.AnyPointer)
instance (OL.IsLabel "answerId" (F.Field F.Slot Return Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "answerId" F.Slot Return Std_.Word32)
instance (OL.IsLabel "releaseParamCaps" (F.Field F.Slot Return Std_.Bool)) where
    fromLabel  = (GH.dataField 32 0 1 1)
instance (F.HasField "releaseParamCaps" F.Slot Return Std_.Bool)
data Finish 
type instance (R.ReprFor Finish) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Finish) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Finish) where
    type AllocHint Finish = ()
    new  = GH.newStruct
instance (OL.IsLabel "questionId" (F.Field F.Slot Finish Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" F.Slot Finish Std_.Word32)
instance (OL.IsLabel "releaseResultCaps" (F.Field F.Slot Finish Std_.Bool)) where
    fromLabel  = (GH.dataField 32 0 1 1)
instance (F.HasField "releaseResultCaps" F.Slot Finish Std_.Bool)
data Resolve 
type instance (R.ReprFor Resolve) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Resolve) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Resolve) where
    type AllocHint Resolve = ()
    new  = GH.newStruct
instance (F.HasUnion Resolve) where
    unionField  = (GH.dataField 2 0 16 0)
    data RawWhich mut_ Resolve
        = Resolve'cap (R.Raw mut_ CapDescriptor)
        | Resolve'exception (R.Raw mut_ Exception)
        | Resolve'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Resolve'cap <$> (GH.readVariant (#cap :: (F.Variant F.Slot _ _)) struct_))
        1 ->
            (Resolve'exception <$> (GH.readVariant (#exception :: (F.Variant F.Slot _ _)) struct_))
        _ ->
            (Std_.pure (Resolve'unknown' tag_))
instance (OL.IsLabel "cap" (F.Variant F.Slot Resolve CapDescriptor)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 0)
instance (F.HasVariant "cap" F.Slot Resolve CapDescriptor)
instance (OL.IsLabel "exception" (F.Variant F.Slot Resolve Exception)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 1)
instance (F.HasVariant "exception" F.Slot Resolve Exception)
instance (OL.IsLabel "promiseId" (F.Field F.Slot Resolve Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "promiseId" F.Slot Resolve Std_.Word32)
data Release 
type instance (R.ReprFor Release) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Release) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Release) where
    type AllocHint Release = ()
    new  = GH.newStruct
instance (OL.IsLabel "id" (F.Field F.Slot Release Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "id" F.Slot Release Std_.Word32)
instance (OL.IsLabel "referenceCount" (F.Field F.Slot Release Std_.Word32)) where
    fromLabel  = (GH.dataField 32 0 32 0)
instance (F.HasField "referenceCount" F.Slot Release Std_.Word32)
data Disembargo 
type instance (R.ReprFor Disembargo) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Disembargo) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Disembargo) where
    type AllocHint Disembargo = ()
    new  = GH.newStruct
instance (OL.IsLabel "target" (F.Field F.Slot Disembargo MessageTarget)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "target" F.Slot Disembargo MessageTarget)
instance (OL.IsLabel "context" (F.Field F.Group Disembargo Disembargo'context)) where
    fromLabel  = GH.groupField
instance (F.HasField "context" F.Group Disembargo Disembargo'context)
data Disembargo'context 
type instance (R.ReprFor Disembargo'context) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Disembargo'context) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Disembargo'context) where
    type AllocHint Disembargo'context = ()
    new  = GH.newStruct
instance (F.HasUnion Disembargo'context) where
    unionField  = (GH.dataField 2 0 16 0)
    data RawWhich mut_ Disembargo'context
        = Disembargo'context'senderLoopback (R.Raw mut_ Std_.Word32)
        | Disembargo'context'receiverLoopback (R.Raw mut_ Std_.Word32)
        | Disembargo'context'accept (R.Raw mut_ ())
        | Disembargo'context'provide (R.Raw mut_ Std_.Word32)
        | Disembargo'context'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Disembargo'context'senderLoopback <$> (GH.readVariant (#senderLoopback :: (F.Variant F.Slot _ _)) struct_))
        1 ->
            (Disembargo'context'receiverLoopback <$> (GH.readVariant (#receiverLoopback :: (F.Variant F.Slot _ _)) struct_))
        2 ->
            (Disembargo'context'accept <$> (GH.readVariant (#accept :: (F.Variant F.Slot _ _)) struct_))
        3 ->
            (Disembargo'context'provide <$> (GH.readVariant (#provide :: (F.Variant F.Slot _ _)) struct_))
        _ ->
            (Std_.pure (Disembargo'context'unknown' tag_))
instance (OL.IsLabel "senderLoopback" (F.Variant F.Slot Disembargo'context Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 0 0 32 0) 0)
instance (F.HasVariant "senderLoopback" F.Slot Disembargo'context Std_.Word32)
instance (OL.IsLabel "receiverLoopback" (F.Variant F.Slot Disembargo'context Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 0 0 32 0) 1)
instance (F.HasVariant "receiverLoopback" F.Slot Disembargo'context Std_.Word32)
instance (OL.IsLabel "accept" (F.Variant F.Slot Disembargo'context ())) where
    fromLabel  = (F.Variant GH.voidField 2)
instance (F.HasVariant "accept" F.Slot Disembargo'context ())
instance (OL.IsLabel "provide" (F.Variant F.Slot Disembargo'context Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 0 0 32 0) 3)
instance (F.HasVariant "provide" F.Slot Disembargo'context Std_.Word32)
data Provide 
type instance (R.ReprFor Provide) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Provide) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Provide) where
    type AllocHint Provide = ()
    new  = GH.newStruct
instance (OL.IsLabel "questionId" (F.Field F.Slot Provide Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" F.Slot Provide Std_.Word32)
instance (OL.IsLabel "target" (F.Field F.Slot Provide MessageTarget)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "target" F.Slot Provide MessageTarget)
instance (OL.IsLabel "recipient" (F.Field F.Slot Provide Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "recipient" F.Slot Provide Basics.AnyPointer)
data Accept 
type instance (R.ReprFor Accept) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Accept) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Accept) where
    type AllocHint Accept = ()
    new  = GH.newStruct
instance (OL.IsLabel "questionId" (F.Field F.Slot Accept Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" F.Slot Accept Std_.Word32)
instance (OL.IsLabel "provision" (F.Field F.Slot Accept Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "provision" F.Slot Accept Basics.AnyPointer)
instance (OL.IsLabel "embargo" (F.Field F.Slot Accept Std_.Bool)) where
    fromLabel  = (GH.dataField 32 0 1 0)
instance (F.HasField "embargo" F.Slot Accept Std_.Bool)
data Join 
type instance (R.ReprFor Join) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Join) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Join) where
    type AllocHint Join = ()
    new  = GH.newStruct
instance (OL.IsLabel "questionId" (F.Field F.Slot Join Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" F.Slot Join Std_.Word32)
instance (OL.IsLabel "target" (F.Field F.Slot Join MessageTarget)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "target" F.Slot Join MessageTarget)
instance (OL.IsLabel "keyPart" (F.Field F.Slot Join Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "keyPart" F.Slot Join Basics.AnyPointer)
data MessageTarget 
type instance (R.ReprFor MessageTarget) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct MessageTarget) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate MessageTarget) where
    type AllocHint MessageTarget = ()
    new  = GH.newStruct
instance (F.HasUnion MessageTarget) where
    unionField  = (GH.dataField 2 0 16 0)
    data RawWhich mut_ MessageTarget
        = MessageTarget'importedCap (R.Raw mut_ Std_.Word32)
        | MessageTarget'promisedAnswer (R.Raw mut_ PromisedAnswer)
        | MessageTarget'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (MessageTarget'importedCap <$> (GH.readVariant (#importedCap :: (F.Variant F.Slot _ _)) struct_))
        1 ->
            (MessageTarget'promisedAnswer <$> (GH.readVariant (#promisedAnswer :: (F.Variant F.Slot _ _)) struct_))
        _ ->
            (Std_.pure (MessageTarget'unknown' tag_))
instance (OL.IsLabel "importedCap" (F.Variant F.Slot MessageTarget Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 0 0 32 0) 0)
instance (F.HasVariant "importedCap" F.Slot MessageTarget Std_.Word32)
instance (OL.IsLabel "promisedAnswer" (F.Variant F.Slot MessageTarget PromisedAnswer)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 1)
instance (F.HasVariant "promisedAnswer" F.Slot MessageTarget PromisedAnswer)
data Payload 
type instance (R.ReprFor Payload) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Payload) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Payload) where
    type AllocHint Payload = ()
    new  = GH.newStruct
instance (OL.IsLabel "content" (F.Field F.Slot Payload Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "content" F.Slot Payload Basics.AnyPointer)
instance (OL.IsLabel "capTable" (F.Field F.Slot Payload (R.List CapDescriptor))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "capTable" F.Slot Payload (R.List CapDescriptor))
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
            (CapDescriptor'none <$> (GH.readVariant (#none :: (F.Variant F.Slot _ _)) struct_))
        1 ->
            (CapDescriptor'senderHosted <$> (GH.readVariant (#senderHosted :: (F.Variant F.Slot _ _)) struct_))
        2 ->
            (CapDescriptor'senderPromise <$> (GH.readVariant (#senderPromise :: (F.Variant F.Slot _ _)) struct_))
        3 ->
            (CapDescriptor'receiverHosted <$> (GH.readVariant (#receiverHosted :: (F.Variant F.Slot _ _)) struct_))
        4 ->
            (CapDescriptor'receiverAnswer <$> (GH.readVariant (#receiverAnswer :: (F.Variant F.Slot _ _)) struct_))
        5 ->
            (CapDescriptor'thirdPartyHosted <$> (GH.readVariant (#thirdPartyHosted :: (F.Variant F.Slot _ _)) struct_))
        _ ->
            (Std_.pure (CapDescriptor'unknown' tag_))
instance (OL.IsLabel "none" (F.Variant F.Slot CapDescriptor ())) where
    fromLabel  = (F.Variant GH.voidField 0)
instance (F.HasVariant "none" F.Slot CapDescriptor ())
instance (OL.IsLabel "senderHosted" (F.Variant F.Slot CapDescriptor Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 32 0 32 0) 1)
instance (F.HasVariant "senderHosted" F.Slot CapDescriptor Std_.Word32)
instance (OL.IsLabel "senderPromise" (F.Variant F.Slot CapDescriptor Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 32 0 32 0) 2)
instance (F.HasVariant "senderPromise" F.Slot CapDescriptor Std_.Word32)
instance (OL.IsLabel "receiverHosted" (F.Variant F.Slot CapDescriptor Std_.Word32)) where
    fromLabel  = (F.Variant (GH.dataField 32 0 32 0) 3)
instance (F.HasVariant "receiverHosted" F.Slot CapDescriptor Std_.Word32)
instance (OL.IsLabel "receiverAnswer" (F.Variant F.Slot CapDescriptor PromisedAnswer)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 4)
instance (F.HasVariant "receiverAnswer" F.Slot CapDescriptor PromisedAnswer)
instance (OL.IsLabel "thirdPartyHosted" (F.Variant F.Slot CapDescriptor ThirdPartyCapDescriptor)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 5)
instance (F.HasVariant "thirdPartyHosted" F.Slot CapDescriptor ThirdPartyCapDescriptor)
instance (OL.IsLabel "attachedFd" (F.Field F.Slot CapDescriptor Std_.Word8)) where
    fromLabel  = (GH.dataField 16 0 8 255)
instance (F.HasField "attachedFd" F.Slot CapDescriptor Std_.Word8)
data PromisedAnswer 
type instance (R.ReprFor PromisedAnswer) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct PromisedAnswer) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate PromisedAnswer) where
    type AllocHint PromisedAnswer = ()
    new  = GH.newStruct
instance (OL.IsLabel "questionId" (F.Field F.Slot PromisedAnswer Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" F.Slot PromisedAnswer Std_.Word32)
instance (OL.IsLabel "transform" (F.Field F.Slot PromisedAnswer (R.List PromisedAnswer'Op))) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "transform" F.Slot PromisedAnswer (R.List PromisedAnswer'Op))
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
            (PromisedAnswer'Op'noop <$> (GH.readVariant (#noop :: (F.Variant F.Slot _ _)) struct_))
        1 ->
            (PromisedAnswer'Op'getPointerField <$> (GH.readVariant (#getPointerField :: (F.Variant F.Slot _ _)) struct_))
        _ ->
            (Std_.pure (PromisedAnswer'Op'unknown' tag_))
instance (OL.IsLabel "noop" (F.Variant F.Slot PromisedAnswer'Op ())) where
    fromLabel  = (F.Variant GH.voidField 0)
instance (F.HasVariant "noop" F.Slot PromisedAnswer'Op ())
instance (OL.IsLabel "getPointerField" (F.Variant F.Slot PromisedAnswer'Op Std_.Word16)) where
    fromLabel  = (F.Variant (GH.dataField 16 0 16 0) 1)
instance (F.HasVariant "getPointerField" F.Slot PromisedAnswer'Op Std_.Word16)
data ThirdPartyCapDescriptor 
type instance (R.ReprFor ThirdPartyCapDescriptor) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct ThirdPartyCapDescriptor) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate ThirdPartyCapDescriptor) where
    type AllocHint ThirdPartyCapDescriptor = ()
    new  = GH.newStruct
instance (OL.IsLabel "id" (F.Field F.Slot ThirdPartyCapDescriptor Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "id" F.Slot ThirdPartyCapDescriptor Basics.AnyPointer)
instance (OL.IsLabel "vineId" (F.Field F.Slot ThirdPartyCapDescriptor Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "vineId" F.Slot ThirdPartyCapDescriptor Std_.Word32)
data Exception 
type instance (R.ReprFor Exception) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Exception) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Exception) where
    type AllocHint Exception = ()
    new  = GH.newStruct
instance (OL.IsLabel "reason" (F.Field F.Slot Exception Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "reason" F.Slot Exception Basics.Text)
instance (OL.IsLabel "obsoleteIsCallersFault" (F.Field F.Slot Exception Std_.Bool)) where
    fromLabel  = (GH.dataField 0 0 1 0)
instance (F.HasField "obsoleteIsCallersFault" F.Slot Exception Std_.Bool)
instance (OL.IsLabel "obsoleteDurability" (F.Field F.Slot Exception Std_.Word16)) where
    fromLabel  = (GH.dataField 16 0 16 0)
instance (F.HasField "obsoleteDurability" F.Slot Exception Std_.Word16)
instance (OL.IsLabel "type_" (F.Field F.Slot Exception Exception'Type)) where
    fromLabel  = (GH.dataField 32 0 16 0)
instance (F.HasField "type_" F.Slot Exception Exception'Type)
data Exception'Type 
type instance (R.ReprFor Exception'Type) = (R.Data R.Sz16)