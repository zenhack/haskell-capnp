{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.Rpc.New where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.New.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers.New as GH
import qualified Capnp.New.Classes as C
import qualified GHC.Generics as Generics
import qualified Capnp.GenHelpers.ReExports.Data.ByteString as BS
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Message 
type instance (R.ReprFor Message) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Message) where
    typeId  = 10500036013887172658
instance (C.TypedStruct Message) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Message) where
    type AllocHint Message = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Message (C.Parsed Message))
instance (C.AllocateList Message) where
    type ListAllocHint Message = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Message (C.Parsed Message))
data instance C.Parsed Message
    = Message 
        {union' :: (C.Parsed (GH.Which Message))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Message))
deriving instance (Std_.Eq (C.Parsed Message))
instance (C.Parse Message (C.Parsed Message)) where
    parse raw_ = (Message <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Message (C.Parsed Message)) where
    marshalInto raw_ Message{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Message) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ Message
        = RW_Message'unimplemented (R.Raw mut_ Message)
        | RW_Message'abort (R.Raw mut_ Exception)
        | RW_Message'call (R.Raw mut_ Call)
        | RW_Message'return (R.Raw mut_ Return)
        | RW_Message'finish (R.Raw mut_ Finish)
        | RW_Message'resolve (R.Raw mut_ Resolve)
        | RW_Message'release (R.Raw mut_ Release)
        | RW_Message'obsoleteSave (R.Raw mut_ (Std_.Maybe Basics.AnyPointer))
        | RW_Message'bootstrap (R.Raw mut_ Bootstrap)
        | RW_Message'obsoleteDelete (R.Raw mut_ (Std_.Maybe Basics.AnyPointer))
        | RW_Message'provide (R.Raw mut_ Provide)
        | RW_Message'accept (R.Raw mut_ Accept)
        | RW_Message'join (R.Raw mut_ Join)
        | RW_Message'disembargo (R.Raw mut_ Disembargo)
        | RW_Message'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Message'unimplemented <$> (GH.readVariant #unimplemented struct_))
        1 ->
            (RW_Message'abort <$> (GH.readVariant #abort struct_))
        2 ->
            (RW_Message'call <$> (GH.readVariant #call struct_))
        3 ->
            (RW_Message'return <$> (GH.readVariant #return struct_))
        4 ->
            (RW_Message'finish <$> (GH.readVariant #finish struct_))
        5 ->
            (RW_Message'resolve <$> (GH.readVariant #resolve struct_))
        6 ->
            (RW_Message'release <$> (GH.readVariant #release struct_))
        7 ->
            (RW_Message'obsoleteSave <$> (GH.readVariant #obsoleteSave struct_))
        8 ->
            (RW_Message'bootstrap <$> (GH.readVariant #bootstrap struct_))
        9 ->
            (RW_Message'obsoleteDelete <$> (GH.readVariant #obsoleteDelete struct_))
        10 ->
            (RW_Message'provide <$> (GH.readVariant #provide struct_))
        11 ->
            (RW_Message'accept <$> (GH.readVariant #accept struct_))
        12 ->
            (RW_Message'join <$> (GH.readVariant #join struct_))
        13 ->
            (RW_Message'disembargo <$> (GH.readVariant #disembargo struct_))
        _ ->
            (Std_.pure (RW_Message'unknown' tag_))
    data Which Message
instance (GH.HasVariant "unimplemented" GH.Slot Message Message) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 0)
instance (GH.HasVariant "abort" GH.Slot Message Exception) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 1)
instance (GH.HasVariant "call" GH.Slot Message Call) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 2)
instance (GH.HasVariant "return" GH.Slot Message Return) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 3)
instance (GH.HasVariant "finish" GH.Slot Message Finish) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 4)
instance (GH.HasVariant "resolve" GH.Slot Message Resolve) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 5)
instance (GH.HasVariant "release" GH.Slot Message Release) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 6)
instance (GH.HasVariant "obsoleteSave" GH.Slot Message (Std_.Maybe Basics.AnyPointer)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 7)
instance (GH.HasVariant "bootstrap" GH.Slot Message Bootstrap) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 8)
instance (GH.HasVariant "obsoleteDelete" GH.Slot Message (Std_.Maybe Basics.AnyPointer)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 9)
instance (GH.HasVariant "provide" GH.Slot Message Provide) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 10)
instance (GH.HasVariant "accept" GH.Slot Message Accept) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 11)
instance (GH.HasVariant "join" GH.Slot Message Join) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 12)
instance (GH.HasVariant "disembargo" GH.Slot Message Disembargo) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 13)
data instance C.Parsed (GH.Which Message)
    = Message'unimplemented (RP.Parsed Message)
    | Message'abort (RP.Parsed Exception)
    | Message'call (RP.Parsed Call)
    | Message'return (RP.Parsed Return)
    | Message'finish (RP.Parsed Finish)
    | Message'resolve (RP.Parsed Resolve)
    | Message'release (RP.Parsed Release)
    | Message'obsoleteSave (RP.Parsed (Std_.Maybe Basics.AnyPointer))
    | Message'bootstrap (RP.Parsed Bootstrap)
    | Message'obsoleteDelete (RP.Parsed (Std_.Maybe Basics.AnyPointer))
    | Message'provide (RP.Parsed Provide)
    | Message'accept (RP.Parsed Accept)
    | Message'join (RP.Parsed Join)
    | Message'disembargo (RP.Parsed Disembargo)
    | Message'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Message)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Message)))
instance (C.Parse (GH.Which Message) (C.Parsed (GH.Which Message))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Message'unimplemented rawArg_) ->
                (Message'unimplemented <$> (C.parse rawArg_))
            (RW_Message'abort rawArg_) ->
                (Message'abort <$> (C.parse rawArg_))
            (RW_Message'call rawArg_) ->
                (Message'call <$> (C.parse rawArg_))
            (RW_Message'return rawArg_) ->
                (Message'return <$> (C.parse rawArg_))
            (RW_Message'finish rawArg_) ->
                (Message'finish <$> (C.parse rawArg_))
            (RW_Message'resolve rawArg_) ->
                (Message'resolve <$> (C.parse rawArg_))
            (RW_Message'release rawArg_) ->
                (Message'release <$> (C.parse rawArg_))
            (RW_Message'obsoleteSave rawArg_) ->
                (Message'obsoleteSave <$> (C.parse rawArg_))
            (RW_Message'bootstrap rawArg_) ->
                (Message'bootstrap <$> (C.parse rawArg_))
            (RW_Message'obsoleteDelete rawArg_) ->
                (Message'obsoleteDelete <$> (C.parse rawArg_))
            (RW_Message'provide rawArg_) ->
                (Message'provide <$> (C.parse rawArg_))
            (RW_Message'accept rawArg_) ->
                (Message'accept <$> (C.parse rawArg_))
            (RW_Message'join rawArg_) ->
                (Message'join <$> (C.parse rawArg_))
            (RW_Message'disembargo rawArg_) ->
                (Message'disembargo <$> (C.parse rawArg_))
            (RW_Message'unknown' tag_) ->
                (Std_.pure (Message'unknown' tag_))
        )
instance (C.Marshal (GH.Which Message) (C.Parsed (GH.Which Message))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Message'unimplemented arg_) ->
            (GH.encodeVariant #unimplemented arg_ (GH.unionStruct raw_))
        (Message'abort arg_) ->
            (GH.encodeVariant #abort arg_ (GH.unionStruct raw_))
        (Message'call arg_) ->
            (GH.encodeVariant #call arg_ (GH.unionStruct raw_))
        (Message'return arg_) ->
            (GH.encodeVariant #return arg_ (GH.unionStruct raw_))
        (Message'finish arg_) ->
            (GH.encodeVariant #finish arg_ (GH.unionStruct raw_))
        (Message'resolve arg_) ->
            (GH.encodeVariant #resolve arg_ (GH.unionStruct raw_))
        (Message'release arg_) ->
            (GH.encodeVariant #release arg_ (GH.unionStruct raw_))
        (Message'obsoleteSave arg_) ->
            (GH.encodeVariant #obsoleteSave arg_ (GH.unionStruct raw_))
        (Message'bootstrap arg_) ->
            (GH.encodeVariant #bootstrap arg_ (GH.unionStruct raw_))
        (Message'obsoleteDelete arg_) ->
            (GH.encodeVariant #obsoleteDelete arg_ (GH.unionStruct raw_))
        (Message'provide arg_) ->
            (GH.encodeVariant #provide arg_ (GH.unionStruct raw_))
        (Message'accept arg_) ->
            (GH.encodeVariant #accept arg_ (GH.unionStruct raw_))
        (Message'join arg_) ->
            (GH.encodeVariant #join arg_ (GH.unionStruct raw_))
        (Message'disembargo arg_) ->
            (GH.encodeVariant #disembargo arg_ (GH.unionStruct raw_))
        (Message'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Bootstrap 
type instance (R.ReprFor Bootstrap) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Bootstrap) where
    typeId  = 16811039658553601732
instance (C.TypedStruct Bootstrap) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Bootstrap) where
    type AllocHint Bootstrap = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Bootstrap (C.Parsed Bootstrap))
instance (C.AllocateList Bootstrap) where
    type ListAllocHint Bootstrap = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Bootstrap (C.Parsed Bootstrap))
data instance C.Parsed Bootstrap
    = Bootstrap 
        {questionId :: (RP.Parsed Std_.Word32)
        ,deprecatedObjectId :: (RP.Parsed (Std_.Maybe Basics.AnyPointer))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Bootstrap))
deriving instance (Std_.Eq (C.Parsed Bootstrap))
instance (C.Parse Bootstrap (C.Parsed Bootstrap)) where
    parse raw_ = (Bootstrap <$> (GH.parseField #questionId raw_)
                            <*> (GH.parseField #deprecatedObjectId raw_))
instance (C.Marshal Bootstrap (C.Parsed Bootstrap)) where
    marshalInto raw_ Bootstrap{..} = (do
        (GH.encodeField #questionId questionId raw_)
        (GH.encodeField #deprecatedObjectId deprecatedObjectId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "questionId" GH.Slot Bootstrap Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "deprecatedObjectId" GH.Slot Bootstrap (Std_.Maybe Basics.AnyPointer)) where
    fieldByLabel  = (GH.ptrField 0)
data Call 
type instance (R.ReprFor Call) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Call) where
    typeId  = 9469473312751832276
instance (C.TypedStruct Call) where
    numStructWords  = 3
    numStructPtrs  = 3
instance (C.Allocate Call) where
    type AllocHint Call = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Call (C.Parsed Call))
instance (C.AllocateList Call) where
    type ListAllocHint Call = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Call (C.Parsed Call))
data instance C.Parsed Call
    = Call 
        {questionId :: (RP.Parsed Std_.Word32)
        ,target :: (RP.Parsed MessageTarget)
        ,interfaceId :: (RP.Parsed Std_.Word64)
        ,methodId :: (RP.Parsed Std_.Word16)
        ,params :: (RP.Parsed Payload)
        ,sendResultsTo :: (RP.Parsed Call'sendResultsTo)
        ,allowThirdPartyTailCall :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Call))
deriving instance (Std_.Eq (C.Parsed Call))
instance (C.Parse Call (C.Parsed Call)) where
    parse raw_ = (Call <$> (GH.parseField #questionId raw_)
                       <*> (GH.parseField #target raw_)
                       <*> (GH.parseField #interfaceId raw_)
                       <*> (GH.parseField #methodId raw_)
                       <*> (GH.parseField #params raw_)
                       <*> (GH.parseField #sendResultsTo raw_)
                       <*> (GH.parseField #allowThirdPartyTailCall raw_))
instance (C.Marshal Call (C.Parsed Call)) where
    marshalInto raw_ Call{..} = (do
        (GH.encodeField #questionId questionId raw_)
        (GH.encodeField #target target raw_)
        (GH.encodeField #interfaceId interfaceId raw_)
        (GH.encodeField #methodId methodId raw_)
        (GH.encodeField #params params raw_)
        (do
            group_ <- (GH.readField #sendResultsTo raw_)
            (C.marshalInto group_ sendResultsTo)
            )
        (GH.encodeField #allowThirdPartyTailCall allowThirdPartyTailCall raw_)
        (Std_.pure ())
        )
instance (GH.HasField "questionId" GH.Slot Call Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "target" GH.Slot Call MessageTarget) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "interfaceId" GH.Slot Call Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "methodId" GH.Slot Call Std_.Word16) where
    fieldByLabel  = (GH.dataField 32 0 16 0)
instance (GH.HasField "params" GH.Slot Call Payload) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "sendResultsTo" GH.Group Call Call'sendResultsTo) where
    fieldByLabel  = GH.groupField
instance (GH.HasField "allowThirdPartyTailCall" GH.Slot Call Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 2 1 0)
data Call'sendResultsTo 
type instance (R.ReprFor Call'sendResultsTo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Call'sendResultsTo) where
    typeId  = 15774052265921044377
instance (C.TypedStruct Call'sendResultsTo) where
    numStructWords  = 3
    numStructPtrs  = 3
instance (C.Allocate Call'sendResultsTo) where
    type AllocHint Call'sendResultsTo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Call'sendResultsTo (C.Parsed Call'sendResultsTo))
instance (C.AllocateList Call'sendResultsTo) where
    type ListAllocHint Call'sendResultsTo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Call'sendResultsTo (C.Parsed Call'sendResultsTo))
data instance C.Parsed Call'sendResultsTo
    = Call'sendResultsTo' 
        {union' :: (C.Parsed (GH.Which Call'sendResultsTo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Call'sendResultsTo))
deriving instance (Std_.Eq (C.Parsed Call'sendResultsTo))
instance (C.Parse Call'sendResultsTo (C.Parsed Call'sendResultsTo)) where
    parse raw_ = (Call'sendResultsTo' <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Call'sendResultsTo (C.Parsed Call'sendResultsTo)) where
    marshalInto raw_ Call'sendResultsTo'{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Call'sendResultsTo) where
    unionField  = (GH.dataField 48 0 16 0)
    data RawWhich mut_ Call'sendResultsTo
        = RW_Call'sendResultsTo'caller (R.Raw mut_ ())
        | RW_Call'sendResultsTo'yourself (R.Raw mut_ ())
        | RW_Call'sendResultsTo'thirdParty (R.Raw mut_ (Std_.Maybe Basics.AnyPointer))
        | RW_Call'sendResultsTo'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Call'sendResultsTo'caller <$> (GH.readVariant #caller struct_))
        1 ->
            (RW_Call'sendResultsTo'yourself <$> (GH.readVariant #yourself struct_))
        2 ->
            (RW_Call'sendResultsTo'thirdParty <$> (GH.readVariant #thirdParty struct_))
        _ ->
            (Std_.pure (RW_Call'sendResultsTo'unknown' tag_))
    data Which Call'sendResultsTo
instance (GH.HasVariant "caller" GH.Slot Call'sendResultsTo ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance (GH.HasVariant "yourself" GH.Slot Call'sendResultsTo ()) where
    variantByLabel  = (GH.Variant GH.voidField 1)
instance (GH.HasVariant "thirdParty" GH.Slot Call'sendResultsTo (Std_.Maybe Basics.AnyPointer)) where
    variantByLabel  = (GH.Variant (GH.ptrField 2) 2)
data instance C.Parsed (GH.Which Call'sendResultsTo)
    = Call'sendResultsTo'caller 
    | Call'sendResultsTo'yourself 
    | Call'sendResultsTo'thirdParty (RP.Parsed (Std_.Maybe Basics.AnyPointer))
    | Call'sendResultsTo'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Call'sendResultsTo)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Call'sendResultsTo)))
instance (C.Parse (GH.Which Call'sendResultsTo) (C.Parsed (GH.Which Call'sendResultsTo))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Call'sendResultsTo'caller _) ->
                (Std_.pure Call'sendResultsTo'caller)
            (RW_Call'sendResultsTo'yourself _) ->
                (Std_.pure Call'sendResultsTo'yourself)
            (RW_Call'sendResultsTo'thirdParty rawArg_) ->
                (Call'sendResultsTo'thirdParty <$> (C.parse rawArg_))
            (RW_Call'sendResultsTo'unknown' tag_) ->
                (Std_.pure (Call'sendResultsTo'unknown' tag_))
        )
instance (C.Marshal (GH.Which Call'sendResultsTo) (C.Parsed (GH.Which Call'sendResultsTo))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Call'sendResultsTo'caller) ->
            (GH.encodeVariant #caller () (GH.unionStruct raw_))
        (Call'sendResultsTo'yourself) ->
            (GH.encodeVariant #yourself () (GH.unionStruct raw_))
        (Call'sendResultsTo'thirdParty arg_) ->
            (GH.encodeVariant #thirdParty arg_ (GH.unionStruct raw_))
        (Call'sendResultsTo'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Return 
type instance (R.ReprFor Return) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Return) where
    typeId  = 11392333052105676602
instance (C.TypedStruct Return) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate Return) where
    type AllocHint Return = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Return (C.Parsed Return))
instance (C.AllocateList Return) where
    type ListAllocHint Return = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Return (C.Parsed Return))
data instance C.Parsed Return
    = Return 
        {answerId :: (RP.Parsed Std_.Word32)
        ,releaseParamCaps :: (RP.Parsed Std_.Bool)
        ,union' :: (C.Parsed (GH.Which Return))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Return))
deriving instance (Std_.Eq (C.Parsed Return))
instance (C.Parse Return (C.Parsed Return)) where
    parse raw_ = (Return <$> (GH.parseField #answerId raw_)
                         <*> (GH.parseField #releaseParamCaps raw_)
                         <*> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Return (C.Parsed Return)) where
    marshalInto raw_ Return{..} = (do
        (GH.encodeField #answerId answerId raw_)
        (GH.encodeField #releaseParamCaps releaseParamCaps raw_)
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Return) where
    unionField  = (GH.dataField 48 0 16 0)
    data RawWhich mut_ Return
        = RW_Return'results (R.Raw mut_ Payload)
        | RW_Return'exception (R.Raw mut_ Exception)
        | RW_Return'canceled (R.Raw mut_ ())
        | RW_Return'resultsSentElsewhere (R.Raw mut_ ())
        | RW_Return'takeFromOtherQuestion (R.Raw mut_ Std_.Word32)
        | RW_Return'acceptFromThirdParty (R.Raw mut_ (Std_.Maybe Basics.AnyPointer))
        | RW_Return'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Return'results <$> (GH.readVariant #results struct_))
        1 ->
            (RW_Return'exception <$> (GH.readVariant #exception struct_))
        2 ->
            (RW_Return'canceled <$> (GH.readVariant #canceled struct_))
        3 ->
            (RW_Return'resultsSentElsewhere <$> (GH.readVariant #resultsSentElsewhere struct_))
        4 ->
            (RW_Return'takeFromOtherQuestion <$> (GH.readVariant #takeFromOtherQuestion struct_))
        5 ->
            (RW_Return'acceptFromThirdParty <$> (GH.readVariant #acceptFromThirdParty struct_))
        _ ->
            (Std_.pure (RW_Return'unknown' tag_))
    data Which Return
instance (GH.HasVariant "results" GH.Slot Return Payload) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 0)
instance (GH.HasVariant "exception" GH.Slot Return Exception) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 1)
instance (GH.HasVariant "canceled" GH.Slot Return ()) where
    variantByLabel  = (GH.Variant GH.voidField 2)
instance (GH.HasVariant "resultsSentElsewhere" GH.Slot Return ()) where
    variantByLabel  = (GH.Variant GH.voidField 3)
instance (GH.HasVariant "takeFromOtherQuestion" GH.Slot Return Std_.Word32) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 32 0) 4)
instance (GH.HasVariant "acceptFromThirdParty" GH.Slot Return (Std_.Maybe Basics.AnyPointer)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 5)
data instance C.Parsed (GH.Which Return)
    = Return'results (RP.Parsed Payload)
    | Return'exception (RP.Parsed Exception)
    | Return'canceled 
    | Return'resultsSentElsewhere 
    | Return'takeFromOtherQuestion (RP.Parsed Std_.Word32)
    | Return'acceptFromThirdParty (RP.Parsed (Std_.Maybe Basics.AnyPointer))
    | Return'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Return)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Return)))
instance (C.Parse (GH.Which Return) (C.Parsed (GH.Which Return))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Return'results rawArg_) ->
                (Return'results <$> (C.parse rawArg_))
            (RW_Return'exception rawArg_) ->
                (Return'exception <$> (C.parse rawArg_))
            (RW_Return'canceled _) ->
                (Std_.pure Return'canceled)
            (RW_Return'resultsSentElsewhere _) ->
                (Std_.pure Return'resultsSentElsewhere)
            (RW_Return'takeFromOtherQuestion rawArg_) ->
                (Return'takeFromOtherQuestion <$> (C.parse rawArg_))
            (RW_Return'acceptFromThirdParty rawArg_) ->
                (Return'acceptFromThirdParty <$> (C.parse rawArg_))
            (RW_Return'unknown' tag_) ->
                (Std_.pure (Return'unknown' tag_))
        )
instance (C.Marshal (GH.Which Return) (C.Parsed (GH.Which Return))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Return'results arg_) ->
            (GH.encodeVariant #results arg_ (GH.unionStruct raw_))
        (Return'exception arg_) ->
            (GH.encodeVariant #exception arg_ (GH.unionStruct raw_))
        (Return'canceled) ->
            (GH.encodeVariant #canceled () (GH.unionStruct raw_))
        (Return'resultsSentElsewhere) ->
            (GH.encodeVariant #resultsSentElsewhere () (GH.unionStruct raw_))
        (Return'takeFromOtherQuestion arg_) ->
            (GH.encodeVariant #takeFromOtherQuestion arg_ (GH.unionStruct raw_))
        (Return'acceptFromThirdParty arg_) ->
            (GH.encodeVariant #acceptFromThirdParty arg_ (GH.unionStruct raw_))
        (Return'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
instance (GH.HasField "answerId" GH.Slot Return Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "releaseParamCaps" GH.Slot Return Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 0 1 1)
data Finish 
type instance (R.ReprFor Finish) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Finish) where
    typeId  = 15239388059401719395
instance (C.TypedStruct Finish) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Finish) where
    type AllocHint Finish = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Finish (C.Parsed Finish))
instance (C.AllocateList Finish) where
    type ListAllocHint Finish = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Finish (C.Parsed Finish))
data instance C.Parsed Finish
    = Finish 
        {questionId :: (RP.Parsed Std_.Word32)
        ,releaseResultCaps :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Finish))
deriving instance (Std_.Eq (C.Parsed Finish))
instance (C.Parse Finish (C.Parsed Finish)) where
    parse raw_ = (Finish <$> (GH.parseField #questionId raw_)
                         <*> (GH.parseField #releaseResultCaps raw_))
instance (C.Marshal Finish (C.Parsed Finish)) where
    marshalInto raw_ Finish{..} = (do
        (GH.encodeField #questionId questionId raw_)
        (GH.encodeField #releaseResultCaps releaseResultCaps raw_)
        (Std_.pure ())
        )
instance (GH.HasField "questionId" GH.Slot Finish Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "releaseResultCaps" GH.Slot Finish Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 0 1 1)
data Resolve 
type instance (R.ReprFor Resolve) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Resolve) where
    typeId  = 13529541526594062446
instance (C.TypedStruct Resolve) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Resolve) where
    type AllocHint Resolve = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Resolve (C.Parsed Resolve))
instance (C.AllocateList Resolve) where
    type ListAllocHint Resolve = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Resolve (C.Parsed Resolve))
data instance C.Parsed Resolve
    = Resolve 
        {promiseId :: (RP.Parsed Std_.Word32)
        ,union' :: (C.Parsed (GH.Which Resolve))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Resolve))
deriving instance (Std_.Eq (C.Parsed Resolve))
instance (C.Parse Resolve (C.Parsed Resolve)) where
    parse raw_ = (Resolve <$> (GH.parseField #promiseId raw_)
                          <*> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Resolve (C.Parsed Resolve)) where
    marshalInto raw_ Resolve{..} = (do
        (GH.encodeField #promiseId promiseId raw_)
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Resolve) where
    unionField  = (GH.dataField 32 0 16 0)
    data RawWhich mut_ Resolve
        = RW_Resolve'cap (R.Raw mut_ CapDescriptor)
        | RW_Resolve'exception (R.Raw mut_ Exception)
        | RW_Resolve'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Resolve'cap <$> (GH.readVariant #cap struct_))
        1 ->
            (RW_Resolve'exception <$> (GH.readVariant #exception struct_))
        _ ->
            (Std_.pure (RW_Resolve'unknown' tag_))
    data Which Resolve
instance (GH.HasVariant "cap" GH.Slot Resolve CapDescriptor) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 0)
instance (GH.HasVariant "exception" GH.Slot Resolve Exception) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 1)
data instance C.Parsed (GH.Which Resolve)
    = Resolve'cap (RP.Parsed CapDescriptor)
    | Resolve'exception (RP.Parsed Exception)
    | Resolve'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Resolve)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Resolve)))
instance (C.Parse (GH.Which Resolve) (C.Parsed (GH.Which Resolve))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Resolve'cap rawArg_) ->
                (Resolve'cap <$> (C.parse rawArg_))
            (RW_Resolve'exception rawArg_) ->
                (Resolve'exception <$> (C.parse rawArg_))
            (RW_Resolve'unknown' tag_) ->
                (Std_.pure (Resolve'unknown' tag_))
        )
instance (C.Marshal (GH.Which Resolve) (C.Parsed (GH.Which Resolve))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Resolve'cap arg_) ->
            (GH.encodeVariant #cap arg_ (GH.unionStruct raw_))
        (Resolve'exception arg_) ->
            (GH.encodeVariant #exception arg_ (GH.unionStruct raw_))
        (Resolve'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
instance (GH.HasField "promiseId" GH.Slot Resolve Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
data Release 
type instance (R.ReprFor Release) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Release) where
    typeId  = 12473400923157197975
instance (C.TypedStruct Release) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Release) where
    type AllocHint Release = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Release (C.Parsed Release))
instance (C.AllocateList Release) where
    type ListAllocHint Release = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Release (C.Parsed Release))
data instance C.Parsed Release
    = Release 
        {id :: (RP.Parsed Std_.Word32)
        ,referenceCount :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Release))
deriving instance (Std_.Eq (C.Parsed Release))
instance (C.Parse Release (C.Parsed Release)) where
    parse raw_ = (Release <$> (GH.parseField #id raw_)
                          <*> (GH.parseField #referenceCount raw_))
instance (C.Marshal Release (C.Parsed Release)) where
    marshalInto raw_ Release{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #referenceCount referenceCount raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot Release Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "referenceCount" GH.Slot Release Std_.Word32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
data Disembargo 
type instance (R.ReprFor Disembargo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disembargo) where
    typeId  = 17970548384007534353
instance (C.TypedStruct Disembargo) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Disembargo) where
    type AllocHint Disembargo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disembargo (C.Parsed Disembargo))
instance (C.AllocateList Disembargo) where
    type ListAllocHint Disembargo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disembargo (C.Parsed Disembargo))
data instance C.Parsed Disembargo
    = Disembargo 
        {target :: (RP.Parsed MessageTarget)
        ,context :: (RP.Parsed Disembargo'context)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disembargo))
deriving instance (Std_.Eq (C.Parsed Disembargo))
instance (C.Parse Disembargo (C.Parsed Disembargo)) where
    parse raw_ = (Disembargo <$> (GH.parseField #target raw_)
                             <*> (GH.parseField #context raw_))
instance (C.Marshal Disembargo (C.Parsed Disembargo)) where
    marshalInto raw_ Disembargo{..} = (do
        (GH.encodeField #target target raw_)
        (do
            group_ <- (GH.readField #context raw_)
            (C.marshalInto group_ context)
            )
        (Std_.pure ())
        )
instance (GH.HasField "target" GH.Slot Disembargo MessageTarget) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "context" GH.Group Disembargo Disembargo'context) where
    fieldByLabel  = GH.groupField
data Disembargo'context 
type instance (R.ReprFor Disembargo'context) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disembargo'context) where
    typeId  = 15376050949367520589
instance (C.TypedStruct Disembargo'context) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Disembargo'context) where
    type AllocHint Disembargo'context = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disembargo'context (C.Parsed Disembargo'context))
instance (C.AllocateList Disembargo'context) where
    type ListAllocHint Disembargo'context = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disembargo'context (C.Parsed Disembargo'context))
data instance C.Parsed Disembargo'context
    = Disembargo'context' 
        {union' :: (C.Parsed (GH.Which Disembargo'context))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disembargo'context))
deriving instance (Std_.Eq (C.Parsed Disembargo'context))
instance (C.Parse Disembargo'context (C.Parsed Disembargo'context)) where
    parse raw_ = (Disembargo'context' <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Disembargo'context (C.Parsed Disembargo'context)) where
    marshalInto raw_ Disembargo'context'{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Disembargo'context) where
    unionField  = (GH.dataField 32 0 16 0)
    data RawWhich mut_ Disembargo'context
        = RW_Disembargo'context'senderLoopback (R.Raw mut_ Std_.Word32)
        | RW_Disembargo'context'receiverLoopback (R.Raw mut_ Std_.Word32)
        | RW_Disembargo'context'accept (R.Raw mut_ ())
        | RW_Disembargo'context'provide (R.Raw mut_ Std_.Word32)
        | RW_Disembargo'context'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Disembargo'context'senderLoopback <$> (GH.readVariant #senderLoopback struct_))
        1 ->
            (RW_Disembargo'context'receiverLoopback <$> (GH.readVariant #receiverLoopback struct_))
        2 ->
            (RW_Disembargo'context'accept <$> (GH.readVariant #accept struct_))
        3 ->
            (RW_Disembargo'context'provide <$> (GH.readVariant #provide struct_))
        _ ->
            (Std_.pure (RW_Disembargo'context'unknown' tag_))
    data Which Disembargo'context
instance (GH.HasVariant "senderLoopback" GH.Slot Disembargo'context Std_.Word32) where
    variantByLabel  = (GH.Variant (GH.dataField 0 0 32 0) 0)
instance (GH.HasVariant "receiverLoopback" GH.Slot Disembargo'context Std_.Word32) where
    variantByLabel  = (GH.Variant (GH.dataField 0 0 32 0) 1)
instance (GH.HasVariant "accept" GH.Slot Disembargo'context ()) where
    variantByLabel  = (GH.Variant GH.voidField 2)
instance (GH.HasVariant "provide" GH.Slot Disembargo'context Std_.Word32) where
    variantByLabel  = (GH.Variant (GH.dataField 0 0 32 0) 3)
data instance C.Parsed (GH.Which Disembargo'context)
    = Disembargo'context'senderLoopback (RP.Parsed Std_.Word32)
    | Disembargo'context'receiverLoopback (RP.Parsed Std_.Word32)
    | Disembargo'context'accept 
    | Disembargo'context'provide (RP.Parsed Std_.Word32)
    | Disembargo'context'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Disembargo'context)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Disembargo'context)))
instance (C.Parse (GH.Which Disembargo'context) (C.Parsed (GH.Which Disembargo'context))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Disembargo'context'senderLoopback rawArg_) ->
                (Disembargo'context'senderLoopback <$> (C.parse rawArg_))
            (RW_Disembargo'context'receiverLoopback rawArg_) ->
                (Disembargo'context'receiverLoopback <$> (C.parse rawArg_))
            (RW_Disembargo'context'accept _) ->
                (Std_.pure Disembargo'context'accept)
            (RW_Disembargo'context'provide rawArg_) ->
                (Disembargo'context'provide <$> (C.parse rawArg_))
            (RW_Disembargo'context'unknown' tag_) ->
                (Std_.pure (Disembargo'context'unknown' tag_))
        )
instance (C.Marshal (GH.Which Disembargo'context) (C.Parsed (GH.Which Disembargo'context))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Disembargo'context'senderLoopback arg_) ->
            (GH.encodeVariant #senderLoopback arg_ (GH.unionStruct raw_))
        (Disembargo'context'receiverLoopback arg_) ->
            (GH.encodeVariant #receiverLoopback arg_ (GH.unionStruct raw_))
        (Disembargo'context'accept) ->
            (GH.encodeVariant #accept () (GH.unionStruct raw_))
        (Disembargo'context'provide arg_) ->
            (GH.encodeVariant #provide arg_ (GH.unionStruct raw_))
        (Disembargo'context'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Provide 
type instance (R.ReprFor Provide) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Provide) where
    typeId  = 11270825879279873114
instance (C.TypedStruct Provide) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Provide) where
    type AllocHint Provide = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Provide (C.Parsed Provide))
instance (C.AllocateList Provide) where
    type ListAllocHint Provide = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Provide (C.Parsed Provide))
data instance C.Parsed Provide
    = Provide 
        {questionId :: (RP.Parsed Std_.Word32)
        ,target :: (RP.Parsed MessageTarget)
        ,recipient :: (RP.Parsed (Std_.Maybe Basics.AnyPointer))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Provide))
deriving instance (Std_.Eq (C.Parsed Provide))
instance (C.Parse Provide (C.Parsed Provide)) where
    parse raw_ = (Provide <$> (GH.parseField #questionId raw_)
                          <*> (GH.parseField #target raw_)
                          <*> (GH.parseField #recipient raw_))
instance (C.Marshal Provide (C.Parsed Provide)) where
    marshalInto raw_ Provide{..} = (do
        (GH.encodeField #questionId questionId raw_)
        (GH.encodeField #target target raw_)
        (GH.encodeField #recipient recipient raw_)
        (Std_.pure ())
        )
instance (GH.HasField "questionId" GH.Slot Provide Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "target" GH.Slot Provide MessageTarget) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "recipient" GH.Slot Provide (Std_.Maybe Basics.AnyPointer)) where
    fieldByLabel  = (GH.ptrField 1)
data Accept 
type instance (R.ReprFor Accept) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Accept) where
    typeId  = 15332985841292492822
instance (C.TypedStruct Accept) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Accept) where
    type AllocHint Accept = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Accept (C.Parsed Accept))
instance (C.AllocateList Accept) where
    type ListAllocHint Accept = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Accept (C.Parsed Accept))
data instance C.Parsed Accept
    = Accept 
        {questionId :: (RP.Parsed Std_.Word32)
        ,provision :: (RP.Parsed (Std_.Maybe Basics.AnyPointer))
        ,embargo :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Accept))
deriving instance (Std_.Eq (C.Parsed Accept))
instance (C.Parse Accept (C.Parsed Accept)) where
    parse raw_ = (Accept <$> (GH.parseField #questionId raw_)
                         <*> (GH.parseField #provision raw_)
                         <*> (GH.parseField #embargo raw_))
instance (C.Marshal Accept (C.Parsed Accept)) where
    marshalInto raw_ Accept{..} = (do
        (GH.encodeField #questionId questionId raw_)
        (GH.encodeField #provision provision raw_)
        (GH.encodeField #embargo embargo raw_)
        (Std_.pure ())
        )
instance (GH.HasField "questionId" GH.Slot Accept Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "provision" GH.Slot Accept (Std_.Maybe Basics.AnyPointer)) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "embargo" GH.Slot Accept Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 0 1 0)
data Join 
type instance (R.ReprFor Join) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Join) where
    typeId  = 18149955118657700271
instance (C.TypedStruct Join) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Join) where
    type AllocHint Join = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Join (C.Parsed Join))
instance (C.AllocateList Join) where
    type ListAllocHint Join = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Join (C.Parsed Join))
data instance C.Parsed Join
    = Join 
        {questionId :: (RP.Parsed Std_.Word32)
        ,target :: (RP.Parsed MessageTarget)
        ,keyPart :: (RP.Parsed (Std_.Maybe Basics.AnyPointer))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Join))
deriving instance (Std_.Eq (C.Parsed Join))
instance (C.Parse Join (C.Parsed Join)) where
    parse raw_ = (Join <$> (GH.parseField #questionId raw_)
                       <*> (GH.parseField #target raw_)
                       <*> (GH.parseField #keyPart raw_))
instance (C.Marshal Join (C.Parsed Join)) where
    marshalInto raw_ Join{..} = (do
        (GH.encodeField #questionId questionId raw_)
        (GH.encodeField #target target raw_)
        (GH.encodeField #keyPart keyPart raw_)
        (Std_.pure ())
        )
instance (GH.HasField "questionId" GH.Slot Join Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "target" GH.Slot Join MessageTarget) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "keyPart" GH.Slot Join (Std_.Maybe Basics.AnyPointer)) where
    fieldByLabel  = (GH.ptrField 1)
data MessageTarget 
type instance (R.ReprFor MessageTarget) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId MessageTarget) where
    typeId  = 10789521159760378817
instance (C.TypedStruct MessageTarget) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate MessageTarget) where
    type AllocHint MessageTarget = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc MessageTarget (C.Parsed MessageTarget))
instance (C.AllocateList MessageTarget) where
    type ListAllocHint MessageTarget = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc MessageTarget (C.Parsed MessageTarget))
data instance C.Parsed MessageTarget
    = MessageTarget 
        {union' :: (C.Parsed (GH.Which MessageTarget))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed MessageTarget))
deriving instance (Std_.Eq (C.Parsed MessageTarget))
instance (C.Parse MessageTarget (C.Parsed MessageTarget)) where
    parse raw_ = (MessageTarget <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal MessageTarget (C.Parsed MessageTarget)) where
    marshalInto raw_ MessageTarget{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion MessageTarget) where
    unionField  = (GH.dataField 32 0 16 0)
    data RawWhich mut_ MessageTarget
        = RW_MessageTarget'importedCap (R.Raw mut_ Std_.Word32)
        | RW_MessageTarget'promisedAnswer (R.Raw mut_ PromisedAnswer)
        | RW_MessageTarget'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_MessageTarget'importedCap <$> (GH.readVariant #importedCap struct_))
        1 ->
            (RW_MessageTarget'promisedAnswer <$> (GH.readVariant #promisedAnswer struct_))
        _ ->
            (Std_.pure (RW_MessageTarget'unknown' tag_))
    data Which MessageTarget
instance (GH.HasVariant "importedCap" GH.Slot MessageTarget Std_.Word32) where
    variantByLabel  = (GH.Variant (GH.dataField 0 0 32 0) 0)
instance (GH.HasVariant "promisedAnswer" GH.Slot MessageTarget PromisedAnswer) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 1)
data instance C.Parsed (GH.Which MessageTarget)
    = MessageTarget'importedCap (RP.Parsed Std_.Word32)
    | MessageTarget'promisedAnswer (RP.Parsed PromisedAnswer)
    | MessageTarget'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which MessageTarget)))
deriving instance (Std_.Eq (C.Parsed (GH.Which MessageTarget)))
instance (C.Parse (GH.Which MessageTarget) (C.Parsed (GH.Which MessageTarget))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_MessageTarget'importedCap rawArg_) ->
                (MessageTarget'importedCap <$> (C.parse rawArg_))
            (RW_MessageTarget'promisedAnswer rawArg_) ->
                (MessageTarget'promisedAnswer <$> (C.parse rawArg_))
            (RW_MessageTarget'unknown' tag_) ->
                (Std_.pure (MessageTarget'unknown' tag_))
        )
instance (C.Marshal (GH.Which MessageTarget) (C.Parsed (GH.Which MessageTarget))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (MessageTarget'importedCap arg_) ->
            (GH.encodeVariant #importedCap arg_ (GH.unionStruct raw_))
        (MessageTarget'promisedAnswer arg_) ->
            (GH.encodeVariant #promisedAnswer arg_ (GH.unionStruct raw_))
        (MessageTarget'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Payload 
type instance (R.ReprFor Payload) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Payload) where
    typeId  = 11100916931204903995
instance (C.TypedStruct Payload) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Payload) where
    type AllocHint Payload = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Payload (C.Parsed Payload))
instance (C.AllocateList Payload) where
    type ListAllocHint Payload = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Payload (C.Parsed Payload))
data instance C.Parsed Payload
    = Payload 
        {content :: (RP.Parsed (Std_.Maybe Basics.AnyPointer))
        ,capTable :: (RP.Parsed (R.List CapDescriptor))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Payload))
deriving instance (Std_.Eq (C.Parsed Payload))
instance (C.Parse Payload (C.Parsed Payload)) where
    parse raw_ = (Payload <$> (GH.parseField #content raw_)
                          <*> (GH.parseField #capTable raw_))
instance (C.Marshal Payload (C.Parsed Payload)) where
    marshalInto raw_ Payload{..} = (do
        (GH.encodeField #content content raw_)
        (GH.encodeField #capTable capTable raw_)
        (Std_.pure ())
        )
instance (GH.HasField "content" GH.Slot Payload (Std_.Maybe Basics.AnyPointer)) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "capTable" GH.Slot Payload (R.List CapDescriptor)) where
    fieldByLabel  = (GH.ptrField 1)
data CapDescriptor 
type instance (R.ReprFor CapDescriptor) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CapDescriptor) where
    typeId  = 9593755465305995440
instance (C.TypedStruct CapDescriptor) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate CapDescriptor) where
    type AllocHint CapDescriptor = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CapDescriptor (C.Parsed CapDescriptor))
instance (C.AllocateList CapDescriptor) where
    type ListAllocHint CapDescriptor = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CapDescriptor (C.Parsed CapDescriptor))
data instance C.Parsed CapDescriptor
    = CapDescriptor 
        {attachedFd :: (RP.Parsed Std_.Word8)
        ,union' :: (C.Parsed (GH.Which CapDescriptor))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CapDescriptor))
deriving instance (Std_.Eq (C.Parsed CapDescriptor))
instance (C.Parse CapDescriptor (C.Parsed CapDescriptor)) where
    parse raw_ = (CapDescriptor <$> (GH.parseField #attachedFd raw_)
                                <*> (C.parse (GH.structUnion raw_)))
instance (C.Marshal CapDescriptor (C.Parsed CapDescriptor)) where
    marshalInto raw_ CapDescriptor{..} = (do
        (GH.encodeField #attachedFd attachedFd raw_)
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion CapDescriptor) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ CapDescriptor
        = RW_CapDescriptor'none (R.Raw mut_ ())
        | RW_CapDescriptor'senderHosted (R.Raw mut_ Std_.Word32)
        | RW_CapDescriptor'senderPromise (R.Raw mut_ Std_.Word32)
        | RW_CapDescriptor'receiverHosted (R.Raw mut_ Std_.Word32)
        | RW_CapDescriptor'receiverAnswer (R.Raw mut_ PromisedAnswer)
        | RW_CapDescriptor'thirdPartyHosted (R.Raw mut_ ThirdPartyCapDescriptor)
        | RW_CapDescriptor'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_CapDescriptor'none <$> (GH.readVariant #none struct_))
        1 ->
            (RW_CapDescriptor'senderHosted <$> (GH.readVariant #senderHosted struct_))
        2 ->
            (RW_CapDescriptor'senderPromise <$> (GH.readVariant #senderPromise struct_))
        3 ->
            (RW_CapDescriptor'receiverHosted <$> (GH.readVariant #receiverHosted struct_))
        4 ->
            (RW_CapDescriptor'receiverAnswer <$> (GH.readVariant #receiverAnswer struct_))
        5 ->
            (RW_CapDescriptor'thirdPartyHosted <$> (GH.readVariant #thirdPartyHosted struct_))
        _ ->
            (Std_.pure (RW_CapDescriptor'unknown' tag_))
    data Which CapDescriptor
instance (GH.HasVariant "none" GH.Slot CapDescriptor ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance (GH.HasVariant "senderHosted" GH.Slot CapDescriptor Std_.Word32) where
    variantByLabel  = (GH.Variant (GH.dataField 32 0 32 0) 1)
instance (GH.HasVariant "senderPromise" GH.Slot CapDescriptor Std_.Word32) where
    variantByLabel  = (GH.Variant (GH.dataField 32 0 32 0) 2)
instance (GH.HasVariant "receiverHosted" GH.Slot CapDescriptor Std_.Word32) where
    variantByLabel  = (GH.Variant (GH.dataField 32 0 32 0) 3)
instance (GH.HasVariant "receiverAnswer" GH.Slot CapDescriptor PromisedAnswer) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 4)
instance (GH.HasVariant "thirdPartyHosted" GH.Slot CapDescriptor ThirdPartyCapDescriptor) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 5)
data instance C.Parsed (GH.Which CapDescriptor)
    = CapDescriptor'none 
    | CapDescriptor'senderHosted (RP.Parsed Std_.Word32)
    | CapDescriptor'senderPromise (RP.Parsed Std_.Word32)
    | CapDescriptor'receiverHosted (RP.Parsed Std_.Word32)
    | CapDescriptor'receiverAnswer (RP.Parsed PromisedAnswer)
    | CapDescriptor'thirdPartyHosted (RP.Parsed ThirdPartyCapDescriptor)
    | CapDescriptor'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which CapDescriptor)))
deriving instance (Std_.Eq (C.Parsed (GH.Which CapDescriptor)))
instance (C.Parse (GH.Which CapDescriptor) (C.Parsed (GH.Which CapDescriptor))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_CapDescriptor'none _) ->
                (Std_.pure CapDescriptor'none)
            (RW_CapDescriptor'senderHosted rawArg_) ->
                (CapDescriptor'senderHosted <$> (C.parse rawArg_))
            (RW_CapDescriptor'senderPromise rawArg_) ->
                (CapDescriptor'senderPromise <$> (C.parse rawArg_))
            (RW_CapDescriptor'receiverHosted rawArg_) ->
                (CapDescriptor'receiverHosted <$> (C.parse rawArg_))
            (RW_CapDescriptor'receiverAnswer rawArg_) ->
                (CapDescriptor'receiverAnswer <$> (C.parse rawArg_))
            (RW_CapDescriptor'thirdPartyHosted rawArg_) ->
                (CapDescriptor'thirdPartyHosted <$> (C.parse rawArg_))
            (RW_CapDescriptor'unknown' tag_) ->
                (Std_.pure (CapDescriptor'unknown' tag_))
        )
instance (C.Marshal (GH.Which CapDescriptor) (C.Parsed (GH.Which CapDescriptor))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (CapDescriptor'none) ->
            (GH.encodeVariant #none () (GH.unionStruct raw_))
        (CapDescriptor'senderHosted arg_) ->
            (GH.encodeVariant #senderHosted arg_ (GH.unionStruct raw_))
        (CapDescriptor'senderPromise arg_) ->
            (GH.encodeVariant #senderPromise arg_ (GH.unionStruct raw_))
        (CapDescriptor'receiverHosted arg_) ->
            (GH.encodeVariant #receiverHosted arg_ (GH.unionStruct raw_))
        (CapDescriptor'receiverAnswer arg_) ->
            (GH.encodeVariant #receiverAnswer arg_ (GH.unionStruct raw_))
        (CapDescriptor'thirdPartyHosted arg_) ->
            (GH.encodeVariant #thirdPartyHosted arg_ (GH.unionStruct raw_))
        (CapDescriptor'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
instance (GH.HasField "attachedFd" GH.Slot CapDescriptor Std_.Word8) where
    fieldByLabel  = (GH.dataField 16 0 8 255)
data PromisedAnswer 
type instance (R.ReprFor PromisedAnswer) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId PromisedAnswer) where
    typeId  = 15564635848320162976
instance (C.TypedStruct PromisedAnswer) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate PromisedAnswer) where
    type AllocHint PromisedAnswer = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc PromisedAnswer (C.Parsed PromisedAnswer))
instance (C.AllocateList PromisedAnswer) where
    type ListAllocHint PromisedAnswer = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc PromisedAnswer (C.Parsed PromisedAnswer))
data instance C.Parsed PromisedAnswer
    = PromisedAnswer 
        {questionId :: (RP.Parsed Std_.Word32)
        ,transform :: (RP.Parsed (R.List PromisedAnswer'Op))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed PromisedAnswer))
deriving instance (Std_.Eq (C.Parsed PromisedAnswer))
instance (C.Parse PromisedAnswer (C.Parsed PromisedAnswer)) where
    parse raw_ = (PromisedAnswer <$> (GH.parseField #questionId raw_)
                                 <*> (GH.parseField #transform raw_))
instance (C.Marshal PromisedAnswer (C.Parsed PromisedAnswer)) where
    marshalInto raw_ PromisedAnswer{..} = (do
        (GH.encodeField #questionId questionId raw_)
        (GH.encodeField #transform transform raw_)
        (Std_.pure ())
        )
instance (GH.HasField "questionId" GH.Slot PromisedAnswer Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "transform" GH.Slot PromisedAnswer (R.List PromisedAnswer'Op)) where
    fieldByLabel  = (GH.ptrField 0)
data PromisedAnswer'Op 
type instance (R.ReprFor PromisedAnswer'Op) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId PromisedAnswer'Op) where
    typeId  = 17516350820840804481
instance (C.TypedStruct PromisedAnswer'Op) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate PromisedAnswer'Op) where
    type AllocHint PromisedAnswer'Op = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc PromisedAnswer'Op (C.Parsed PromisedAnswer'Op))
instance (C.AllocateList PromisedAnswer'Op) where
    type ListAllocHint PromisedAnswer'Op = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc PromisedAnswer'Op (C.Parsed PromisedAnswer'Op))
data instance C.Parsed PromisedAnswer'Op
    = PromisedAnswer'Op 
        {union' :: (C.Parsed (GH.Which PromisedAnswer'Op))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed PromisedAnswer'Op))
deriving instance (Std_.Eq (C.Parsed PromisedAnswer'Op))
instance (C.Parse PromisedAnswer'Op (C.Parsed PromisedAnswer'Op)) where
    parse raw_ = (PromisedAnswer'Op <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal PromisedAnswer'Op (C.Parsed PromisedAnswer'Op)) where
    marshalInto raw_ PromisedAnswer'Op{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion PromisedAnswer'Op) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ PromisedAnswer'Op
        = RW_PromisedAnswer'Op'noop (R.Raw mut_ ())
        | RW_PromisedAnswer'Op'getPointerField (R.Raw mut_ Std_.Word16)
        | RW_PromisedAnswer'Op'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_PromisedAnswer'Op'noop <$> (GH.readVariant #noop struct_))
        1 ->
            (RW_PromisedAnswer'Op'getPointerField <$> (GH.readVariant #getPointerField struct_))
        _ ->
            (Std_.pure (RW_PromisedAnswer'Op'unknown' tag_))
    data Which PromisedAnswer'Op
instance (GH.HasVariant "noop" GH.Slot PromisedAnswer'Op ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance (GH.HasVariant "getPointerField" GH.Slot PromisedAnswer'Op Std_.Word16) where
    variantByLabel  = (GH.Variant (GH.dataField 16 0 16 0) 1)
data instance C.Parsed (GH.Which PromisedAnswer'Op)
    = PromisedAnswer'Op'noop 
    | PromisedAnswer'Op'getPointerField (RP.Parsed Std_.Word16)
    | PromisedAnswer'Op'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which PromisedAnswer'Op)))
deriving instance (Std_.Eq (C.Parsed (GH.Which PromisedAnswer'Op)))
instance (C.Parse (GH.Which PromisedAnswer'Op) (C.Parsed (GH.Which PromisedAnswer'Op))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_PromisedAnswer'Op'noop _) ->
                (Std_.pure PromisedAnswer'Op'noop)
            (RW_PromisedAnswer'Op'getPointerField rawArg_) ->
                (PromisedAnswer'Op'getPointerField <$> (C.parse rawArg_))
            (RW_PromisedAnswer'Op'unknown' tag_) ->
                (Std_.pure (PromisedAnswer'Op'unknown' tag_))
        )
instance (C.Marshal (GH.Which PromisedAnswer'Op) (C.Parsed (GH.Which PromisedAnswer'Op))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (PromisedAnswer'Op'noop) ->
            (GH.encodeVariant #noop () (GH.unionStruct raw_))
        (PromisedAnswer'Op'getPointerField arg_) ->
            (GH.encodeVariant #getPointerField arg_ (GH.unionStruct raw_))
        (PromisedAnswer'Op'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data ThirdPartyCapDescriptor 
type instance (R.ReprFor ThirdPartyCapDescriptor) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId ThirdPartyCapDescriptor) where
    typeId  = 15235686326393111165
instance (C.TypedStruct ThirdPartyCapDescriptor) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate ThirdPartyCapDescriptor) where
    type AllocHint ThirdPartyCapDescriptor = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc ThirdPartyCapDescriptor (C.Parsed ThirdPartyCapDescriptor))
instance (C.AllocateList ThirdPartyCapDescriptor) where
    type ListAllocHint ThirdPartyCapDescriptor = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc ThirdPartyCapDescriptor (C.Parsed ThirdPartyCapDescriptor))
data instance C.Parsed ThirdPartyCapDescriptor
    = ThirdPartyCapDescriptor 
        {id :: (RP.Parsed (Std_.Maybe Basics.AnyPointer))
        ,vineId :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed ThirdPartyCapDescriptor))
deriving instance (Std_.Eq (C.Parsed ThirdPartyCapDescriptor))
instance (C.Parse ThirdPartyCapDescriptor (C.Parsed ThirdPartyCapDescriptor)) where
    parse raw_ = (ThirdPartyCapDescriptor <$> (GH.parseField #id raw_)
                                          <*> (GH.parseField #vineId raw_))
instance (C.Marshal ThirdPartyCapDescriptor (C.Parsed ThirdPartyCapDescriptor)) where
    marshalInto raw_ ThirdPartyCapDescriptor{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #vineId vineId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot ThirdPartyCapDescriptor (Std_.Maybe Basics.AnyPointer)) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "vineId" GH.Slot ThirdPartyCapDescriptor Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
data Exception 
type instance (R.ReprFor Exception) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Exception) where
    typeId  = 15430940935639230746
instance (C.TypedStruct Exception) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Exception) where
    type AllocHint Exception = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Exception (C.Parsed Exception))
instance (C.AllocateList Exception) where
    type ListAllocHint Exception = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Exception (C.Parsed Exception))
data instance C.Parsed Exception
    = Exception 
        {reason :: (RP.Parsed Basics.Text)
        ,obsoleteIsCallersFault :: (RP.Parsed Std_.Bool)
        ,obsoleteDurability :: (RP.Parsed Std_.Word16)
        ,type_ :: (RP.Parsed Exception'Type)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Exception))
deriving instance (Std_.Eq (C.Parsed Exception))
instance (C.Parse Exception (C.Parsed Exception)) where
    parse raw_ = (Exception <$> (GH.parseField #reason raw_)
                            <*> (GH.parseField #obsoleteIsCallersFault raw_)
                            <*> (GH.parseField #obsoleteDurability raw_)
                            <*> (GH.parseField #type_ raw_))
instance (C.Marshal Exception (C.Parsed Exception)) where
    marshalInto raw_ Exception{..} = (do
        (GH.encodeField #reason reason raw_)
        (GH.encodeField #obsoleteIsCallersFault obsoleteIsCallersFault raw_)
        (GH.encodeField #obsoleteDurability obsoleteDurability raw_)
        (GH.encodeField #type_ type_ raw_)
        (Std_.pure ())
        )
instance (GH.HasField "reason" GH.Slot Exception Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "obsoleteIsCallersFault" GH.Slot Exception Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "obsoleteDurability" GH.Slot Exception Std_.Word16) where
    fieldByLabel  = (GH.dataField 16 0 16 0)
instance (GH.HasField "type_" GH.Slot Exception Exception'Type) where
    fieldByLabel  = (GH.dataField 32 0 16 0)
data Exception'Type 
    = Exception'Type'failed 
    | Exception'Type'overloaded 
    | Exception'Type'disconnected 
    | Exception'Type'unimplemented 
    | Exception'Type'unknown' Std_.Word16
    deriving(Std_.Eq,Std_.Show)
type instance (R.ReprFor Exception'Type) = (R.Data R.Sz16)
instance (C.HasTypeId Exception'Type) where
    typeId  = 12865824133959433560
instance (Std_.Enum Exception'Type) where
    toEnum n_ = case n_ of
        0 ->
            Exception'Type'failed
        1 ->
            Exception'Type'overloaded
        2 ->
            Exception'Type'disconnected
        3 ->
            Exception'Type'unimplemented
        tag_ ->
            (Exception'Type'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (Exception'Type'failed) ->
            0
        (Exception'Type'overloaded) ->
            1
        (Exception'Type'disconnected) ->
            2
        (Exception'Type'unimplemented) ->
            3
        (Exception'Type'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord Exception'Type) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse Exception'Type Exception'Type) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList Exception'Type) where
    type ListAllocHint Exception'Type = Std_.Int
instance (C.EstimateListAlloc Exception'Type Exception'Type)