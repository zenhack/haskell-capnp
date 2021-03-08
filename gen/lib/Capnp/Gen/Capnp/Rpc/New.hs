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
data Return 
type instance (R.ReprFor Return) = (R.Ptr (Std_.Just R.Struct))
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