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
instance (OL.IsLabel "questionId" (F.Field (Bootstrap) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" (Bootstrap) Std_.Word32)
instance (OL.IsLabel "deprecatedObjectId" (F.Field (Bootstrap) Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "deprecatedObjectId" (Bootstrap) Basics.AnyPointer)
data Call 
type instance (R.ReprFor Call) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "questionId" (F.Field (Call) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" (Call) Std_.Word32)
instance (OL.IsLabel "target" (F.Field (Call) MessageTarget)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "target" (Call) MessageTarget)
instance (OL.IsLabel "interfaceId" (F.Field (Call) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 1 64 0)
instance (F.HasField "interfaceId" (Call) Std_.Word64)
instance (OL.IsLabel "methodId" (F.Field (Call) Std_.Word16)) where
    fromLabel  = (GH.dataField 32 0 16 0)
instance (F.HasField "methodId" (Call) Std_.Word16)
instance (OL.IsLabel "params" (F.Field (Call) Payload)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "params" (Call) Payload)
instance (OL.IsLabel "sendResultsTo" (F.Field (Call) Call'sendResultsTo)) where
    fromLabel  = Std_.undefined
instance (F.HasField "sendResultsTo" (Call) Call'sendResultsTo)
instance (OL.IsLabel "allowThirdPartyTailCall" (F.Field (Call) Std_.Bool)) where
    fromLabel  = (GH.dataField 0 2 1 0)
instance (F.HasField "allowThirdPartyTailCall" (Call) Std_.Bool)
data Call'sendResultsTo 
type instance (R.ReprFor Call'sendResultsTo) = (R.Ptr (Std_.Just R.Struct))
data Return 
type instance (R.ReprFor Return) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "answerId" (F.Field (Return) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "answerId" (Return) Std_.Word32)
instance (OL.IsLabel "releaseParamCaps" (F.Field (Return) Std_.Bool)) where
    fromLabel  = (GH.dataField 32 0 1 1)
instance (F.HasField "releaseParamCaps" (Return) Std_.Bool)
data Finish 
type instance (R.ReprFor Finish) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "questionId" (F.Field (Finish) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" (Finish) Std_.Word32)
instance (OL.IsLabel "releaseResultCaps" (F.Field (Finish) Std_.Bool)) where
    fromLabel  = (GH.dataField 32 0 1 1)
instance (F.HasField "releaseResultCaps" (Finish) Std_.Bool)
data Resolve 
type instance (R.ReprFor Resolve) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "promiseId" (F.Field (Resolve) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "promiseId" (Resolve) Std_.Word32)
data Release 
type instance (R.ReprFor Release) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field (Release) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "id" (Release) Std_.Word32)
instance (OL.IsLabel "referenceCount" (F.Field (Release) Std_.Word32)) where
    fromLabel  = (GH.dataField 32 0 32 0)
instance (F.HasField "referenceCount" (Release) Std_.Word32)
data Disembargo 
type instance (R.ReprFor Disembargo) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "target" (F.Field (Disembargo) MessageTarget)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "target" (Disembargo) MessageTarget)
instance (OL.IsLabel "context" (F.Field (Disembargo) Disembargo'context)) where
    fromLabel  = Std_.undefined
instance (F.HasField "context" (Disembargo) Disembargo'context)
data Disembargo'context 
type instance (R.ReprFor Disembargo'context) = (R.Ptr (Std_.Just R.Struct))
data Provide 
type instance (R.ReprFor Provide) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "questionId" (F.Field (Provide) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" (Provide) Std_.Word32)
instance (OL.IsLabel "target" (F.Field (Provide) MessageTarget)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "target" (Provide) MessageTarget)
instance (OL.IsLabel "recipient" (F.Field (Provide) Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "recipient" (Provide) Basics.AnyPointer)
data Accept 
type instance (R.ReprFor Accept) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "questionId" (F.Field (Accept) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" (Accept) Std_.Word32)
instance (OL.IsLabel "provision" (F.Field (Accept) Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "provision" (Accept) Basics.AnyPointer)
instance (OL.IsLabel "embargo" (F.Field (Accept) Std_.Bool)) where
    fromLabel  = (GH.dataField 32 0 1 0)
instance (F.HasField "embargo" (Accept) Std_.Bool)
data Join 
type instance (R.ReprFor Join) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "questionId" (F.Field (Join) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" (Join) Std_.Word32)
instance (OL.IsLabel "target" (F.Field (Join) MessageTarget)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "target" (Join) MessageTarget)
instance (OL.IsLabel "keyPart" (F.Field (Join) Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "keyPart" (Join) Basics.AnyPointer)
data MessageTarget 
type instance (R.ReprFor MessageTarget) = (R.Ptr (Std_.Just R.Struct))
data Payload 
type instance (R.ReprFor Payload) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "content" (F.Field (Payload) Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "content" (Payload) Basics.AnyPointer)
instance (OL.IsLabel "capTable" (F.Field (Payload) (R.List CapDescriptor))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "capTable" (Payload) (R.List CapDescriptor))
data CapDescriptor 
type instance (R.ReprFor CapDescriptor) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "attachedFd" (F.Field (CapDescriptor) Std_.Word8)) where
    fromLabel  = (GH.dataField 16 0 8 255)
instance (F.HasField "attachedFd" (CapDescriptor) Std_.Word8)
data PromisedAnswer 
type instance (R.ReprFor PromisedAnswer) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "questionId" (F.Field (PromisedAnswer) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "questionId" (PromisedAnswer) Std_.Word32)
instance (OL.IsLabel "transform" (F.Field (PromisedAnswer) (R.List PromisedAnswer'Op))) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "transform" (PromisedAnswer) (R.List PromisedAnswer'Op))
data PromisedAnswer'Op 
type instance (R.ReprFor PromisedAnswer'Op) = (R.Ptr (Std_.Just R.Struct))
data ThirdPartyCapDescriptor 
type instance (R.ReprFor ThirdPartyCapDescriptor) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field (ThirdPartyCapDescriptor) Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "id" (ThirdPartyCapDescriptor) Basics.AnyPointer)
instance (OL.IsLabel "vineId" (F.Field (ThirdPartyCapDescriptor) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "vineId" (ThirdPartyCapDescriptor) Std_.Word32)
data Exception 
type instance (R.ReprFor Exception) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "reason" (F.Field (Exception) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "reason" (Exception) Basics.Text)
instance (OL.IsLabel "obsoleteIsCallersFault" (F.Field (Exception) Std_.Bool)) where
    fromLabel  = (GH.dataField 0 0 1 0)
instance (F.HasField "obsoleteIsCallersFault" (Exception) Std_.Bool)
instance (OL.IsLabel "obsoleteDurability" (F.Field (Exception) Std_.Word16)) where
    fromLabel  = (GH.dataField 16 0 16 0)
instance (F.HasField "obsoleteDurability" (Exception) Std_.Word16)
instance (OL.IsLabel "type_" (F.Field (Exception) Exception'Type)) where
    fromLabel  = (GH.dataField 32 0 16 0)
instance (F.HasField "type_" (Exception) Exception'Type)
data Exception'Type 
type instance (R.ReprFor Exception'Type) = (R.Data R.Sz16)