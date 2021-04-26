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
module Capnp.Gen.Capnp.RpcTwoparty.New where
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
data Side 
type instance (R.ReprFor Side) = (R.Data R.Sz16)
data VatId 
type instance (R.ReprFor VatId) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct VatId) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate VatId) where
    type AllocHint VatId = ()
    new  = GH.newStruct
instance (OL.IsLabel "side" (F.Field F.Slot VatId Side)) where
    fromLabel  = (GH.dataField 0 0 16 0)
instance (F.HasField "side" F.Slot VatId Side)
data ProvisionId 
type instance (R.ReprFor ProvisionId) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct ProvisionId) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate ProvisionId) where
    type AllocHint ProvisionId = ()
    new  = GH.newStruct
instance (OL.IsLabel "joinId" (F.Field F.Slot ProvisionId Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "joinId" F.Slot ProvisionId Std_.Word32)
data RecipientId 
type instance (R.ReprFor RecipientId) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct RecipientId) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate RecipientId) where
    type AllocHint RecipientId = ()
    new  = GH.newStruct
data ThirdPartyCapId 
type instance (R.ReprFor ThirdPartyCapId) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct ThirdPartyCapId) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate ThirdPartyCapId) where
    type AllocHint ThirdPartyCapId = ()
    new  = GH.newStruct
data JoinKeyPart 
type instance (R.ReprFor JoinKeyPart) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct JoinKeyPart) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate JoinKeyPart) where
    type AllocHint JoinKeyPart = ()
    new  = GH.newStruct
instance (OL.IsLabel "joinId" (F.Field F.Slot JoinKeyPart Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "joinId" F.Slot JoinKeyPart Std_.Word32)
instance (OL.IsLabel "partCount" (F.Field F.Slot JoinKeyPart Std_.Word16)) where
    fromLabel  = (GH.dataField 32 0 16 0)
instance (F.HasField "partCount" F.Slot JoinKeyPart Std_.Word16)
instance (OL.IsLabel "partNum" (F.Field F.Slot JoinKeyPart Std_.Word16)) where
    fromLabel  = (GH.dataField 48 0 16 0)
instance (F.HasField "partNum" F.Slot JoinKeyPart Std_.Word16)
data JoinResult 
type instance (R.ReprFor JoinResult) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct JoinResult) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate JoinResult) where
    type AllocHint JoinResult = ()
    new  = GH.newStruct
instance (OL.IsLabel "joinId" (F.Field F.Slot JoinResult Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "joinId" F.Slot JoinResult Std_.Word32)
instance (OL.IsLabel "succeeded" (F.Field F.Slot JoinResult Std_.Bool)) where
    fromLabel  = (GH.dataField 32 0 1 0)
instance (F.HasField "succeeded" F.Slot JoinResult Std_.Bool)
instance (OL.IsLabel "cap" (F.Field F.Slot JoinResult Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "cap" F.Slot JoinResult Basics.AnyPointer)