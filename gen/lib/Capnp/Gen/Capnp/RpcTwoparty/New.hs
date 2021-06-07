{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.RpcTwoparty.New where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.New.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers.New as GH
import qualified Capnp.New.Classes as C
import qualified GHC.Generics as Generics
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Side 
    = Side'server 
    | Side'client 
    | Side'unknown' Std_.Word16
    deriving(Std_.Eq,Std_.Show)
type instance (R.ReprFor Side) = (R.Data R.Sz16)
data VatId 
type instance (R.ReprFor VatId) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct VatId) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate VatId) where
    type AllocHint VatId = ()
    new _ = C.newTypedStruct
data instance C.Parsed VatId
    = VatId 
        {side :: (RP.Parsed Side)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VatId))
deriving instance (Std_.Eq (C.Parsed VatId))
instance (GH.HasField "side" GH.Slot VatId Side) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
data ProvisionId 
type instance (R.ReprFor ProvisionId) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct ProvisionId) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate ProvisionId) where
    type AllocHint ProvisionId = ()
    new _ = C.newTypedStruct
data instance C.Parsed ProvisionId
    = ProvisionId 
        {joinId :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed ProvisionId))
deriving instance (Std_.Eq (C.Parsed ProvisionId))
instance (GH.HasField "joinId" GH.Slot ProvisionId Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
data RecipientId 
type instance (R.ReprFor RecipientId) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct RecipientId) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate RecipientId) where
    type AllocHint RecipientId = ()
    new _ = C.newTypedStruct
data instance C.Parsed RecipientId
    = RecipientId 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed RecipientId))
deriving instance (Std_.Eq (C.Parsed RecipientId))
data ThirdPartyCapId 
type instance (R.ReprFor ThirdPartyCapId) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct ThirdPartyCapId) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate ThirdPartyCapId) where
    type AllocHint ThirdPartyCapId = ()
    new _ = C.newTypedStruct
data instance C.Parsed ThirdPartyCapId
    = ThirdPartyCapId 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed ThirdPartyCapId))
deriving instance (Std_.Eq (C.Parsed ThirdPartyCapId))
data JoinKeyPart 
type instance (R.ReprFor JoinKeyPart) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct JoinKeyPart) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate JoinKeyPart) where
    type AllocHint JoinKeyPart = ()
    new _ = C.newTypedStruct
data instance C.Parsed JoinKeyPart
    = JoinKeyPart 
        {joinId :: (RP.Parsed Std_.Word32)
        ,partCount :: (RP.Parsed Std_.Word16)
        ,partNum :: (RP.Parsed Std_.Word16)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed JoinKeyPart))
deriving instance (Std_.Eq (C.Parsed JoinKeyPart))
instance (GH.HasField "joinId" GH.Slot JoinKeyPart Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "partCount" GH.Slot JoinKeyPart Std_.Word16) where
    fieldByLabel  = (GH.dataField 32 0 16 0)
instance (GH.HasField "partNum" GH.Slot JoinKeyPart Std_.Word16) where
    fieldByLabel  = (GH.dataField 48 0 16 0)
data JoinResult 
type instance (R.ReprFor JoinResult) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct JoinResult) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate JoinResult) where
    type AllocHint JoinResult = ()
    new _ = C.newTypedStruct
data instance C.Parsed JoinResult
    = JoinResult 
        {joinId :: (RP.Parsed Std_.Word32)
        ,succeeded :: (RP.Parsed Std_.Bool)
        ,cap :: (RP.Parsed Basics.AnyPointer)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed JoinResult))
deriving instance (Std_.Eq (C.Parsed JoinResult))
instance (GH.HasField "joinId" GH.Slot JoinResult Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "succeeded" GH.Slot JoinResult Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 0 1 0)
instance (GH.HasField "cap" GH.Slot JoinResult Basics.AnyPointer) where
    fieldByLabel  = (GH.ptrField 0)