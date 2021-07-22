{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
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
import qualified Capnp.GenHelpers.ReExports.Data.ByteString as BS
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
instance (Std_.Enum Side) where
    toEnum n_ = case n_ of
        0 ->
            Side'server
        1 ->
            Side'client
        tag_ ->
            (Side'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (Side'server) ->
            0
        (Side'client) ->
            1
        (Side'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord Side) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse Side Side) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList Side) where
    type ListAllocHint Side = Std_.Int
instance (C.EstimateListAlloc Side Side)
data VatId 
type instance (R.ReprFor VatId) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct VatId) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate VatId) where
    type AllocHint VatId = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VatId (C.Parsed VatId))
instance (C.AllocateList VatId) where
    type ListAllocHint VatId = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VatId (C.Parsed VatId))
data instance C.Parsed VatId
    = VatId 
        {side :: (RP.Parsed Side)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VatId))
deriving instance (Std_.Eq (C.Parsed VatId))
instance (C.Parse VatId (C.Parsed VatId)) where
    parse raw_ = (VatId <$> (GH.parseField #side raw_))
instance (C.Marshal VatId (C.Parsed VatId)) where
    marshalInto raw_ VatId{..} = (do
        (GH.encodeField #side side raw_)
        (Std_.pure ())
        )
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
instance (C.EstimateAlloc ProvisionId (C.Parsed ProvisionId))
instance (C.AllocateList ProvisionId) where
    type ListAllocHint ProvisionId = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc ProvisionId (C.Parsed ProvisionId))
data instance C.Parsed ProvisionId
    = ProvisionId 
        {joinId :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed ProvisionId))
deriving instance (Std_.Eq (C.Parsed ProvisionId))
instance (C.Parse ProvisionId (C.Parsed ProvisionId)) where
    parse raw_ = (ProvisionId <$> (GH.parseField #joinId raw_))
instance (C.Marshal ProvisionId (C.Parsed ProvisionId)) where
    marshalInto raw_ ProvisionId{..} = (do
        (GH.encodeField #joinId joinId raw_)
        (Std_.pure ())
        )
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
instance (C.EstimateAlloc RecipientId (C.Parsed RecipientId))
instance (C.AllocateList RecipientId) where
    type ListAllocHint RecipientId = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc RecipientId (C.Parsed RecipientId))
data instance C.Parsed RecipientId
    = RecipientId 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed RecipientId))
deriving instance (Std_.Eq (C.Parsed RecipientId))
instance (C.Parse RecipientId (C.Parsed RecipientId)) where
    parse raw_ = (Std_.pure RecipientId)
instance (C.Marshal RecipientId (C.Parsed RecipientId)) where
    marshalInto _raw (RecipientId) = (Std_.pure ())
data ThirdPartyCapId 
type instance (R.ReprFor ThirdPartyCapId) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct ThirdPartyCapId) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate ThirdPartyCapId) where
    type AllocHint ThirdPartyCapId = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc ThirdPartyCapId (C.Parsed ThirdPartyCapId))
instance (C.AllocateList ThirdPartyCapId) where
    type ListAllocHint ThirdPartyCapId = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc ThirdPartyCapId (C.Parsed ThirdPartyCapId))
data instance C.Parsed ThirdPartyCapId
    = ThirdPartyCapId 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed ThirdPartyCapId))
deriving instance (Std_.Eq (C.Parsed ThirdPartyCapId))
instance (C.Parse ThirdPartyCapId (C.Parsed ThirdPartyCapId)) where
    parse raw_ = (Std_.pure ThirdPartyCapId)
instance (C.Marshal ThirdPartyCapId (C.Parsed ThirdPartyCapId)) where
    marshalInto _raw (ThirdPartyCapId) = (Std_.pure ())
data JoinKeyPart 
type instance (R.ReprFor JoinKeyPart) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct JoinKeyPart) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate JoinKeyPart) where
    type AllocHint JoinKeyPart = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc JoinKeyPart (C.Parsed JoinKeyPart))
instance (C.AllocateList JoinKeyPart) where
    type ListAllocHint JoinKeyPart = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc JoinKeyPart (C.Parsed JoinKeyPart))
data instance C.Parsed JoinKeyPart
    = JoinKeyPart 
        {joinId :: (RP.Parsed Std_.Word32)
        ,partCount :: (RP.Parsed Std_.Word16)
        ,partNum :: (RP.Parsed Std_.Word16)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed JoinKeyPart))
deriving instance (Std_.Eq (C.Parsed JoinKeyPart))
instance (C.Parse JoinKeyPart (C.Parsed JoinKeyPart)) where
    parse raw_ = (JoinKeyPart <$> (GH.parseField #joinId raw_)
                              <*> (GH.parseField #partCount raw_)
                              <*> (GH.parseField #partNum raw_))
instance (C.Marshal JoinKeyPart (C.Parsed JoinKeyPart)) where
    marshalInto raw_ JoinKeyPart{..} = (do
        (GH.encodeField #joinId joinId raw_)
        (GH.encodeField #partCount partCount raw_)
        (GH.encodeField #partNum partNum raw_)
        (Std_.pure ())
        )
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
instance (C.EstimateAlloc JoinResult (C.Parsed JoinResult))
instance (C.AllocateList JoinResult) where
    type ListAllocHint JoinResult = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc JoinResult (C.Parsed JoinResult))
data instance C.Parsed JoinResult
    = JoinResult 
        {joinId :: (RP.Parsed Std_.Word32)
        ,succeeded :: (RP.Parsed Std_.Bool)
        ,cap :: (RP.Parsed Basics.AnyPointer)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed JoinResult))
deriving instance (Std_.Eq (C.Parsed JoinResult))
instance (C.Parse JoinResult (C.Parsed JoinResult)) where
    parse raw_ = (JoinResult <$> (GH.parseField #joinId raw_)
                             <*> (GH.parseField #succeeded raw_)
                             <*> (GH.parseField #cap raw_))
instance (C.Marshal JoinResult (C.Parsed JoinResult)) where
    marshalInto raw_ JoinResult{..} = (do
        (GH.encodeField #joinId joinId raw_)
        (GH.encodeField #succeeded succeeded raw_)
        (GH.encodeField #cap cap raw_)
        (Std_.pure ())
        )
instance (GH.HasField "joinId" GH.Slot JoinResult Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "succeeded" GH.Slot JoinResult Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 0 1 0)
instance (GH.HasField "cap" GH.Slot JoinResult Basics.AnyPointer) where
    fieldByLabel  = (GH.ptrField 0)