{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Capnp.Gen.Capnp.RpcTwoparty.Pure(Capnp.Gen.ById.Xa184c7885cdaf2a1.Side(..)
                                       ,VatId(..)
                                       ,ProvisionId(..)
                                       ,RecipientId(..)
                                       ,ThirdPartyCapId(..)
                                       ,JoinKeyPart(..)
                                       ,JoinResult(..)) where
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Default as Default
import qualified GHC.Generics as Generics
import qualified Control.Monad.IO.Class as MonadIO
import qualified Capnp.Untyped.Pure as UntypedPure
import qualified Capnp.Untyped as Untyped
import qualified Capnp.Message as Message
import qualified Capnp.Classes as Classes
import qualified Capnp.Basics.Pure as BasicsPure
import qualified Capnp.GenHelpers.Pure as GenHelpersPure
import qualified Capnp.Gen.ById.Xa184c7885cdaf2a1
import qualified Capnp.Gen.ById.Xbdf87d7bb8304e81
import qualified Capnp.Gen.ById.Xbdf87d7bb8304e81.Pure
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data VatId 
    = VatId 
        {side :: Capnp.Gen.ById.Xa184c7885cdaf2a1.Side}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default VatId) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg VatId) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize VatId) where
    type Cerial msg VatId = (Capnp.Gen.ById.Xa184c7885cdaf2a1.VatId msg)
    decerialize raw = (VatId <$> (Capnp.Gen.ById.Xa184c7885cdaf2a1.get_VatId'side raw))
instance (Classes.Marshal VatId) where
    marshalInto raw_ value_ = case value_ of
        VatId{..} ->
            (do
                (Capnp.Gen.ById.Xa184c7885cdaf2a1.set_VatId'side raw_ side)
                (Std_.pure ())
                )
instance (Classes.Cerialize VatId)
instance (Classes.Cerialize (V.Vector VatId)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector VatId))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector VatId)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector VatId))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector VatId)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector VatId))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector VatId)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data ProvisionId 
    = ProvisionId 
        {joinId :: Std_.Word32}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default ProvisionId) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg ProvisionId) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize ProvisionId) where
    type Cerial msg ProvisionId = (Capnp.Gen.ById.Xa184c7885cdaf2a1.ProvisionId msg)
    decerialize raw = (ProvisionId <$> (Capnp.Gen.ById.Xa184c7885cdaf2a1.get_ProvisionId'joinId raw))
instance (Classes.Marshal ProvisionId) where
    marshalInto raw_ value_ = case value_ of
        ProvisionId{..} ->
            (do
                (Capnp.Gen.ById.Xa184c7885cdaf2a1.set_ProvisionId'joinId raw_ joinId)
                (Std_.pure ())
                )
instance (Classes.Cerialize ProvisionId)
instance (Classes.Cerialize (V.Vector ProvisionId)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector ProvisionId))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector ProvisionId)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector ProvisionId))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ProvisionId)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ProvisionId))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ProvisionId)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data RecipientId 
    = RecipientId 
        {}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default RecipientId) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg RecipientId) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize RecipientId) where
    type Cerial msg RecipientId = (Capnp.Gen.ById.Xa184c7885cdaf2a1.RecipientId msg)
    decerialize raw = (Std_.pure RecipientId)
instance (Classes.Marshal RecipientId) where
    marshalInto raw_ value_ = case value_ of
        (RecipientId) ->
            (Std_.pure ())
instance (Classes.Cerialize RecipientId)
instance (Classes.Cerialize (V.Vector RecipientId)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector RecipientId))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector RecipientId)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector RecipientId))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector RecipientId)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector RecipientId))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector RecipientId)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data ThirdPartyCapId 
    = ThirdPartyCapId 
        {}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default ThirdPartyCapId) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg ThirdPartyCapId) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize ThirdPartyCapId) where
    type Cerial msg ThirdPartyCapId = (Capnp.Gen.ById.Xa184c7885cdaf2a1.ThirdPartyCapId msg)
    decerialize raw = (Std_.pure ThirdPartyCapId)
instance (Classes.Marshal ThirdPartyCapId) where
    marshalInto raw_ value_ = case value_ of
        (ThirdPartyCapId) ->
            (Std_.pure ())
instance (Classes.Cerialize ThirdPartyCapId)
instance (Classes.Cerialize (V.Vector ThirdPartyCapId)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector ThirdPartyCapId))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector ThirdPartyCapId)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector ThirdPartyCapId))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ThirdPartyCapId)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ThirdPartyCapId))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ThirdPartyCapId)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data JoinKeyPart 
    = JoinKeyPart 
        {joinId :: Std_.Word32
        ,partCount :: Std_.Word16
        ,partNum :: Std_.Word16}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default JoinKeyPart) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg JoinKeyPart) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize JoinKeyPart) where
    type Cerial msg JoinKeyPart = (Capnp.Gen.ById.Xa184c7885cdaf2a1.JoinKeyPart msg)
    decerialize raw = (JoinKeyPart <$> (Capnp.Gen.ById.Xa184c7885cdaf2a1.get_JoinKeyPart'joinId raw)
                                   <*> (Capnp.Gen.ById.Xa184c7885cdaf2a1.get_JoinKeyPart'partCount raw)
                                   <*> (Capnp.Gen.ById.Xa184c7885cdaf2a1.get_JoinKeyPart'partNum raw))
instance (Classes.Marshal JoinKeyPart) where
    marshalInto raw_ value_ = case value_ of
        JoinKeyPart{..} ->
            (do
                (Capnp.Gen.ById.Xa184c7885cdaf2a1.set_JoinKeyPart'joinId raw_ joinId)
                (Capnp.Gen.ById.Xa184c7885cdaf2a1.set_JoinKeyPart'partCount raw_ partCount)
                (Capnp.Gen.ById.Xa184c7885cdaf2a1.set_JoinKeyPart'partNum raw_ partNum)
                (Std_.pure ())
                )
instance (Classes.Cerialize JoinKeyPart)
instance (Classes.Cerialize (V.Vector JoinKeyPart)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector JoinKeyPart))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector JoinKeyPart)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector JoinKeyPart))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JoinKeyPart)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JoinKeyPart))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JoinKeyPart)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data JoinResult 
    = JoinResult 
        {joinId :: Std_.Word32
        ,succeeded :: Std_.Bool
        ,cap :: (Std_.Maybe UntypedPure.Ptr)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default JoinResult) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg JoinResult) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize JoinResult) where
    type Cerial msg JoinResult = (Capnp.Gen.ById.Xa184c7885cdaf2a1.JoinResult msg)
    decerialize raw = (JoinResult <$> (Capnp.Gen.ById.Xa184c7885cdaf2a1.get_JoinResult'joinId raw)
                                  <*> (Capnp.Gen.ById.Xa184c7885cdaf2a1.get_JoinResult'succeeded raw)
                                  <*> ((Capnp.Gen.ById.Xa184c7885cdaf2a1.get_JoinResult'cap raw) >>= Classes.decerialize))
instance (Classes.Marshal JoinResult) where
    marshalInto raw_ value_ = case value_ of
        JoinResult{..} ->
            (do
                (Capnp.Gen.ById.Xa184c7885cdaf2a1.set_JoinResult'joinId raw_ joinId)
                (Capnp.Gen.ById.Xa184c7885cdaf2a1.set_JoinResult'succeeded raw_ succeeded)
                ((Classes.cerialize (Untyped.message raw_) cap) >>= (Capnp.Gen.ById.Xa184c7885cdaf2a1.set_JoinResult'cap raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize JoinResult)
instance (Classes.Cerialize (V.Vector JoinResult)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector JoinResult))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector JoinResult)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector JoinResult))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JoinResult)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JoinResult))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JoinResult)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Decerialize Capnp.Gen.ById.Xa184c7885cdaf2a1.Side) where
    type Cerial msg Capnp.Gen.ById.Xa184c7885cdaf2a1.Side = Capnp.Gen.ById.Xa184c7885cdaf2a1.Side
    decerialize  = Std_.pure
instance (Classes.Cerialize Capnp.Gen.ById.Xa184c7885cdaf2a1.Side) where
    cerialize _ = Std_.pure
instance (Classes.Cerialize (V.Vector Capnp.Gen.ById.Xa184c7885cdaf2a1.Side)) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector Capnp.Gen.ById.Xa184c7885cdaf2a1.Side))) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Capnp.Gen.ById.Xa184c7885cdaf2a1.Side)))) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Capnp.Gen.ById.Xa184c7885cdaf2a1.Side))))) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Capnp.Gen.ById.Xa184c7885cdaf2a1.Side)))))) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Capnp.Gen.ById.Xa184c7885cdaf2a1.Side))))))) where
    cerialize  = Classes.cerializeBasicVec