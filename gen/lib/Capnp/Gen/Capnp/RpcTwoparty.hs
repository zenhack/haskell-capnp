{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Capnp.Gen.Capnp.RpcTwoparty where
import qualified Capnp.Message as Message
import qualified Capnp.Untyped as Untyped
import qualified Capnp.Basics as Basics
import qualified Capnp.GenHelpers as GenHelpers
import qualified Capnp.Classes as Classes
import qualified GHC.Generics as Generics
import qualified Capnp.Bits as Std_
import qualified Data.Maybe as Std_
import qualified Data.ByteString as BS
import qualified Capnp.Gen.ById.Xbdf87d7bb8304e81
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Side 
    = Side'server 
    | Side'client 
    | Side'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Read
            ,Std_.Eq
            ,Generics.Generic)
instance (Classes.IsWord Side) where
    fromWord n = case ((Std_.fromIntegral n) :: Std_.Word16) of
        0 ->
            Side'server
        1 ->
            Side'client
        tag ->
            (Side'unknown' tag)
    toWord (Side'server) = 0
    toWord (Side'client) = 1
    toWord (Side'unknown' tag) = (Std_.fromIntegral tag)
instance (Std_.Enum Side) where
    fromEnum x = (Std_.fromIntegral (Classes.toWord x))
    toEnum x = (Classes.fromWord (Std_.fromIntegral x))
instance (Basics.ListElem msg Side) where
    newtype List msg Side
        = Side'List_ (Untyped.ListOf msg Std_.Word16)
    index i (Side'List_ l) = (Classes.fromWord <$> (Std_.fromIntegral <$> (Untyped.index i l)))
    listFromPtr msg ptr = (Side'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Side'List_ l) = (Untyped.List16 l)
    length (Side'List_ l) = (Untyped.length l)
instance (Classes.MutListElem s Side) where
    setIndex elt i (Side'List_ l) = (Untyped.setIndex (Std_.fromIntegral (Classes.toWord elt)) i l)
    newList msg size = (Side'List_ <$> (Untyped.allocList16 msg size))
newtype VatId msg
    = VatId'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg VatId) where
    tMsg f (VatId'newtype_ s) = (VatId'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (VatId msg)) where
    fromStruct struct = (Std_.pure (VatId'newtype_ struct))
instance (Classes.ToStruct msg (VatId msg)) where
    toStruct (VatId'newtype_ struct) = struct
instance (Untyped.HasMessage (VatId msg)) where
    type InMessage (VatId msg) = msg
    message (VatId'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (VatId msg)) where
    messageDefault msg = (VatId'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (VatId msg)) where
    fromPtr msg ptr = (VatId'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (VatId (Message.MutMsg s))) where
    toPtr msg (VatId'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (VatId (Message.MutMsg s))) where
    new msg = (VatId'newtype_ <$> (Untyped.allocStruct msg 1 0))
instance (Basics.ListElem msg (VatId msg)) where
    newtype List msg (VatId msg)
        = VatId'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (VatId'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (VatId'List_ l) = (Untyped.ListStruct l)
    length (VatId'List_ l) = (Untyped.length l)
    index i (VatId'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (VatId (Message.MutMsg s))) where
    setIndex (VatId'newtype_ elt) i (VatId'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (VatId'List_ <$> (Untyped.allocCompositeList msg 1 0 len))
get_VatId'side :: ((Untyped.ReadCtx m msg)) => (VatId msg) -> (m Side)
get_VatId'side (VatId'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_VatId'side :: ((Untyped.RWCtx m s)) => (VatId (Message.MutMsg s)) -> Side -> (m ())
set_VatId'side (VatId'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
newtype ProvisionId msg
    = ProvisionId'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg ProvisionId) where
    tMsg f (ProvisionId'newtype_ s) = (ProvisionId'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (ProvisionId msg)) where
    fromStruct struct = (Std_.pure (ProvisionId'newtype_ struct))
instance (Classes.ToStruct msg (ProvisionId msg)) where
    toStruct (ProvisionId'newtype_ struct) = struct
instance (Untyped.HasMessage (ProvisionId msg)) where
    type InMessage (ProvisionId msg) = msg
    message (ProvisionId'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (ProvisionId msg)) where
    messageDefault msg = (ProvisionId'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (ProvisionId msg)) where
    fromPtr msg ptr = (ProvisionId'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (ProvisionId (Message.MutMsg s))) where
    toPtr msg (ProvisionId'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (ProvisionId (Message.MutMsg s))) where
    new msg = (ProvisionId'newtype_ <$> (Untyped.allocStruct msg 1 0))
instance (Basics.ListElem msg (ProvisionId msg)) where
    newtype List msg (ProvisionId msg)
        = ProvisionId'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (ProvisionId'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (ProvisionId'List_ l) = (Untyped.ListStruct l)
    length (ProvisionId'List_ l) = (Untyped.length l)
    index i (ProvisionId'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (ProvisionId (Message.MutMsg s))) where
    setIndex (ProvisionId'newtype_ elt) i (ProvisionId'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (ProvisionId'List_ <$> (Untyped.allocCompositeList msg 1 0 len))
get_ProvisionId'joinId :: ((Untyped.ReadCtx m msg)) => (ProvisionId msg) -> (m Std_.Word32)
get_ProvisionId'joinId (ProvisionId'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_ProvisionId'joinId :: ((Untyped.RWCtx m s)) => (ProvisionId (Message.MutMsg s)) -> Std_.Word32 -> (m ())
set_ProvisionId'joinId (ProvisionId'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
newtype RecipientId msg
    = RecipientId'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg RecipientId) where
    tMsg f (RecipientId'newtype_ s) = (RecipientId'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (RecipientId msg)) where
    fromStruct struct = (Std_.pure (RecipientId'newtype_ struct))
instance (Classes.ToStruct msg (RecipientId msg)) where
    toStruct (RecipientId'newtype_ struct) = struct
instance (Untyped.HasMessage (RecipientId msg)) where
    type InMessage (RecipientId msg) = msg
    message (RecipientId'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (RecipientId msg)) where
    messageDefault msg = (RecipientId'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (RecipientId msg)) where
    fromPtr msg ptr = (RecipientId'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (RecipientId (Message.MutMsg s))) where
    toPtr msg (RecipientId'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (RecipientId (Message.MutMsg s))) where
    new msg = (RecipientId'newtype_ <$> (Untyped.allocStruct msg 0 0))
instance (Basics.ListElem msg (RecipientId msg)) where
    newtype List msg (RecipientId msg)
        = RecipientId'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (RecipientId'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (RecipientId'List_ l) = (Untyped.ListStruct l)
    length (RecipientId'List_ l) = (Untyped.length l)
    index i (RecipientId'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (RecipientId (Message.MutMsg s))) where
    setIndex (RecipientId'newtype_ elt) i (RecipientId'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (RecipientId'List_ <$> (Untyped.allocCompositeList msg 0 0 len))
newtype ThirdPartyCapId msg
    = ThirdPartyCapId'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg ThirdPartyCapId) where
    tMsg f (ThirdPartyCapId'newtype_ s) = (ThirdPartyCapId'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (ThirdPartyCapId msg)) where
    fromStruct struct = (Std_.pure (ThirdPartyCapId'newtype_ struct))
instance (Classes.ToStruct msg (ThirdPartyCapId msg)) where
    toStruct (ThirdPartyCapId'newtype_ struct) = struct
instance (Untyped.HasMessage (ThirdPartyCapId msg)) where
    type InMessage (ThirdPartyCapId msg) = msg
    message (ThirdPartyCapId'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (ThirdPartyCapId msg)) where
    messageDefault msg = (ThirdPartyCapId'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (ThirdPartyCapId msg)) where
    fromPtr msg ptr = (ThirdPartyCapId'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (ThirdPartyCapId (Message.MutMsg s))) where
    toPtr msg (ThirdPartyCapId'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (ThirdPartyCapId (Message.MutMsg s))) where
    new msg = (ThirdPartyCapId'newtype_ <$> (Untyped.allocStruct msg 0 0))
instance (Basics.ListElem msg (ThirdPartyCapId msg)) where
    newtype List msg (ThirdPartyCapId msg)
        = ThirdPartyCapId'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (ThirdPartyCapId'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (ThirdPartyCapId'List_ l) = (Untyped.ListStruct l)
    length (ThirdPartyCapId'List_ l) = (Untyped.length l)
    index i (ThirdPartyCapId'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (ThirdPartyCapId (Message.MutMsg s))) where
    setIndex (ThirdPartyCapId'newtype_ elt) i (ThirdPartyCapId'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (ThirdPartyCapId'List_ <$> (Untyped.allocCompositeList msg 0 0 len))
newtype JoinKeyPart msg
    = JoinKeyPart'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg JoinKeyPart) where
    tMsg f (JoinKeyPart'newtype_ s) = (JoinKeyPart'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (JoinKeyPart msg)) where
    fromStruct struct = (Std_.pure (JoinKeyPart'newtype_ struct))
instance (Classes.ToStruct msg (JoinKeyPart msg)) where
    toStruct (JoinKeyPart'newtype_ struct) = struct
instance (Untyped.HasMessage (JoinKeyPart msg)) where
    type InMessage (JoinKeyPart msg) = msg
    message (JoinKeyPart'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (JoinKeyPart msg)) where
    messageDefault msg = (JoinKeyPart'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (JoinKeyPart msg)) where
    fromPtr msg ptr = (JoinKeyPart'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (JoinKeyPart (Message.MutMsg s))) where
    toPtr msg (JoinKeyPart'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (JoinKeyPart (Message.MutMsg s))) where
    new msg = (JoinKeyPart'newtype_ <$> (Untyped.allocStruct msg 1 0))
instance (Basics.ListElem msg (JoinKeyPart msg)) where
    newtype List msg (JoinKeyPart msg)
        = JoinKeyPart'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (JoinKeyPart'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (JoinKeyPart'List_ l) = (Untyped.ListStruct l)
    length (JoinKeyPart'List_ l) = (Untyped.length l)
    index i (JoinKeyPart'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (JoinKeyPart (Message.MutMsg s))) where
    setIndex (JoinKeyPart'newtype_ elt) i (JoinKeyPart'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (JoinKeyPart'List_ <$> (Untyped.allocCompositeList msg 1 0 len))
get_JoinKeyPart'joinId :: ((Untyped.ReadCtx m msg)) => (JoinKeyPart msg) -> (m Std_.Word32)
get_JoinKeyPart'joinId (JoinKeyPart'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_JoinKeyPart'joinId :: ((Untyped.RWCtx m s)) => (JoinKeyPart (Message.MutMsg s)) -> Std_.Word32 -> (m ())
set_JoinKeyPart'joinId (JoinKeyPart'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
get_JoinKeyPart'partCount :: ((Untyped.ReadCtx m msg)) => (JoinKeyPart msg) -> (m Std_.Word16)
get_JoinKeyPart'partCount (JoinKeyPart'newtype_ struct) = (GenHelpers.getWordField struct 0 32 0)
set_JoinKeyPart'partCount :: ((Untyped.RWCtx m s)) => (JoinKeyPart (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_JoinKeyPart'partCount (JoinKeyPart'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 32 0)
get_JoinKeyPart'partNum :: ((Untyped.ReadCtx m msg)) => (JoinKeyPart msg) -> (m Std_.Word16)
get_JoinKeyPart'partNum (JoinKeyPart'newtype_ struct) = (GenHelpers.getWordField struct 0 48 0)
set_JoinKeyPart'partNum :: ((Untyped.RWCtx m s)) => (JoinKeyPart (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_JoinKeyPart'partNum (JoinKeyPart'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 48 0)
newtype JoinResult msg
    = JoinResult'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg JoinResult) where
    tMsg f (JoinResult'newtype_ s) = (JoinResult'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (JoinResult msg)) where
    fromStruct struct = (Std_.pure (JoinResult'newtype_ struct))
instance (Classes.ToStruct msg (JoinResult msg)) where
    toStruct (JoinResult'newtype_ struct) = struct
instance (Untyped.HasMessage (JoinResult msg)) where
    type InMessage (JoinResult msg) = msg
    message (JoinResult'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (JoinResult msg)) where
    messageDefault msg = (JoinResult'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (JoinResult msg)) where
    fromPtr msg ptr = (JoinResult'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (JoinResult (Message.MutMsg s))) where
    toPtr msg (JoinResult'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (JoinResult (Message.MutMsg s))) where
    new msg = (JoinResult'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem msg (JoinResult msg)) where
    newtype List msg (JoinResult msg)
        = JoinResult'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (JoinResult'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (JoinResult'List_ l) = (Untyped.ListStruct l)
    length (JoinResult'List_ l) = (Untyped.length l)
    index i (JoinResult'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (JoinResult (Message.MutMsg s))) where
    setIndex (JoinResult'newtype_ elt) i (JoinResult'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (JoinResult'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_JoinResult'joinId :: ((Untyped.ReadCtx m msg)) => (JoinResult msg) -> (m Std_.Word32)
get_JoinResult'joinId (JoinResult'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_JoinResult'joinId :: ((Untyped.RWCtx m s)) => (JoinResult (Message.MutMsg s)) -> Std_.Word32 -> (m ())
set_JoinResult'joinId (JoinResult'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
get_JoinResult'succeeded :: ((Untyped.ReadCtx m msg)) => (JoinResult msg) -> (m Std_.Bool)
get_JoinResult'succeeded (JoinResult'newtype_ struct) = (GenHelpers.getWordField struct 0 32 0)
set_JoinResult'succeeded :: ((Untyped.RWCtx m s)) => (JoinResult (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_JoinResult'succeeded (JoinResult'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 0 32 0)
get_JoinResult'cap :: ((Untyped.ReadCtx m msg)) => (JoinResult msg) -> (m (Std_.Maybe (Untyped.Ptr msg)))
get_JoinResult'cap (JoinResult'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_JoinResult'cap :: ((Untyped.RWCtx m s)) => (JoinResult (Message.MutMsg s)) -> (Std_.Maybe (Untyped.Ptr (Message.MutMsg s))) -> (m ())
set_JoinResult'cap (JoinResult'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_JoinResult'cap :: ((Untyped.ReadCtx m msg)) => (JoinResult msg) -> (m Std_.Bool)
has_JoinResult'cap (JoinResult'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))