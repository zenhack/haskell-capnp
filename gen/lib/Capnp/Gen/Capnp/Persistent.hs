{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Capnp.Gen.Capnp.Persistent where
import qualified Capnp.Message as Message
import qualified Capnp.Untyped as Untyped
import qualified Capnp.Basics as Basics
import qualified Capnp.GenHelpers as GenHelpers
import qualified Capnp.Classes as Classes
import qualified GHC.Generics as Generics
import qualified Capnp.Bits as Std_
import qualified Data.Maybe as Std_
import qualified Capnp.GenHelpers.ReExports.Data.ByteString as BS
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
newtype Persistent sturdyRef owner msg
    = Persistent'newtype_ (Std_.Maybe (Untyped.Cap msg))
instance (Classes.FromPtr msg (Persistent sturdyRef owner msg)) where
    fromPtr msg ptr = (Persistent'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Persistent sturdyRef owner (Message.MutMsg s))) where
    toPtr msg (Persistent'newtype_ (Std_.Nothing)) = (Std_.pure Std_.Nothing)
    toPtr msg (Persistent'newtype_ (Std_.Just cap)) = (Std_.pure (Std_.Just (Untyped.PtrCap cap)))
newtype Persistent'SaveParams sturdyRef owner msg
    = Persistent'SaveParams'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Persistent'SaveParams sturdyRef owner msg)) where
    fromStruct struct = (Std_.pure (Persistent'SaveParams'newtype_ struct))
instance (Classes.ToStruct msg (Persistent'SaveParams sturdyRef owner msg)) where
    toStruct (Persistent'SaveParams'newtype_ struct) = struct
instance (Untyped.HasMessage (Persistent'SaveParams sturdyRef owner msg)) where
    type InMessage (Persistent'SaveParams sturdyRef owner msg) = msg
    message (Persistent'SaveParams'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Persistent'SaveParams sturdyRef owner msg)) where
    messageDefault msg = (Persistent'SaveParams'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Persistent'SaveParams sturdyRef owner msg)) where
    fromPtr msg ptr = (Persistent'SaveParams'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Persistent'SaveParams sturdyRef owner (Message.MutMsg s))) where
    toPtr msg (Persistent'SaveParams'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Persistent'SaveParams sturdyRef owner (Message.MutMsg s))) where
    new msg = (Persistent'SaveParams'newtype_ <$> (Untyped.allocStruct msg 0 1))
instance (Basics.ListElem msg (Persistent'SaveParams sturdyRef owner msg)) where
    newtype List msg (Persistent'SaveParams sturdyRef owner msg)
        = Persistent'SaveParams'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Persistent'SaveParams'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Persistent'SaveParams'List_ l) = (Untyped.ListStruct l)
    length (Persistent'SaveParams'List_ l) = (Untyped.length l)
    index i (Persistent'SaveParams'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Persistent'SaveParams sturdyRef owner (Message.MutMsg s))) where
    setIndex (Persistent'SaveParams'newtype_ elt) i (Persistent'SaveParams'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Persistent'SaveParams'List_ <$> (Untyped.allocCompositeList msg 0 1 len))
get_Persistent'SaveParams'sealFor :: ((Untyped.ReadCtx m msg)
                                     ,(Classes.FromPtr msg owner)) => (Persistent'SaveParams sturdyRef owner msg) -> (m owner)
get_Persistent'SaveParams'sealFor (Persistent'SaveParams'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Persistent'SaveParams'sealFor :: ((Untyped.RWCtx m s)
                                     ,(Classes.ToPtr s owner)) => (Persistent'SaveParams sturdyRef owner (Message.MutMsg s)) -> owner -> (m ())
set_Persistent'SaveParams'sealFor (Persistent'SaveParams'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Persistent'SaveParams'sealFor :: ((Untyped.ReadCtx m msg)) => (Persistent'SaveParams sturdyRef owner msg) -> (m Std_.Bool)
has_Persistent'SaveParams'sealFor (Persistent'SaveParams'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
newtype Persistent'SaveResults sturdyRef owner msg
    = Persistent'SaveResults'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Persistent'SaveResults sturdyRef owner msg)) where
    fromStruct struct = (Std_.pure (Persistent'SaveResults'newtype_ struct))
instance (Classes.ToStruct msg (Persistent'SaveResults sturdyRef owner msg)) where
    toStruct (Persistent'SaveResults'newtype_ struct) = struct
instance (Untyped.HasMessage (Persistent'SaveResults sturdyRef owner msg)) where
    type InMessage (Persistent'SaveResults sturdyRef owner msg) = msg
    message (Persistent'SaveResults'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Persistent'SaveResults sturdyRef owner msg)) where
    messageDefault msg = (Persistent'SaveResults'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Persistent'SaveResults sturdyRef owner msg)) where
    fromPtr msg ptr = (Persistent'SaveResults'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Persistent'SaveResults sturdyRef owner (Message.MutMsg s))) where
    toPtr msg (Persistent'SaveResults'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Persistent'SaveResults sturdyRef owner (Message.MutMsg s))) where
    new msg = (Persistent'SaveResults'newtype_ <$> (Untyped.allocStruct msg 0 1))
instance (Basics.ListElem msg (Persistent'SaveResults sturdyRef owner msg)) where
    newtype List msg (Persistent'SaveResults sturdyRef owner msg)
        = Persistent'SaveResults'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Persistent'SaveResults'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Persistent'SaveResults'List_ l) = (Untyped.ListStruct l)
    length (Persistent'SaveResults'List_ l) = (Untyped.length l)
    index i (Persistent'SaveResults'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Persistent'SaveResults sturdyRef owner (Message.MutMsg s))) where
    setIndex (Persistent'SaveResults'newtype_ elt) i (Persistent'SaveResults'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Persistent'SaveResults'List_ <$> (Untyped.allocCompositeList msg 0 1 len))
get_Persistent'SaveResults'sturdyRef :: ((Untyped.ReadCtx m msg)
                                        ,(Classes.FromPtr msg sturdyRef)) => (Persistent'SaveResults sturdyRef owner msg) -> (m sturdyRef)
get_Persistent'SaveResults'sturdyRef (Persistent'SaveResults'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Persistent'SaveResults'sturdyRef :: ((Untyped.RWCtx m s)
                                        ,(Classes.ToPtr s sturdyRef)) => (Persistent'SaveResults sturdyRef owner (Message.MutMsg s)) -> sturdyRef -> (m ())
set_Persistent'SaveResults'sturdyRef (Persistent'SaveResults'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Persistent'SaveResults'sturdyRef :: ((Untyped.ReadCtx m msg)) => (Persistent'SaveResults sturdyRef owner msg) -> (m Std_.Bool)
has_Persistent'SaveResults'sturdyRef (Persistent'SaveResults'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
newtype RealmGateway internalRef externalRef internalOwner externalOwner msg
    = RealmGateway'newtype_ (Std_.Maybe (Untyped.Cap msg))
instance (Classes.FromPtr msg (RealmGateway internalRef externalRef internalOwner externalOwner msg)) where
    fromPtr msg ptr = (RealmGateway'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (RealmGateway internalRef externalRef internalOwner externalOwner (Message.MutMsg s))) where
    toPtr msg (RealmGateway'newtype_ (Std_.Nothing)) = (Std_.pure Std_.Nothing)
    toPtr msg (RealmGateway'newtype_ (Std_.Just cap)) = (Std_.pure (Std_.Just (Untyped.PtrCap cap)))
newtype RealmGateway'import'params internalRef externalRef internalOwner externalOwner msg
    = RealmGateway'import'params'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (RealmGateway'import'params internalRef externalRef internalOwner externalOwner msg)) where
    fromStruct struct = (Std_.pure (RealmGateway'import'params'newtype_ struct))
instance (Classes.ToStruct msg (RealmGateway'import'params internalRef externalRef internalOwner externalOwner msg)) where
    toStruct (RealmGateway'import'params'newtype_ struct) = struct
instance (Untyped.HasMessage (RealmGateway'import'params internalRef externalRef internalOwner externalOwner msg)) where
    type InMessage (RealmGateway'import'params internalRef externalRef internalOwner externalOwner msg) = msg
    message (RealmGateway'import'params'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (RealmGateway'import'params internalRef externalRef internalOwner externalOwner msg)) where
    messageDefault msg = (RealmGateway'import'params'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (RealmGateway'import'params internalRef externalRef internalOwner externalOwner msg)) where
    fromPtr msg ptr = (RealmGateway'import'params'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (RealmGateway'import'params internalRef externalRef internalOwner externalOwner (Message.MutMsg s))) where
    toPtr msg (RealmGateway'import'params'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (RealmGateway'import'params internalRef externalRef internalOwner externalOwner (Message.MutMsg s))) where
    new msg = (RealmGateway'import'params'newtype_ <$> (Untyped.allocStruct msg 0 2))
instance (Basics.ListElem msg (RealmGateway'import'params internalRef externalRef internalOwner externalOwner msg)) where
    newtype List msg (RealmGateway'import'params internalRef externalRef internalOwner externalOwner msg)
        = RealmGateway'import'params'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (RealmGateway'import'params'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (RealmGateway'import'params'List_ l) = (Untyped.ListStruct l)
    length (RealmGateway'import'params'List_ l) = (Untyped.length l)
    index i (RealmGateway'import'params'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (RealmGateway'import'params internalRef externalRef internalOwner externalOwner (Message.MutMsg s))) where
    setIndex (RealmGateway'import'params'newtype_ elt) i (RealmGateway'import'params'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (RealmGateway'import'params'List_ <$> (Untyped.allocCompositeList msg 0 2 len))
get_RealmGateway'import'params'cap :: ((Untyped.ReadCtx m msg)
                                      ,(Classes.FromPtr msg (Persistent externalRef externalOwner msg))) => (RealmGateway'import'params internalRef externalRef internalOwner externalOwner msg) -> (m (Persistent externalRef externalOwner msg))
get_RealmGateway'import'params'cap (RealmGateway'import'params'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_RealmGateway'import'params'cap :: ((Untyped.RWCtx m s)
                                      ,(Classes.ToPtr s (Persistent externalRef externalOwner (Message.MutMsg s)))) => (RealmGateway'import'params internalRef externalRef internalOwner externalOwner (Message.MutMsg s)) -> (Persistent externalRef externalOwner (Message.MutMsg s)) -> (m ())
set_RealmGateway'import'params'cap (RealmGateway'import'params'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_RealmGateway'import'params'cap :: ((Untyped.ReadCtx m msg)) => (RealmGateway'import'params internalRef externalRef internalOwner externalOwner msg) -> (m Std_.Bool)
has_RealmGateway'import'params'cap (RealmGateway'import'params'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
get_RealmGateway'import'params'params :: ((Untyped.ReadCtx m msg)
                                         ,(Classes.FromPtr msg (Persistent'SaveParams internalRef internalOwner msg))) => (RealmGateway'import'params internalRef externalRef internalOwner externalOwner msg) -> (m (Persistent'SaveParams internalRef internalOwner msg))
get_RealmGateway'import'params'params (RealmGateway'import'params'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_RealmGateway'import'params'params :: ((Untyped.RWCtx m s)
                                         ,(Classes.ToPtr s (Persistent'SaveParams internalRef internalOwner (Message.MutMsg s)))) => (RealmGateway'import'params internalRef externalRef internalOwner externalOwner (Message.MutMsg s)) -> (Persistent'SaveParams internalRef internalOwner (Message.MutMsg s)) -> (m ())
set_RealmGateway'import'params'params (RealmGateway'import'params'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_RealmGateway'import'params'params :: ((Untyped.ReadCtx m msg)) => (RealmGateway'import'params internalRef externalRef internalOwner externalOwner msg) -> (m Std_.Bool)
has_RealmGateway'import'params'params (RealmGateway'import'params'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_RealmGateway'import'params'params :: ((Untyped.RWCtx m s)) => (RealmGateway'import'params internalRef externalRef internalOwner externalOwner (Message.MutMsg s)) -> (m (Persistent'SaveParams internalRef internalOwner (Message.MutMsg s)))
new_RealmGateway'import'params'params struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_RealmGateway'import'params'params struct result)
    (Std_.pure result)
    )
newtype RealmGateway'export'params internalRef externalRef internalOwner externalOwner msg
    = RealmGateway'export'params'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (RealmGateway'export'params internalRef externalRef internalOwner externalOwner msg)) where
    fromStruct struct = (Std_.pure (RealmGateway'export'params'newtype_ struct))
instance (Classes.ToStruct msg (RealmGateway'export'params internalRef externalRef internalOwner externalOwner msg)) where
    toStruct (RealmGateway'export'params'newtype_ struct) = struct
instance (Untyped.HasMessage (RealmGateway'export'params internalRef externalRef internalOwner externalOwner msg)) where
    type InMessage (RealmGateway'export'params internalRef externalRef internalOwner externalOwner msg) = msg
    message (RealmGateway'export'params'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (RealmGateway'export'params internalRef externalRef internalOwner externalOwner msg)) where
    messageDefault msg = (RealmGateway'export'params'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (RealmGateway'export'params internalRef externalRef internalOwner externalOwner msg)) where
    fromPtr msg ptr = (RealmGateway'export'params'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (RealmGateway'export'params internalRef externalRef internalOwner externalOwner (Message.MutMsg s))) where
    toPtr msg (RealmGateway'export'params'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (RealmGateway'export'params internalRef externalRef internalOwner externalOwner (Message.MutMsg s))) where
    new msg = (RealmGateway'export'params'newtype_ <$> (Untyped.allocStruct msg 0 2))
instance (Basics.ListElem msg (RealmGateway'export'params internalRef externalRef internalOwner externalOwner msg)) where
    newtype List msg (RealmGateway'export'params internalRef externalRef internalOwner externalOwner msg)
        = RealmGateway'export'params'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (RealmGateway'export'params'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (RealmGateway'export'params'List_ l) = (Untyped.ListStruct l)
    length (RealmGateway'export'params'List_ l) = (Untyped.length l)
    index i (RealmGateway'export'params'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (RealmGateway'export'params internalRef externalRef internalOwner externalOwner (Message.MutMsg s))) where
    setIndex (RealmGateway'export'params'newtype_ elt) i (RealmGateway'export'params'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (RealmGateway'export'params'List_ <$> (Untyped.allocCompositeList msg 0 2 len))
get_RealmGateway'export'params'cap :: ((Untyped.ReadCtx m msg)
                                      ,(Classes.FromPtr msg (Persistent internalRef internalOwner msg))) => (RealmGateway'export'params internalRef externalRef internalOwner externalOwner msg) -> (m (Persistent internalRef internalOwner msg))
get_RealmGateway'export'params'cap (RealmGateway'export'params'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_RealmGateway'export'params'cap :: ((Untyped.RWCtx m s)
                                      ,(Classes.ToPtr s (Persistent internalRef internalOwner (Message.MutMsg s)))) => (RealmGateway'export'params internalRef externalRef internalOwner externalOwner (Message.MutMsg s)) -> (Persistent internalRef internalOwner (Message.MutMsg s)) -> (m ())
set_RealmGateway'export'params'cap (RealmGateway'export'params'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_RealmGateway'export'params'cap :: ((Untyped.ReadCtx m msg)) => (RealmGateway'export'params internalRef externalRef internalOwner externalOwner msg) -> (m Std_.Bool)
has_RealmGateway'export'params'cap (RealmGateway'export'params'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
get_RealmGateway'export'params'params :: ((Untyped.ReadCtx m msg)
                                         ,(Classes.FromPtr msg (Persistent'SaveParams externalRef externalOwner msg))) => (RealmGateway'export'params internalRef externalRef internalOwner externalOwner msg) -> (m (Persistent'SaveParams externalRef externalOwner msg))
get_RealmGateway'export'params'params (RealmGateway'export'params'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_RealmGateway'export'params'params :: ((Untyped.RWCtx m s)
                                         ,(Classes.ToPtr s (Persistent'SaveParams externalRef externalOwner (Message.MutMsg s)))) => (RealmGateway'export'params internalRef externalRef internalOwner externalOwner (Message.MutMsg s)) -> (Persistent'SaveParams externalRef externalOwner (Message.MutMsg s)) -> (m ())
set_RealmGateway'export'params'params (RealmGateway'export'params'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_RealmGateway'export'params'params :: ((Untyped.ReadCtx m msg)) => (RealmGateway'export'params internalRef externalRef internalOwner externalOwner msg) -> (m Std_.Bool)
has_RealmGateway'export'params'params (RealmGateway'export'params'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_RealmGateway'export'params'params :: ((Untyped.RWCtx m s)) => (RealmGateway'export'params internalRef externalRef internalOwner externalOwner (Message.MutMsg s)) -> (m (Persistent'SaveParams externalRef externalOwner (Message.MutMsg s)))
new_RealmGateway'export'params'params struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_RealmGateway'export'params'params struct result)
    (Std_.pure result)
    )