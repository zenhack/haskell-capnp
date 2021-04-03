{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Capnp.Gen.Capnp.Compat.Json where
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
newtype Value msg
    = Value'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Value msg)) where
    fromStruct struct = (Std_.pure (Value'newtype_ struct))
instance (Classes.ToStruct msg (Value msg)) where
    toStruct (Value'newtype_ struct) = struct
instance (Untyped.HasMessage (Value mut) mut) where
    message (Value'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Value mut) mut) where
    messageDefault msg = (Value'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Value msg)) where
    fromPtr msg ptr = (Value'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Value (Message.Mut s))) where
    toPtr msg (Value'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Value (Message.Mut s))) where
    new msg = (Value'newtype_ <$> (Untyped.allocStruct msg 2 1))
instance (Basics.ListElem mut (Value mut)) where
    newtype List mut (Value mut)
        = Value'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Value'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Value'List_ l) = (Untyped.ListStruct l)
    length (Value'List_ l) = (Untyped.length l)
    index i (Value'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Value (Message.Mut s))) where
    setIndex (Value'newtype_ elt) i (Value'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Value'List_ <$> (Untyped.allocCompositeList msg 2 1 len))
data Value' (mut :: Message.Mutability)
    = Value'null 
    | Value'boolean Std_.Bool
    | Value'number Std_.Double
    | Value'string (Basics.Text mut)
    | Value'array (Basics.List mut (Value mut))
    | Value'object (Basics.List mut (Value'Field mut))
    | Value'call (Value'Call mut)
    | Value'unknown' Std_.Word16
instance (Classes.FromStruct mut (Value' mut)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 0)
        case tag of
            0 ->
                (Std_.pure Value'null)
            1 ->
                (Value'boolean <$> (GenHelpers.getWordField struct 0 16 0))
            2 ->
                (Value'number <$> (GenHelpers.getWordField struct 1 0 0))
            3 ->
                (Value'string <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            4 ->
                (Value'array <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            5 ->
                (Value'object <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            6 ->
                (Value'call <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            _ ->
                (Std_.pure (Value'unknown' (Std_.fromIntegral tag)))
        )
get_Value' :: ((Untyped.ReadCtx m msg)
              ,(Classes.FromStruct msg (Value' msg))) => (Value msg) -> (m (Value' msg))
get_Value' (Value'newtype_ struct) = (Classes.fromStruct struct)
set_Value'null :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> (m ())
set_Value'null (Value'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Value'boolean :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Bool -> (m ())
set_Value'boolean (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 0 16 0)
    )
set_Value'number :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Double -> (m ())
set_Value'number (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
    )
set_Value'string :: ((Untyped.RWCtx m s)
                    ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (Value (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_Value'string (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Value'array :: ((Untyped.RWCtx m s)
                   ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Value (Message.Mut s))))) => (Value (Message.Mut s)) -> (Basics.List (Message.Mut s) (Value (Message.Mut s))) -> (m ())
set_Value'array (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (4 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Value'object :: ((Untyped.RWCtx m s)
                    ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Value'Field (Message.Mut s))))) => (Value (Message.Mut s)) -> (Basics.List (Message.Mut s) (Value'Field (Message.Mut s))) -> (m ())
set_Value'object (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (5 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Value'call :: ((Untyped.RWCtx m s)
                  ,(Classes.ToPtr s (Value'Call (Message.Mut s)))) => (Value (Message.Mut s)) -> (Value'Call (Message.Mut s)) -> (m ())
set_Value'call (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (6 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Value'unknown' :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Value'unknown' (Value'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
newtype Value'Field msg
    = Value'Field'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Value'Field msg)) where
    fromStruct struct = (Std_.pure (Value'Field'newtype_ struct))
instance (Classes.ToStruct msg (Value'Field msg)) where
    toStruct (Value'Field'newtype_ struct) = struct
instance (Untyped.HasMessage (Value'Field mut) mut) where
    message (Value'Field'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Value'Field mut) mut) where
    messageDefault msg = (Value'Field'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Value'Field msg)) where
    fromPtr msg ptr = (Value'Field'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Value'Field (Message.Mut s))) where
    toPtr msg (Value'Field'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Value'Field (Message.Mut s))) where
    new msg = (Value'Field'newtype_ <$> (Untyped.allocStruct msg 0 2))
instance (Basics.ListElem mut (Value'Field mut)) where
    newtype List mut (Value'Field mut)
        = Value'Field'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Value'Field'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Value'Field'List_ l) = (Untyped.ListStruct l)
    length (Value'Field'List_ l) = (Untyped.length l)
    index i (Value'Field'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Value'Field (Message.Mut s))) where
    setIndex (Value'Field'newtype_ elt) i (Value'Field'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Value'Field'List_ <$> (Untyped.allocCompositeList msg 0 2 len))
get_Value'Field'name :: ((Untyped.ReadCtx m msg)
                        ,(Classes.FromPtr msg (Basics.Text msg))) => (Value'Field msg) -> (m (Basics.Text msg))
get_Value'Field'name (Value'Field'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Value'Field'name :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (Value'Field (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_Value'Field'name (Value'Field'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Value'Field'name :: ((Untyped.ReadCtx m msg)) => (Value'Field msg) -> (m Std_.Bool)
has_Value'Field'name (Value'Field'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Value'Field'name :: ((Untyped.RWCtx m s)) => Std_.Int -> (Value'Field (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_Value'Field'name len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Value'Field'name struct result)
    (Std_.pure result)
    )
get_Value'Field'value :: ((Untyped.ReadCtx m msg)
                         ,(Classes.FromPtr msg (Value msg))) => (Value'Field msg) -> (m (Value msg))
get_Value'Field'value (Value'Field'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Value'Field'value :: ((Untyped.RWCtx m s)
                         ,(Classes.ToPtr s (Value (Message.Mut s)))) => (Value'Field (Message.Mut s)) -> (Value (Message.Mut s)) -> (m ())
set_Value'Field'value (Value'Field'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Value'Field'value :: ((Untyped.ReadCtx m msg)) => (Value'Field msg) -> (m Std_.Bool)
has_Value'Field'value (Value'Field'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Value'Field'value :: ((Untyped.RWCtx m s)) => (Value'Field (Message.Mut s)) -> (m (Value (Message.Mut s)))
new_Value'Field'value struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Value'Field'value struct result)
    (Std_.pure result)
    )
newtype Value'Call msg
    = Value'Call'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Value'Call msg)) where
    fromStruct struct = (Std_.pure (Value'Call'newtype_ struct))
instance (Classes.ToStruct msg (Value'Call msg)) where
    toStruct (Value'Call'newtype_ struct) = struct
instance (Untyped.HasMessage (Value'Call mut) mut) where
    message (Value'Call'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Value'Call mut) mut) where
    messageDefault msg = (Value'Call'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Value'Call msg)) where
    fromPtr msg ptr = (Value'Call'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Value'Call (Message.Mut s))) where
    toPtr msg (Value'Call'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Value'Call (Message.Mut s))) where
    new msg = (Value'Call'newtype_ <$> (Untyped.allocStruct msg 0 2))
instance (Basics.ListElem mut (Value'Call mut)) where
    newtype List mut (Value'Call mut)
        = Value'Call'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Value'Call'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Value'Call'List_ l) = (Untyped.ListStruct l)
    length (Value'Call'List_ l) = (Untyped.length l)
    index i (Value'Call'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Value'Call (Message.Mut s))) where
    setIndex (Value'Call'newtype_ elt) i (Value'Call'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Value'Call'List_ <$> (Untyped.allocCompositeList msg 0 2 len))
get_Value'Call'function :: ((Untyped.ReadCtx m msg)
                           ,(Classes.FromPtr msg (Basics.Text msg))) => (Value'Call msg) -> (m (Basics.Text msg))
get_Value'Call'function (Value'Call'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Value'Call'function :: ((Untyped.RWCtx m s)
                           ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (Value'Call (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_Value'Call'function (Value'Call'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Value'Call'function :: ((Untyped.ReadCtx m msg)) => (Value'Call msg) -> (m Std_.Bool)
has_Value'Call'function (Value'Call'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Value'Call'function :: ((Untyped.RWCtx m s)) => Std_.Int -> (Value'Call (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_Value'Call'function len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Value'Call'function struct result)
    (Std_.pure result)
    )
get_Value'Call'params :: ((Untyped.ReadCtx m msg)
                         ,(Classes.FromPtr msg (Basics.List msg (Value msg)))) => (Value'Call msg) -> (m (Basics.List msg (Value msg)))
get_Value'Call'params (Value'Call'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Value'Call'params :: ((Untyped.RWCtx m s)
                         ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Value (Message.Mut s))))) => (Value'Call (Message.Mut s)) -> (Basics.List (Message.Mut s) (Value (Message.Mut s))) -> (m ())
set_Value'Call'params (Value'Call'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Value'Call'params :: ((Untyped.ReadCtx m msg)) => (Value'Call msg) -> (m Std_.Bool)
has_Value'Call'params (Value'Call'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Value'Call'params :: ((Untyped.RWCtx m s)) => Std_.Int -> (Value'Call (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Value (Message.Mut s))))
new_Value'Call'params len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Value'Call'params struct result)
    (Std_.pure result)
    )
newtype FlattenOptions msg
    = FlattenOptions'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (FlattenOptions msg)) where
    fromStruct struct = (Std_.pure (FlattenOptions'newtype_ struct))
instance (Classes.ToStruct msg (FlattenOptions msg)) where
    toStruct (FlattenOptions'newtype_ struct) = struct
instance (Untyped.HasMessage (FlattenOptions mut) mut) where
    message (FlattenOptions'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (FlattenOptions mut) mut) where
    messageDefault msg = (FlattenOptions'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (FlattenOptions msg)) where
    fromPtr msg ptr = (FlattenOptions'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (FlattenOptions (Message.Mut s))) where
    toPtr msg (FlattenOptions'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (FlattenOptions (Message.Mut s))) where
    new msg = (FlattenOptions'newtype_ <$> (Untyped.allocStruct msg 0 1))
instance (Basics.ListElem mut (FlattenOptions mut)) where
    newtype List mut (FlattenOptions mut)
        = FlattenOptions'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (FlattenOptions'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (FlattenOptions'List_ l) = (Untyped.ListStruct l)
    length (FlattenOptions'List_ l) = (Untyped.length l)
    index i (FlattenOptions'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (FlattenOptions (Message.Mut s))) where
    setIndex (FlattenOptions'newtype_ elt) i (FlattenOptions'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (FlattenOptions'List_ <$> (Untyped.allocCompositeList msg 0 1 len))
get_FlattenOptions'prefix :: ((Untyped.ReadCtx m msg)
                             ,(Classes.FromPtr msg (Basics.Text msg))) => (FlattenOptions msg) -> (m (Basics.Text msg))
get_FlattenOptions'prefix (FlattenOptions'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_FlattenOptions'prefix :: ((Untyped.RWCtx m s)
                             ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (FlattenOptions (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_FlattenOptions'prefix (FlattenOptions'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_FlattenOptions'prefix :: ((Untyped.ReadCtx m msg)) => (FlattenOptions msg) -> (m Std_.Bool)
has_FlattenOptions'prefix (FlattenOptions'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_FlattenOptions'prefix :: ((Untyped.RWCtx m s)) => Std_.Int -> (FlattenOptions (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_FlattenOptions'prefix len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_FlattenOptions'prefix struct result)
    (Std_.pure result)
    )
newtype DiscriminatorOptions msg
    = DiscriminatorOptions'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (DiscriminatorOptions msg)) where
    fromStruct struct = (Std_.pure (DiscriminatorOptions'newtype_ struct))
instance (Classes.ToStruct msg (DiscriminatorOptions msg)) where
    toStruct (DiscriminatorOptions'newtype_ struct) = struct
instance (Untyped.HasMessage (DiscriminatorOptions mut) mut) where
    message (DiscriminatorOptions'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (DiscriminatorOptions mut) mut) where
    messageDefault msg = (DiscriminatorOptions'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (DiscriminatorOptions msg)) where
    fromPtr msg ptr = (DiscriminatorOptions'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (DiscriminatorOptions (Message.Mut s))) where
    toPtr msg (DiscriminatorOptions'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (DiscriminatorOptions (Message.Mut s))) where
    new msg = (DiscriminatorOptions'newtype_ <$> (Untyped.allocStruct msg 0 2))
instance (Basics.ListElem mut (DiscriminatorOptions mut)) where
    newtype List mut (DiscriminatorOptions mut)
        = DiscriminatorOptions'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (DiscriminatorOptions'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (DiscriminatorOptions'List_ l) = (Untyped.ListStruct l)
    length (DiscriminatorOptions'List_ l) = (Untyped.length l)
    index i (DiscriminatorOptions'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (DiscriminatorOptions (Message.Mut s))) where
    setIndex (DiscriminatorOptions'newtype_ elt) i (DiscriminatorOptions'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (DiscriminatorOptions'List_ <$> (Untyped.allocCompositeList msg 0 2 len))
get_DiscriminatorOptions'name :: ((Untyped.ReadCtx m msg)
                                 ,(Classes.FromPtr msg (Basics.Text msg))) => (DiscriminatorOptions msg) -> (m (Basics.Text msg))
get_DiscriminatorOptions'name (DiscriminatorOptions'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_DiscriminatorOptions'name :: ((Untyped.RWCtx m s)
                                 ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (DiscriminatorOptions (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_DiscriminatorOptions'name (DiscriminatorOptions'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_DiscriminatorOptions'name :: ((Untyped.ReadCtx m msg)) => (DiscriminatorOptions msg) -> (m Std_.Bool)
has_DiscriminatorOptions'name (DiscriminatorOptions'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_DiscriminatorOptions'name :: ((Untyped.RWCtx m s)) => Std_.Int -> (DiscriminatorOptions (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_DiscriminatorOptions'name len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_DiscriminatorOptions'name struct result)
    (Std_.pure result)
    )
get_DiscriminatorOptions'valueName :: ((Untyped.ReadCtx m msg)
                                      ,(Classes.FromPtr msg (Basics.Text msg))) => (DiscriminatorOptions msg) -> (m (Basics.Text msg))
get_DiscriminatorOptions'valueName (DiscriminatorOptions'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_DiscriminatorOptions'valueName :: ((Untyped.RWCtx m s)
                                      ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (DiscriminatorOptions (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_DiscriminatorOptions'valueName (DiscriminatorOptions'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_DiscriminatorOptions'valueName :: ((Untyped.ReadCtx m msg)) => (DiscriminatorOptions msg) -> (m Std_.Bool)
has_DiscriminatorOptions'valueName (DiscriminatorOptions'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_DiscriminatorOptions'valueName :: ((Untyped.RWCtx m s)) => Std_.Int -> (DiscriminatorOptions (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_DiscriminatorOptions'valueName len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_DiscriminatorOptions'valueName struct result)
    (Std_.pure result)
    )