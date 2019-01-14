{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Capnp.Gen.Capnp.Json where
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
newtype JsonValue msg
    = JsonValue'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg JsonValue) where
    tMsg f (JsonValue'newtype_ s) = (JsonValue'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (JsonValue msg)) where
    fromStruct struct = (Std_.pure (JsonValue'newtype_ struct))
instance (Classes.ToStruct msg (JsonValue msg)) where
    toStruct (JsonValue'newtype_ struct) = struct
instance (Untyped.HasMessage (JsonValue msg)) where
    type InMessage (JsonValue msg) = msg
    message (JsonValue'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (JsonValue msg)) where
    messageDefault msg = (JsonValue'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (JsonValue msg)) where
    fromPtr msg ptr = (JsonValue'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (JsonValue (Message.MutMsg s))) where
    toPtr msg (JsonValue'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (JsonValue (Message.MutMsg s))) where
    new msg = (JsonValue'newtype_ <$> (Untyped.allocStruct msg 2 1))
instance (Basics.ListElem msg (JsonValue msg)) where
    newtype List msg (JsonValue msg)
        = JsonValue'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (JsonValue'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (JsonValue'List_ l) = (Untyped.ListStruct l)
    length (JsonValue'List_ l) = (Untyped.length l)
    index i (JsonValue'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (JsonValue (Message.MutMsg s))) where
    setIndex (JsonValue'newtype_ elt) i (JsonValue'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (JsonValue'List_ <$> (Untyped.allocCompositeList msg 2 1 len))
data JsonValue' msg
    = JsonValue'null 
    | JsonValue'boolean Std_.Bool
    | JsonValue'number Std_.Double
    | JsonValue'string (Basics.Text msg)
    | JsonValue'array (Basics.List msg (JsonValue msg))
    | JsonValue'object (Basics.List msg (JsonValue'Field msg))
    | JsonValue'call (JsonValue'Call msg)
    | JsonValue'unknown' Std_.Word16
instance (Classes.FromStruct msg (JsonValue' msg)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 0)
        case tag of
            0 ->
                (Std_.pure JsonValue'null)
            1 ->
                (JsonValue'boolean <$> (GenHelpers.getWordField struct 0 16 0))
            2 ->
                (JsonValue'number <$> (GenHelpers.getWordField struct 1 0 0))
            3 ->
                (JsonValue'string <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            4 ->
                (JsonValue'array <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            5 ->
                (JsonValue'object <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            6 ->
                (JsonValue'call <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            _ ->
                (Std_.pure (JsonValue'unknown' (Std_.fromIntegral tag)))
        )
get_JsonValue' :: ((Untyped.ReadCtx m msg)) => (JsonValue msg) -> (m (JsonValue' msg))
get_JsonValue' (JsonValue'newtype_ struct) = (Classes.fromStruct struct)
set_JsonValue'null :: ((Untyped.RWCtx m s)) => (JsonValue (Message.MutMsg s)) -> () -> (m ())
set_JsonValue'null (JsonValue'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_JsonValue'boolean :: ((Untyped.RWCtx m s)) => (JsonValue (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_JsonValue'boolean (JsonValue'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 0 16 0)
    )
set_JsonValue'number :: ((Untyped.RWCtx m s)) => (JsonValue (Message.MutMsg s)) -> Std_.Double -> (m ())
set_JsonValue'number (JsonValue'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
    )
set_JsonValue'string :: ((Untyped.RWCtx m s)) => (JsonValue (Message.MutMsg s)) -> (Basics.Text (Message.MutMsg s)) -> (m ())
set_JsonValue'string (JsonValue'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_JsonValue'array :: ((Untyped.RWCtx m s)) => (JsonValue (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (JsonValue (Message.MutMsg s))) -> (m ())
set_JsonValue'array (JsonValue'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (4 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_JsonValue'object :: ((Untyped.RWCtx m s)) => (JsonValue (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (JsonValue'Field (Message.MutMsg s))) -> (m ())
set_JsonValue'object (JsonValue'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (5 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_JsonValue'call :: ((Untyped.RWCtx m s)) => (JsonValue (Message.MutMsg s)) -> (JsonValue'Call (Message.MutMsg s)) -> (m ())
set_JsonValue'call (JsonValue'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (6 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_JsonValue'unknown' :: ((Untyped.RWCtx m s)) => (JsonValue (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_JsonValue'unknown' (JsonValue'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
newtype JsonValue'Field msg
    = JsonValue'Field'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg JsonValue'Field) where
    tMsg f (JsonValue'Field'newtype_ s) = (JsonValue'Field'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (JsonValue'Field msg)) where
    fromStruct struct = (Std_.pure (JsonValue'Field'newtype_ struct))
instance (Classes.ToStruct msg (JsonValue'Field msg)) where
    toStruct (JsonValue'Field'newtype_ struct) = struct
instance (Untyped.HasMessage (JsonValue'Field msg)) where
    type InMessage (JsonValue'Field msg) = msg
    message (JsonValue'Field'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (JsonValue'Field msg)) where
    messageDefault msg = (JsonValue'Field'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (JsonValue'Field msg)) where
    fromPtr msg ptr = (JsonValue'Field'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (JsonValue'Field (Message.MutMsg s))) where
    toPtr msg (JsonValue'Field'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (JsonValue'Field (Message.MutMsg s))) where
    new msg = (JsonValue'Field'newtype_ <$> (Untyped.allocStruct msg 0 2))
instance (Basics.ListElem msg (JsonValue'Field msg)) where
    newtype List msg (JsonValue'Field msg)
        = JsonValue'Field'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (JsonValue'Field'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (JsonValue'Field'List_ l) = (Untyped.ListStruct l)
    length (JsonValue'Field'List_ l) = (Untyped.length l)
    index i (JsonValue'Field'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (JsonValue'Field (Message.MutMsg s))) where
    setIndex (JsonValue'Field'newtype_ elt) i (JsonValue'Field'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (JsonValue'Field'List_ <$> (Untyped.allocCompositeList msg 0 2 len))
get_JsonValue'Field'name :: ((Untyped.ReadCtx m msg)) => (JsonValue'Field msg) -> (m (Basics.Text msg))
get_JsonValue'Field'name (JsonValue'Field'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_JsonValue'Field'name :: ((Untyped.RWCtx m s)) => (JsonValue'Field (Message.MutMsg s)) -> (Basics.Text (Message.MutMsg s)) -> (m ())
set_JsonValue'Field'name (JsonValue'Field'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_JsonValue'Field'name :: ((Untyped.ReadCtx m msg)) => (JsonValue'Field msg) -> (m Std_.Bool)
has_JsonValue'Field'name (JsonValue'Field'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_JsonValue'Field'name :: ((Untyped.RWCtx m s)) => Std_.Int -> (JsonValue'Field (Message.MutMsg s)) -> (m (Basics.Text (Message.MutMsg s)))
new_JsonValue'Field'name len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_JsonValue'Field'name struct result)
    (Std_.pure result)
    )
get_JsonValue'Field'value :: ((Untyped.ReadCtx m msg)) => (JsonValue'Field msg) -> (m (JsonValue msg))
get_JsonValue'Field'value (JsonValue'Field'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_JsonValue'Field'value :: ((Untyped.RWCtx m s)) => (JsonValue'Field (Message.MutMsg s)) -> (JsonValue (Message.MutMsg s)) -> (m ())
set_JsonValue'Field'value (JsonValue'Field'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_JsonValue'Field'value :: ((Untyped.ReadCtx m msg)) => (JsonValue'Field msg) -> (m Std_.Bool)
has_JsonValue'Field'value (JsonValue'Field'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_JsonValue'Field'value :: ((Untyped.RWCtx m s)) => (JsonValue'Field (Message.MutMsg s)) -> (m (JsonValue (Message.MutMsg s)))
new_JsonValue'Field'value struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_JsonValue'Field'value struct result)
    (Std_.pure result)
    )
newtype JsonValue'Call msg
    = JsonValue'Call'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg JsonValue'Call) where
    tMsg f (JsonValue'Call'newtype_ s) = (JsonValue'Call'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (JsonValue'Call msg)) where
    fromStruct struct = (Std_.pure (JsonValue'Call'newtype_ struct))
instance (Classes.ToStruct msg (JsonValue'Call msg)) where
    toStruct (JsonValue'Call'newtype_ struct) = struct
instance (Untyped.HasMessage (JsonValue'Call msg)) where
    type InMessage (JsonValue'Call msg) = msg
    message (JsonValue'Call'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (JsonValue'Call msg)) where
    messageDefault msg = (JsonValue'Call'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (JsonValue'Call msg)) where
    fromPtr msg ptr = (JsonValue'Call'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (JsonValue'Call (Message.MutMsg s))) where
    toPtr msg (JsonValue'Call'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (JsonValue'Call (Message.MutMsg s))) where
    new msg = (JsonValue'Call'newtype_ <$> (Untyped.allocStruct msg 0 2))
instance (Basics.ListElem msg (JsonValue'Call msg)) where
    newtype List msg (JsonValue'Call msg)
        = JsonValue'Call'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (JsonValue'Call'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (JsonValue'Call'List_ l) = (Untyped.ListStruct l)
    length (JsonValue'Call'List_ l) = (Untyped.length l)
    index i (JsonValue'Call'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (JsonValue'Call (Message.MutMsg s))) where
    setIndex (JsonValue'Call'newtype_ elt) i (JsonValue'Call'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (JsonValue'Call'List_ <$> (Untyped.allocCompositeList msg 0 2 len))
get_JsonValue'Call'function :: ((Untyped.ReadCtx m msg)) => (JsonValue'Call msg) -> (m (Basics.Text msg))
get_JsonValue'Call'function (JsonValue'Call'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_JsonValue'Call'function :: ((Untyped.RWCtx m s)) => (JsonValue'Call (Message.MutMsg s)) -> (Basics.Text (Message.MutMsg s)) -> (m ())
set_JsonValue'Call'function (JsonValue'Call'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_JsonValue'Call'function :: ((Untyped.ReadCtx m msg)) => (JsonValue'Call msg) -> (m Std_.Bool)
has_JsonValue'Call'function (JsonValue'Call'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_JsonValue'Call'function :: ((Untyped.RWCtx m s)) => Std_.Int -> (JsonValue'Call (Message.MutMsg s)) -> (m (Basics.Text (Message.MutMsg s)))
new_JsonValue'Call'function len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_JsonValue'Call'function struct result)
    (Std_.pure result)
    )
get_JsonValue'Call'params :: ((Untyped.ReadCtx m msg)) => (JsonValue'Call msg) -> (m (Basics.List msg (JsonValue msg)))
get_JsonValue'Call'params (JsonValue'Call'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_JsonValue'Call'params :: ((Untyped.RWCtx m s)) => (JsonValue'Call (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (JsonValue (Message.MutMsg s))) -> (m ())
set_JsonValue'Call'params (JsonValue'Call'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_JsonValue'Call'params :: ((Untyped.ReadCtx m msg)) => (JsonValue'Call msg) -> (m Std_.Bool)
has_JsonValue'Call'params (JsonValue'Call'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_JsonValue'Call'params :: ((Untyped.RWCtx m s)) => Std_.Int -> (JsonValue'Call (Message.MutMsg s)) -> (m (Basics.List (Message.MutMsg s) (JsonValue (Message.MutMsg s))))
new_JsonValue'Call'params len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_JsonValue'Call'params struct result)
    (Std_.pure result)
    )