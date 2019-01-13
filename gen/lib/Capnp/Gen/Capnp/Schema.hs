{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Capnp.Gen.Capnp.Schema where
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
newtype Node msg
    = Node'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Node) where
    tMsg f (Node'newtype_ s) = (Node'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Node msg)) where
    fromStruct struct = (Std_.pure (Node'newtype_ struct))
instance (Classes.ToStruct msg (Node msg)) where
    toStruct (Node'newtype_ struct) = struct
instance (Untyped.HasMessage (Node msg)) where
    type InMessage (Node msg) = msg
    message (Node'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node msg)) where
    messageDefault msg = (Node'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Node msg)) where
    fromPtr msg ptr = (Node'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Node (Message.MutMsg s))) where
    toPtr msg (Node'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Node (Message.MutMsg s))) where
    new msg = (Node'newtype_ <$> (Untyped.allocStruct msg 5 6))
instance (Basics.ListElem msg (Node msg)) where
    newtype List msg (Node msg)
        = Node'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Node'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Node'List_ l) = (Untyped.ListStruct l)
    length (Node'List_ l) = (Untyped.length l)
    index i (Node'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Node (Message.MutMsg s))) where
    setIndex (Node'newtype_ elt) i (Node'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Node'List_ <$> (Untyped.allocCompositeList msg 5 6 len))
get_Node'id :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Word64)
get_Node'id (Node'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Node'id :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_Node'id (Node'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
get_Node'displayName :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m (Basics.Text msg))
get_Node'displayName (Node'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'displayName :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> (Basics.Text (Message.MutMsg s)) -> (m ())
set_Node'displayName (Node'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Node'displayName :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Bool)
has_Node'displayName (Node'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Node'displayName :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> Std_.Int -> (m (Basics.Text (Message.MutMsg s)))
new_Node'displayName struct len = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Node'displayName struct result)
    (Std_.pure result)
    )
get_Node'displayNamePrefixLength :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Word32)
get_Node'displayNamePrefixLength (Node'newtype_ struct) = (GenHelpers.getWordField struct 1 0 0)
set_Node'displayNamePrefixLength :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> Std_.Word32 -> (m ())
set_Node'displayNamePrefixLength (Node'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 1 0 0)
get_Node'scopeId :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Word64)
get_Node'scopeId (Node'newtype_ struct) = (GenHelpers.getWordField struct 2 0 0)
set_Node'scopeId :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_Node'scopeId (Node'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 2 0 0)
get_Node'nestedNodes :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m (Basics.List msg (Node'NestedNode msg)))
get_Node'nestedNodes (Node'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'nestedNodes :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Node'NestedNode (Message.MutMsg s))) -> (m ())
set_Node'nestedNodes (Node'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Node'nestedNodes :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Bool)
has_Node'nestedNodes (Node'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Node'nestedNodes :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (Node'NestedNode (Message.MutMsg s))))
new_Node'nestedNodes struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'nestedNodes struct result)
    (Std_.pure result)
    )
get_Node'annotations :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m (Basics.List msg (Annotation msg)))
get_Node'annotations (Node'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 2 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'annotations :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Annotation (Message.MutMsg s))) -> (m ())
set_Node'annotations (Node'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 2 struct)
    )
has_Node'annotations :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Bool)
has_Node'annotations (Node'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 2 struct))
new_Node'annotations :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (Annotation (Message.MutMsg s))))
new_Node'annotations struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'annotations struct result)
    (Std_.pure result)
    )
get_Node'parameters :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m (Basics.List msg (Node'Parameter msg)))
get_Node'parameters (Node'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 5 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'parameters :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Node'Parameter (Message.MutMsg s))) -> (m ())
set_Node'parameters (Node'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 5 struct)
    )
has_Node'parameters :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Bool)
has_Node'parameters (Node'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 5 struct))
new_Node'parameters :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (Node'Parameter (Message.MutMsg s))))
new_Node'parameters struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'parameters struct result)
    (Std_.pure result)
    )
get_Node'isGeneric :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Bool)
get_Node'isGeneric (Node'newtype_ struct) = (GenHelpers.getWordField struct 4 32 0)
set_Node'isGeneric :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Node'isGeneric (Node'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 4 32 0)
data Node' msg
    = Node'file 
    | Node'struct (Node'struct msg)
    | Node'enum (Node'enum msg)
    | Node'interface (Node'interface msg)
    | Node'const (Node'const msg)
    | Node'annotation (Node'annotation msg)
    | Node'unknown' Std_.Word16
instance (Classes.FromStruct msg (Node' msg)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 6)
        case tag of
            0 ->
                (Std_.pure Node'file)
            1 ->
                (Node'struct <$> (Classes.fromStruct struct))
            2 ->
                (Node'enum <$> (Classes.fromStruct struct))
            3 ->
                (Node'interface <$> (Classes.fromStruct struct))
            4 ->
                (Node'const <$> (Classes.fromStruct struct))
            5 ->
                (Node'annotation <$> (Classes.fromStruct struct))
            _ ->
                (Std_.pure (Node'unknown' (Std_.fromIntegral tag)))
        )
get_Node' :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m (Node' msg))
get_Node' (Node'newtype_ struct) = (Classes.fromStruct struct)
set_Node'file :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> () -> (m ())
set_Node'file (Node'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 1 32 0)
    (Std_.pure ())
    )
set_Node'struct :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> (m (Node'struct (Message.MutMsg s)))
set_Node'struct (Node'newtype_ struct) = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 1 32 0)
    (Classes.fromStruct struct)
    )
set_Node'enum :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> (m (Node'enum (Message.MutMsg s)))
set_Node'enum (Node'newtype_ struct) = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 1 32 0)
    (Classes.fromStruct struct)
    )
set_Node'interface :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> (m (Node'interface (Message.MutMsg s)))
set_Node'interface (Node'newtype_ struct) = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 1 32 0)
    (Classes.fromStruct struct)
    )
set_Node'const :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> (m (Node'const (Message.MutMsg s)))
set_Node'const (Node'newtype_ struct) = (do
    (GenHelpers.setWordField struct (4 :: Std_.Word16) 1 32 0)
    (Classes.fromStruct struct)
    )
set_Node'annotation :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> (m (Node'annotation (Message.MutMsg s)))
set_Node'annotation (Node'newtype_ struct) = (do
    (GenHelpers.setWordField struct (5 :: Std_.Word16) 1 32 0)
    (Classes.fromStruct struct)
    )
set_Node'unknown' :: ((Untyped.RWCtx m s)) => (Node (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Node'unknown' (Node'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 32 0)
newtype Node'struct msg
    = Node'struct'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Node'struct) where
    tMsg f (Node'struct'newtype_ s) = (Node'struct'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Node'struct msg)) where
    fromStruct struct = (Std_.pure (Node'struct'newtype_ struct))
instance (Classes.ToStruct msg (Node'struct msg)) where
    toStruct (Node'struct'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'struct msg)) where
    type InMessage (Node'struct msg) = msg
    message (Node'struct'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'struct msg)) where
    messageDefault msg = (Node'struct'newtype_ (Untyped.messageDefault msg))
get_Node'struct'dataWordCount :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m Std_.Word16)
get_Node'struct'dataWordCount (Node'struct'newtype_ struct) = (GenHelpers.getWordField struct 1 48 0)
set_Node'struct'dataWordCount :: ((Untyped.RWCtx m s)) => (Node'struct (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Node'struct'dataWordCount (Node'struct'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 48 0)
get_Node'struct'pointerCount :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m Std_.Word16)
get_Node'struct'pointerCount (Node'struct'newtype_ struct) = (GenHelpers.getWordField struct 3 0 0)
set_Node'struct'pointerCount :: ((Untyped.RWCtx m s)) => (Node'struct (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Node'struct'pointerCount (Node'struct'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 3 0 0)
get_Node'struct'preferredListEncoding :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m ElementSize)
get_Node'struct'preferredListEncoding (Node'struct'newtype_ struct) = (GenHelpers.getWordField struct 3 16 0)
set_Node'struct'preferredListEncoding :: ((Untyped.RWCtx m s)) => (Node'struct (Message.MutMsg s)) -> ElementSize -> (m ())
set_Node'struct'preferredListEncoding (Node'struct'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 3 16 0)
get_Node'struct'isGroup :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m Std_.Bool)
get_Node'struct'isGroup (Node'struct'newtype_ struct) = (GenHelpers.getWordField struct 3 32 0)
set_Node'struct'isGroup :: ((Untyped.RWCtx m s)) => (Node'struct (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Node'struct'isGroup (Node'struct'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 3 32 0)
get_Node'struct'discriminantCount :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m Std_.Word16)
get_Node'struct'discriminantCount (Node'struct'newtype_ struct) = (GenHelpers.getWordField struct 3 48 0)
set_Node'struct'discriminantCount :: ((Untyped.RWCtx m s)) => (Node'struct (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Node'struct'discriminantCount (Node'struct'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 3 48 0)
get_Node'struct'discriminantOffset :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m Std_.Word32)
get_Node'struct'discriminantOffset (Node'struct'newtype_ struct) = (GenHelpers.getWordField struct 4 0 0)
set_Node'struct'discriminantOffset :: ((Untyped.RWCtx m s)) => (Node'struct (Message.MutMsg s)) -> Std_.Word32 -> (m ())
set_Node'struct'discriminantOffset (Node'struct'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 4 0 0)
get_Node'struct'fields :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m (Basics.List msg (Field msg)))
get_Node'struct'fields (Node'struct'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'struct'fields :: ((Untyped.RWCtx m s)) => (Node'struct (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Field (Message.MutMsg s))) -> (m ())
set_Node'struct'fields (Node'struct'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_Node'struct'fields :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m Std_.Bool)
has_Node'struct'fields (Node'struct'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_Node'struct'fields :: ((Untyped.RWCtx m s)) => (Node'struct (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (Field (Message.MutMsg s))))
new_Node'struct'fields struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'struct'fields struct result)
    (Std_.pure result)
    )
newtype Node'enum msg
    = Node'enum'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Node'enum) where
    tMsg f (Node'enum'newtype_ s) = (Node'enum'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Node'enum msg)) where
    fromStruct struct = (Std_.pure (Node'enum'newtype_ struct))
instance (Classes.ToStruct msg (Node'enum msg)) where
    toStruct (Node'enum'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'enum msg)) where
    type InMessage (Node'enum msg) = msg
    message (Node'enum'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'enum msg)) where
    messageDefault msg = (Node'enum'newtype_ (Untyped.messageDefault msg))
get_Node'enum'enumerants :: ((Untyped.ReadCtx m msg)) => (Node'enum msg) -> (m (Basics.List msg (Enumerant msg)))
get_Node'enum'enumerants (Node'enum'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'enum'enumerants :: ((Untyped.RWCtx m s)) => (Node'enum (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Enumerant (Message.MutMsg s))) -> (m ())
set_Node'enum'enumerants (Node'enum'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_Node'enum'enumerants :: ((Untyped.ReadCtx m msg)) => (Node'enum msg) -> (m Std_.Bool)
has_Node'enum'enumerants (Node'enum'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_Node'enum'enumerants :: ((Untyped.RWCtx m s)) => (Node'enum (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (Enumerant (Message.MutMsg s))))
new_Node'enum'enumerants struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'enum'enumerants struct result)
    (Std_.pure result)
    )
newtype Node'interface msg
    = Node'interface'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Node'interface) where
    tMsg f (Node'interface'newtype_ s) = (Node'interface'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Node'interface msg)) where
    fromStruct struct = (Std_.pure (Node'interface'newtype_ struct))
instance (Classes.ToStruct msg (Node'interface msg)) where
    toStruct (Node'interface'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'interface msg)) where
    type InMessage (Node'interface msg) = msg
    message (Node'interface'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'interface msg)) where
    messageDefault msg = (Node'interface'newtype_ (Untyped.messageDefault msg))
get_Node'interface'methods :: ((Untyped.ReadCtx m msg)) => (Node'interface msg) -> (m (Basics.List msg (Method msg)))
get_Node'interface'methods (Node'interface'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'interface'methods :: ((Untyped.RWCtx m s)) => (Node'interface (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Method (Message.MutMsg s))) -> (m ())
set_Node'interface'methods (Node'interface'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_Node'interface'methods :: ((Untyped.ReadCtx m msg)) => (Node'interface msg) -> (m Std_.Bool)
has_Node'interface'methods (Node'interface'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_Node'interface'methods :: ((Untyped.RWCtx m s)) => (Node'interface (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (Method (Message.MutMsg s))))
new_Node'interface'methods struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'interface'methods struct result)
    (Std_.pure result)
    )
get_Node'interface'superclasses :: ((Untyped.ReadCtx m msg)) => (Node'interface msg) -> (m (Basics.List msg (Superclass msg)))
get_Node'interface'superclasses (Node'interface'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 4 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'interface'superclasses :: ((Untyped.RWCtx m s)) => (Node'interface (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Superclass (Message.MutMsg s))) -> (m ())
set_Node'interface'superclasses (Node'interface'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 4 struct)
    )
has_Node'interface'superclasses :: ((Untyped.ReadCtx m msg)) => (Node'interface msg) -> (m Std_.Bool)
has_Node'interface'superclasses (Node'interface'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 4 struct))
new_Node'interface'superclasses :: ((Untyped.RWCtx m s)) => (Node'interface (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (Superclass (Message.MutMsg s))))
new_Node'interface'superclasses struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'interface'superclasses struct result)
    (Std_.pure result)
    )
newtype Node'const msg
    = Node'const'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Node'const) where
    tMsg f (Node'const'newtype_ s) = (Node'const'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Node'const msg)) where
    fromStruct struct = (Std_.pure (Node'const'newtype_ struct))
instance (Classes.ToStruct msg (Node'const msg)) where
    toStruct (Node'const'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'const msg)) where
    type InMessage (Node'const msg) = msg
    message (Node'const'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'const msg)) where
    messageDefault msg = (Node'const'newtype_ (Untyped.messageDefault msg))
get_Node'const'type_ :: ((Untyped.ReadCtx m msg)) => (Node'const msg) -> (m (Type msg))
get_Node'const'type_ (Node'const'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'const'type_ :: ((Untyped.RWCtx m s)) => (Node'const (Message.MutMsg s)) -> (Type (Message.MutMsg s)) -> (m ())
set_Node'const'type_ (Node'const'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_Node'const'type_ :: ((Untyped.ReadCtx m msg)) => (Node'const msg) -> (m Std_.Bool)
has_Node'const'type_ (Node'const'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_Node'const'type_ :: ((Untyped.RWCtx m s)) => (Node'const (Message.MutMsg s)) -> (m (Type (Message.MutMsg s)))
new_Node'const'type_ struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Node'const'type_ struct result)
    (Std_.pure result)
    )
get_Node'const'value :: ((Untyped.ReadCtx m msg)) => (Node'const msg) -> (m (Value msg))
get_Node'const'value (Node'const'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 4 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'const'value :: ((Untyped.RWCtx m s)) => (Node'const (Message.MutMsg s)) -> (Value (Message.MutMsg s)) -> (m ())
set_Node'const'value (Node'const'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 4 struct)
    )
has_Node'const'value :: ((Untyped.ReadCtx m msg)) => (Node'const msg) -> (m Std_.Bool)
has_Node'const'value (Node'const'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 4 struct))
new_Node'const'value :: ((Untyped.RWCtx m s)) => (Node'const (Message.MutMsg s)) -> (m (Value (Message.MutMsg s)))
new_Node'const'value struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Node'const'value struct result)
    (Std_.pure result)
    )
newtype Node'annotation msg
    = Node'annotation'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Node'annotation) where
    tMsg f (Node'annotation'newtype_ s) = (Node'annotation'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Node'annotation msg)) where
    fromStruct struct = (Std_.pure (Node'annotation'newtype_ struct))
instance (Classes.ToStruct msg (Node'annotation msg)) where
    toStruct (Node'annotation'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'annotation msg)) where
    type InMessage (Node'annotation msg) = msg
    message (Node'annotation'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'annotation msg)) where
    messageDefault msg = (Node'annotation'newtype_ (Untyped.messageDefault msg))
get_Node'annotation'type_ :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m (Type msg))
get_Node'annotation'type_ (Node'annotation'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'annotation'type_ :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.MutMsg s)) -> (Type (Message.MutMsg s)) -> (m ())
set_Node'annotation'type_ (Node'annotation'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_Node'annotation'type_ :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
has_Node'annotation'type_ (Node'annotation'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_Node'annotation'type_ :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.MutMsg s)) -> (m (Type (Message.MutMsg s)))
new_Node'annotation'type_ struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Node'annotation'type_ struct result)
    (Std_.pure result)
    )
get_Node'annotation'targetsFile :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsFile (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 48 0)
set_Node'annotation'targetsFile :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsFile (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 48 0)
get_Node'annotation'targetsConst :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsConst (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 49 0)
set_Node'annotation'targetsConst :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsConst (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 49 0)
get_Node'annotation'targetsEnum :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsEnum (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 50 0)
set_Node'annotation'targetsEnum :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsEnum (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 50 0)
get_Node'annotation'targetsEnumerant :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsEnumerant (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 51 0)
set_Node'annotation'targetsEnumerant :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsEnumerant (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 51 0)
get_Node'annotation'targetsStruct :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsStruct (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 52 0)
set_Node'annotation'targetsStruct :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsStruct (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 52 0)
get_Node'annotation'targetsField :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsField (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 53 0)
set_Node'annotation'targetsField :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsField (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 53 0)
get_Node'annotation'targetsUnion :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsUnion (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 54 0)
set_Node'annotation'targetsUnion :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsUnion (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 54 0)
get_Node'annotation'targetsGroup :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsGroup (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 55 0)
set_Node'annotation'targetsGroup :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsGroup (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 55 0)
get_Node'annotation'targetsInterface :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsInterface (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 56 0)
set_Node'annotation'targetsInterface :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsInterface (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 56 0)
get_Node'annotation'targetsMethod :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsMethod (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 57 0)
set_Node'annotation'targetsMethod :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsMethod (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 57 0)
get_Node'annotation'targetsParam :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsParam (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 58 0)
set_Node'annotation'targetsParam :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsParam (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 58 0)
get_Node'annotation'targetsAnnotation :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsAnnotation (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 59 0)
set_Node'annotation'targetsAnnotation :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsAnnotation (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 59 0)
newtype Node'Parameter msg
    = Node'Parameter'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Node'Parameter) where
    tMsg f (Node'Parameter'newtype_ s) = (Node'Parameter'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Node'Parameter msg)) where
    fromStruct struct = (Std_.pure (Node'Parameter'newtype_ struct))
instance (Classes.ToStruct msg (Node'Parameter msg)) where
    toStruct (Node'Parameter'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'Parameter msg)) where
    type InMessage (Node'Parameter msg) = msg
    message (Node'Parameter'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'Parameter msg)) where
    messageDefault msg = (Node'Parameter'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Node'Parameter msg)) where
    fromPtr msg ptr = (Node'Parameter'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Node'Parameter (Message.MutMsg s))) where
    toPtr msg (Node'Parameter'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Node'Parameter (Message.MutMsg s))) where
    new msg = (Node'Parameter'newtype_ <$> (Untyped.allocStruct msg 0 1))
instance (Basics.ListElem msg (Node'Parameter msg)) where
    newtype List msg (Node'Parameter msg)
        = Node'Parameter'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Node'Parameter'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Node'Parameter'List_ l) = (Untyped.ListStruct l)
    length (Node'Parameter'List_ l) = (Untyped.length l)
    index i (Node'Parameter'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Node'Parameter (Message.MutMsg s))) where
    setIndex (Node'Parameter'newtype_ elt) i (Node'Parameter'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Node'Parameter'List_ <$> (Untyped.allocCompositeList msg 0 1 len))
get_Node'Parameter'name :: ((Untyped.ReadCtx m msg)) => (Node'Parameter msg) -> (m (Basics.Text msg))
get_Node'Parameter'name (Node'Parameter'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'Parameter'name :: ((Untyped.RWCtx m s)) => (Node'Parameter (Message.MutMsg s)) -> (Basics.Text (Message.MutMsg s)) -> (m ())
set_Node'Parameter'name (Node'Parameter'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Node'Parameter'name :: ((Untyped.ReadCtx m msg)) => (Node'Parameter msg) -> (m Std_.Bool)
has_Node'Parameter'name (Node'Parameter'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Node'Parameter'name :: ((Untyped.RWCtx m s)) => (Node'Parameter (Message.MutMsg s)) -> Std_.Int -> (m (Basics.Text (Message.MutMsg s)))
new_Node'Parameter'name struct len = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Node'Parameter'name struct result)
    (Std_.pure result)
    )
newtype Node'NestedNode msg
    = Node'NestedNode'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Node'NestedNode) where
    tMsg f (Node'NestedNode'newtype_ s) = (Node'NestedNode'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Node'NestedNode msg)) where
    fromStruct struct = (Std_.pure (Node'NestedNode'newtype_ struct))
instance (Classes.ToStruct msg (Node'NestedNode msg)) where
    toStruct (Node'NestedNode'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'NestedNode msg)) where
    type InMessage (Node'NestedNode msg) = msg
    message (Node'NestedNode'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'NestedNode msg)) where
    messageDefault msg = (Node'NestedNode'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Node'NestedNode msg)) where
    fromPtr msg ptr = (Node'NestedNode'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Node'NestedNode (Message.MutMsg s))) where
    toPtr msg (Node'NestedNode'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Node'NestedNode (Message.MutMsg s))) where
    new msg = (Node'NestedNode'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem msg (Node'NestedNode msg)) where
    newtype List msg (Node'NestedNode msg)
        = Node'NestedNode'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Node'NestedNode'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Node'NestedNode'List_ l) = (Untyped.ListStruct l)
    length (Node'NestedNode'List_ l) = (Untyped.length l)
    index i (Node'NestedNode'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Node'NestedNode (Message.MutMsg s))) where
    setIndex (Node'NestedNode'newtype_ elt) i (Node'NestedNode'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Node'NestedNode'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_Node'NestedNode'name :: ((Untyped.ReadCtx m msg)) => (Node'NestedNode msg) -> (m (Basics.Text msg))
get_Node'NestedNode'name (Node'NestedNode'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'NestedNode'name :: ((Untyped.RWCtx m s)) => (Node'NestedNode (Message.MutMsg s)) -> (Basics.Text (Message.MutMsg s)) -> (m ())
set_Node'NestedNode'name (Node'NestedNode'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Node'NestedNode'name :: ((Untyped.ReadCtx m msg)) => (Node'NestedNode msg) -> (m Std_.Bool)
has_Node'NestedNode'name (Node'NestedNode'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Node'NestedNode'name :: ((Untyped.RWCtx m s)) => (Node'NestedNode (Message.MutMsg s)) -> Std_.Int -> (m (Basics.Text (Message.MutMsg s)))
new_Node'NestedNode'name struct len = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Node'NestedNode'name struct result)
    (Std_.pure result)
    )
get_Node'NestedNode'id :: ((Untyped.ReadCtx m msg)) => (Node'NestedNode msg) -> (m Std_.Word64)
get_Node'NestedNode'id (Node'NestedNode'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Node'NestedNode'id :: ((Untyped.RWCtx m s)) => (Node'NestedNode (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_Node'NestedNode'id (Node'NestedNode'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
newtype Field msg
    = Field'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Field) where
    tMsg f (Field'newtype_ s) = (Field'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Field msg)) where
    fromStruct struct = (Std_.pure (Field'newtype_ struct))
instance (Classes.ToStruct msg (Field msg)) where
    toStruct (Field'newtype_ struct) = struct
instance (Untyped.HasMessage (Field msg)) where
    type InMessage (Field msg) = msg
    message (Field'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Field msg)) where
    messageDefault msg = (Field'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Field msg)) where
    fromPtr msg ptr = (Field'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Field (Message.MutMsg s))) where
    toPtr msg (Field'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Field (Message.MutMsg s))) where
    new msg = (Field'newtype_ <$> (Untyped.allocStruct msg 3 4))
instance (Basics.ListElem msg (Field msg)) where
    newtype List msg (Field msg)
        = Field'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Field'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Field'List_ l) = (Untyped.ListStruct l)
    length (Field'List_ l) = (Untyped.length l)
    index i (Field'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Field (Message.MutMsg s))) where
    setIndex (Field'newtype_ elt) i (Field'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Field'List_ <$> (Untyped.allocCompositeList msg 3 4 len))
get_Field'name :: ((Untyped.ReadCtx m msg)) => (Field msg) -> (m (Basics.Text msg))
get_Field'name (Field'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Field'name :: ((Untyped.RWCtx m s)) => (Field (Message.MutMsg s)) -> (Basics.Text (Message.MutMsg s)) -> (m ())
set_Field'name (Field'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Field'name :: ((Untyped.ReadCtx m msg)) => (Field msg) -> (m Std_.Bool)
has_Field'name (Field'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Field'name :: ((Untyped.RWCtx m s)) => (Field (Message.MutMsg s)) -> Std_.Int -> (m (Basics.Text (Message.MutMsg s)))
new_Field'name struct len = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Field'name struct result)
    (Std_.pure result)
    )
get_Field'codeOrder :: ((Untyped.ReadCtx m msg)) => (Field msg) -> (m Std_.Word16)
get_Field'codeOrder (Field'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Field'codeOrder :: ((Untyped.RWCtx m s)) => (Field (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Field'codeOrder (Field'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
get_Field'annotations :: ((Untyped.ReadCtx m msg)) => (Field msg) -> (m (Basics.List msg (Annotation msg)))
get_Field'annotations (Field'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Field'annotations :: ((Untyped.RWCtx m s)) => (Field (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Annotation (Message.MutMsg s))) -> (m ())
set_Field'annotations (Field'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Field'annotations :: ((Untyped.ReadCtx m msg)) => (Field msg) -> (m Std_.Bool)
has_Field'annotations (Field'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Field'annotations :: ((Untyped.RWCtx m s)) => (Field (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (Annotation (Message.MutMsg s))))
new_Field'annotations struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Field'annotations struct result)
    (Std_.pure result)
    )
get_Field'discriminantValue :: ((Untyped.ReadCtx m msg)) => (Field msg) -> (m Std_.Word16)
get_Field'discriminantValue (Field'newtype_ struct) = (GenHelpers.getWordField struct 0 16 65535)
set_Field'discriminantValue :: ((Untyped.RWCtx m s)) => (Field (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Field'discriminantValue (Field'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 16 65535)
get_Field'ordinal :: ((Untyped.ReadCtx m msg)) => (Field msg) -> (m (Field'ordinal msg))
get_Field'ordinal (Field'newtype_ struct) = (Classes.fromStruct struct)
data Field' msg
    = Field'slot (Field'slot msg)
    | Field'group (Field'group msg)
    | Field'unknown' Std_.Word16
instance (Classes.FromStruct msg (Field' msg)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 4)
        case tag of
            0 ->
                (Field'slot <$> (Classes.fromStruct struct))
            1 ->
                (Field'group <$> (Classes.fromStruct struct))
            _ ->
                (Std_.pure (Field'unknown' (Std_.fromIntegral tag)))
        )
get_Field' :: ((Untyped.ReadCtx m msg)) => (Field msg) -> (m (Field' msg))
get_Field' (Field'newtype_ struct) = (Classes.fromStruct struct)
set_Field'slot :: ((Untyped.RWCtx m s)) => (Field (Message.MutMsg s)) -> (m (Field'slot (Message.MutMsg s)))
set_Field'slot (Field'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 1 0 0)
    (Classes.fromStruct struct)
    )
set_Field'group :: ((Untyped.RWCtx m s)) => (Field (Message.MutMsg s)) -> (m (Field'group (Message.MutMsg s)))
set_Field'group (Field'newtype_ struct) = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 1 0 0)
    (Classes.fromStruct struct)
    )
set_Field'unknown' :: ((Untyped.RWCtx m s)) => (Field (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Field'unknown' (Field'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 0 0)
newtype Field'slot msg
    = Field'slot'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Field'slot) where
    tMsg f (Field'slot'newtype_ s) = (Field'slot'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Field'slot msg)) where
    fromStruct struct = (Std_.pure (Field'slot'newtype_ struct))
instance (Classes.ToStruct msg (Field'slot msg)) where
    toStruct (Field'slot'newtype_ struct) = struct
instance (Untyped.HasMessage (Field'slot msg)) where
    type InMessage (Field'slot msg) = msg
    message (Field'slot'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Field'slot msg)) where
    messageDefault msg = (Field'slot'newtype_ (Untyped.messageDefault msg))
get_Field'slot'offset :: ((Untyped.ReadCtx m msg)) => (Field'slot msg) -> (m Std_.Word32)
get_Field'slot'offset (Field'slot'newtype_ struct) = (GenHelpers.getWordField struct 0 32 0)
set_Field'slot'offset :: ((Untyped.RWCtx m s)) => (Field'slot (Message.MutMsg s)) -> Std_.Word32 -> (m ())
set_Field'slot'offset (Field'slot'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 32 0)
get_Field'slot'type_ :: ((Untyped.ReadCtx m msg)) => (Field'slot msg) -> (m (Type msg))
get_Field'slot'type_ (Field'slot'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 2 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Field'slot'type_ :: ((Untyped.RWCtx m s)) => (Field'slot (Message.MutMsg s)) -> (Type (Message.MutMsg s)) -> (m ())
set_Field'slot'type_ (Field'slot'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 2 struct)
    )
has_Field'slot'type_ :: ((Untyped.ReadCtx m msg)) => (Field'slot msg) -> (m Std_.Bool)
has_Field'slot'type_ (Field'slot'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 2 struct))
new_Field'slot'type_ :: ((Untyped.RWCtx m s)) => (Field'slot (Message.MutMsg s)) -> (m (Type (Message.MutMsg s)))
new_Field'slot'type_ struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Field'slot'type_ struct result)
    (Std_.pure result)
    )
get_Field'slot'defaultValue :: ((Untyped.ReadCtx m msg)) => (Field'slot msg) -> (m (Value msg))
get_Field'slot'defaultValue (Field'slot'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Field'slot'defaultValue :: ((Untyped.RWCtx m s)) => (Field'slot (Message.MutMsg s)) -> (Value (Message.MutMsg s)) -> (m ())
set_Field'slot'defaultValue (Field'slot'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_Field'slot'defaultValue :: ((Untyped.ReadCtx m msg)) => (Field'slot msg) -> (m Std_.Bool)
has_Field'slot'defaultValue (Field'slot'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_Field'slot'defaultValue :: ((Untyped.RWCtx m s)) => (Field'slot (Message.MutMsg s)) -> (m (Value (Message.MutMsg s)))
new_Field'slot'defaultValue struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Field'slot'defaultValue struct result)
    (Std_.pure result)
    )
get_Field'slot'hadExplicitDefault :: ((Untyped.ReadCtx m msg)) => (Field'slot msg) -> (m Std_.Bool)
get_Field'slot'hadExplicitDefault (Field'slot'newtype_ struct) = (GenHelpers.getWordField struct 2 0 0)
set_Field'slot'hadExplicitDefault :: ((Untyped.RWCtx m s)) => (Field'slot (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Field'slot'hadExplicitDefault (Field'slot'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 2 0 0)
newtype Field'group msg
    = Field'group'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Field'group) where
    tMsg f (Field'group'newtype_ s) = (Field'group'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Field'group msg)) where
    fromStruct struct = (Std_.pure (Field'group'newtype_ struct))
instance (Classes.ToStruct msg (Field'group msg)) where
    toStruct (Field'group'newtype_ struct) = struct
instance (Untyped.HasMessage (Field'group msg)) where
    type InMessage (Field'group msg) = msg
    message (Field'group'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Field'group msg)) where
    messageDefault msg = (Field'group'newtype_ (Untyped.messageDefault msg))
get_Field'group'typeId :: ((Untyped.ReadCtx m msg)) => (Field'group msg) -> (m Std_.Word64)
get_Field'group'typeId (Field'group'newtype_ struct) = (GenHelpers.getWordField struct 2 0 0)
set_Field'group'typeId :: ((Untyped.RWCtx m s)) => (Field'group (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_Field'group'typeId (Field'group'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 2 0 0)
newtype Field'ordinal msg
    = Field'ordinal'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Field'ordinal) where
    tMsg f (Field'ordinal'newtype_ s) = (Field'ordinal'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Field'ordinal msg)) where
    fromStruct struct = (Std_.pure (Field'ordinal'newtype_ struct))
instance (Classes.ToStruct msg (Field'ordinal msg)) where
    toStruct (Field'ordinal'newtype_ struct) = struct
instance (Untyped.HasMessage (Field'ordinal msg)) where
    type InMessage (Field'ordinal msg) = msg
    message (Field'ordinal'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Field'ordinal msg)) where
    messageDefault msg = (Field'ordinal'newtype_ (Untyped.messageDefault msg))
data Field'ordinal' msg
    = Field'ordinal'implicit 
    | Field'ordinal'explicit Std_.Word16
    | Field'ordinal'unknown' Std_.Word16
instance (Classes.FromStruct msg (Field'ordinal' msg)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 5)
        case tag of
            0 ->
                (Std_.pure Field'ordinal'implicit)
            1 ->
                (Field'ordinal'explicit <$> (GenHelpers.getWordField struct 1 32 0))
            _ ->
                (Std_.pure (Field'ordinal'unknown' (Std_.fromIntegral tag)))
        )
get_Field'ordinal' :: ((Untyped.ReadCtx m msg)) => (Field'ordinal msg) -> (m (Field'ordinal' msg))
get_Field'ordinal' (Field'ordinal'newtype_ struct) = (Classes.fromStruct struct)
set_Field'ordinal'implicit :: ((Untyped.RWCtx m s)) => (Field'ordinal (Message.MutMsg s)) -> () -> (m ())
set_Field'ordinal'implicit (Field'ordinal'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 1 16 0)
    (Std_.pure ())
    )
set_Field'ordinal'explicit :: ((Untyped.RWCtx m s)) => (Field'ordinal (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Field'ordinal'explicit (Field'ordinal'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 1 16 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 32 0)
    )
set_Field'ordinal'unknown' :: ((Untyped.RWCtx m s)) => (Field'ordinal (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Field'ordinal'unknown' (Field'ordinal'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 16 0)
field'noDiscriminant :: Std_.Word16
field'noDiscriminant  = (Classes.fromWord 65535)
newtype Enumerant msg
    = Enumerant'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Enumerant) where
    tMsg f (Enumerant'newtype_ s) = (Enumerant'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Enumerant msg)) where
    fromStruct struct = (Std_.pure (Enumerant'newtype_ struct))
instance (Classes.ToStruct msg (Enumerant msg)) where
    toStruct (Enumerant'newtype_ struct) = struct
instance (Untyped.HasMessage (Enumerant msg)) where
    type InMessage (Enumerant msg) = msg
    message (Enumerant'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Enumerant msg)) where
    messageDefault msg = (Enumerant'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Enumerant msg)) where
    fromPtr msg ptr = (Enumerant'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Enumerant (Message.MutMsg s))) where
    toPtr msg (Enumerant'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Enumerant (Message.MutMsg s))) where
    new msg = (Enumerant'newtype_ <$> (Untyped.allocStruct msg 1 2))
instance (Basics.ListElem msg (Enumerant msg)) where
    newtype List msg (Enumerant msg)
        = Enumerant'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Enumerant'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Enumerant'List_ l) = (Untyped.ListStruct l)
    length (Enumerant'List_ l) = (Untyped.length l)
    index i (Enumerant'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Enumerant (Message.MutMsg s))) where
    setIndex (Enumerant'newtype_ elt) i (Enumerant'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Enumerant'List_ <$> (Untyped.allocCompositeList msg 1 2 len))
get_Enumerant'name :: ((Untyped.ReadCtx m msg)) => (Enumerant msg) -> (m (Basics.Text msg))
get_Enumerant'name (Enumerant'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Enumerant'name :: ((Untyped.RWCtx m s)) => (Enumerant (Message.MutMsg s)) -> (Basics.Text (Message.MutMsg s)) -> (m ())
set_Enumerant'name (Enumerant'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Enumerant'name :: ((Untyped.ReadCtx m msg)) => (Enumerant msg) -> (m Std_.Bool)
has_Enumerant'name (Enumerant'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Enumerant'name :: ((Untyped.RWCtx m s)) => (Enumerant (Message.MutMsg s)) -> Std_.Int -> (m (Basics.Text (Message.MutMsg s)))
new_Enumerant'name struct len = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Enumerant'name struct result)
    (Std_.pure result)
    )
get_Enumerant'codeOrder :: ((Untyped.ReadCtx m msg)) => (Enumerant msg) -> (m Std_.Word16)
get_Enumerant'codeOrder (Enumerant'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Enumerant'codeOrder :: ((Untyped.RWCtx m s)) => (Enumerant (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Enumerant'codeOrder (Enumerant'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
get_Enumerant'annotations :: ((Untyped.ReadCtx m msg)) => (Enumerant msg) -> (m (Basics.List msg (Annotation msg)))
get_Enumerant'annotations (Enumerant'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Enumerant'annotations :: ((Untyped.RWCtx m s)) => (Enumerant (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Annotation (Message.MutMsg s))) -> (m ())
set_Enumerant'annotations (Enumerant'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Enumerant'annotations :: ((Untyped.ReadCtx m msg)) => (Enumerant msg) -> (m Std_.Bool)
has_Enumerant'annotations (Enumerant'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Enumerant'annotations :: ((Untyped.RWCtx m s)) => (Enumerant (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (Annotation (Message.MutMsg s))))
new_Enumerant'annotations struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Enumerant'annotations struct result)
    (Std_.pure result)
    )
newtype Superclass msg
    = Superclass'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Superclass) where
    tMsg f (Superclass'newtype_ s) = (Superclass'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Superclass msg)) where
    fromStruct struct = (Std_.pure (Superclass'newtype_ struct))
instance (Classes.ToStruct msg (Superclass msg)) where
    toStruct (Superclass'newtype_ struct) = struct
instance (Untyped.HasMessage (Superclass msg)) where
    type InMessage (Superclass msg) = msg
    message (Superclass'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Superclass msg)) where
    messageDefault msg = (Superclass'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Superclass msg)) where
    fromPtr msg ptr = (Superclass'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Superclass (Message.MutMsg s))) where
    toPtr msg (Superclass'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Superclass (Message.MutMsg s))) where
    new msg = (Superclass'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem msg (Superclass msg)) where
    newtype List msg (Superclass msg)
        = Superclass'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Superclass'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Superclass'List_ l) = (Untyped.ListStruct l)
    length (Superclass'List_ l) = (Untyped.length l)
    index i (Superclass'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Superclass (Message.MutMsg s))) where
    setIndex (Superclass'newtype_ elt) i (Superclass'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Superclass'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_Superclass'id :: ((Untyped.ReadCtx m msg)) => (Superclass msg) -> (m Std_.Word64)
get_Superclass'id (Superclass'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Superclass'id :: ((Untyped.RWCtx m s)) => (Superclass (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_Superclass'id (Superclass'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
get_Superclass'brand :: ((Untyped.ReadCtx m msg)) => (Superclass msg) -> (m (Brand msg))
get_Superclass'brand (Superclass'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Superclass'brand :: ((Untyped.RWCtx m s)) => (Superclass (Message.MutMsg s)) -> (Brand (Message.MutMsg s)) -> (m ())
set_Superclass'brand (Superclass'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Superclass'brand :: ((Untyped.ReadCtx m msg)) => (Superclass msg) -> (m Std_.Bool)
has_Superclass'brand (Superclass'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Superclass'brand :: ((Untyped.RWCtx m s)) => (Superclass (Message.MutMsg s)) -> (m (Brand (Message.MutMsg s)))
new_Superclass'brand struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Superclass'brand struct result)
    (Std_.pure result)
    )
newtype Method msg
    = Method'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Method) where
    tMsg f (Method'newtype_ s) = (Method'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Method msg)) where
    fromStruct struct = (Std_.pure (Method'newtype_ struct))
instance (Classes.ToStruct msg (Method msg)) where
    toStruct (Method'newtype_ struct) = struct
instance (Untyped.HasMessage (Method msg)) where
    type InMessage (Method msg) = msg
    message (Method'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Method msg)) where
    messageDefault msg = (Method'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Method msg)) where
    fromPtr msg ptr = (Method'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Method (Message.MutMsg s))) where
    toPtr msg (Method'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Method (Message.MutMsg s))) where
    new msg = (Method'newtype_ <$> (Untyped.allocStruct msg 3 5))
instance (Basics.ListElem msg (Method msg)) where
    newtype List msg (Method msg)
        = Method'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Method'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Method'List_ l) = (Untyped.ListStruct l)
    length (Method'List_ l) = (Untyped.length l)
    index i (Method'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Method (Message.MutMsg s))) where
    setIndex (Method'newtype_ elt) i (Method'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Method'List_ <$> (Untyped.allocCompositeList msg 3 5 len))
get_Method'name :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m (Basics.Text msg))
get_Method'name (Method'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Method'name :: ((Untyped.RWCtx m s)) => (Method (Message.MutMsg s)) -> (Basics.Text (Message.MutMsg s)) -> (m ())
set_Method'name (Method'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Method'name :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Bool)
has_Method'name (Method'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Method'name :: ((Untyped.RWCtx m s)) => (Method (Message.MutMsg s)) -> Std_.Int -> (m (Basics.Text (Message.MutMsg s)))
new_Method'name struct len = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Method'name struct result)
    (Std_.pure result)
    )
get_Method'codeOrder :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Word16)
get_Method'codeOrder (Method'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Method'codeOrder :: ((Untyped.RWCtx m s)) => (Method (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Method'codeOrder (Method'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
get_Method'paramStructType :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Word64)
get_Method'paramStructType (Method'newtype_ struct) = (GenHelpers.getWordField struct 1 0 0)
set_Method'paramStructType :: ((Untyped.RWCtx m s)) => (Method (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_Method'paramStructType (Method'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
get_Method'resultStructType :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Word64)
get_Method'resultStructType (Method'newtype_ struct) = (GenHelpers.getWordField struct 2 0 0)
set_Method'resultStructType :: ((Untyped.RWCtx m s)) => (Method (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_Method'resultStructType (Method'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 2 0 0)
get_Method'annotations :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m (Basics.List msg (Annotation msg)))
get_Method'annotations (Method'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Method'annotations :: ((Untyped.RWCtx m s)) => (Method (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Annotation (Message.MutMsg s))) -> (m ())
set_Method'annotations (Method'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Method'annotations :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Bool)
has_Method'annotations (Method'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Method'annotations :: ((Untyped.RWCtx m s)) => (Method (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (Annotation (Message.MutMsg s))))
new_Method'annotations struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Method'annotations struct result)
    (Std_.pure result)
    )
get_Method'paramBrand :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m (Brand msg))
get_Method'paramBrand (Method'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 2 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Method'paramBrand :: ((Untyped.RWCtx m s)) => (Method (Message.MutMsg s)) -> (Brand (Message.MutMsg s)) -> (m ())
set_Method'paramBrand (Method'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 2 struct)
    )
has_Method'paramBrand :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Bool)
has_Method'paramBrand (Method'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 2 struct))
new_Method'paramBrand :: ((Untyped.RWCtx m s)) => (Method (Message.MutMsg s)) -> (m (Brand (Message.MutMsg s)))
new_Method'paramBrand struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Method'paramBrand struct result)
    (Std_.pure result)
    )
get_Method'resultBrand :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m (Brand msg))
get_Method'resultBrand (Method'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Method'resultBrand :: ((Untyped.RWCtx m s)) => (Method (Message.MutMsg s)) -> (Brand (Message.MutMsg s)) -> (m ())
set_Method'resultBrand (Method'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_Method'resultBrand :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Bool)
has_Method'resultBrand (Method'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_Method'resultBrand :: ((Untyped.RWCtx m s)) => (Method (Message.MutMsg s)) -> (m (Brand (Message.MutMsg s)))
new_Method'resultBrand struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Method'resultBrand struct result)
    (Std_.pure result)
    )
get_Method'implicitParameters :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m (Basics.List msg (Node'Parameter msg)))
get_Method'implicitParameters (Method'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 4 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Method'implicitParameters :: ((Untyped.RWCtx m s)) => (Method (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Node'Parameter (Message.MutMsg s))) -> (m ())
set_Method'implicitParameters (Method'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 4 struct)
    )
has_Method'implicitParameters :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Bool)
has_Method'implicitParameters (Method'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 4 struct))
new_Method'implicitParameters :: ((Untyped.RWCtx m s)) => (Method (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (Node'Parameter (Message.MutMsg s))))
new_Method'implicitParameters struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Method'implicitParameters struct result)
    (Std_.pure result)
    )
newtype Type msg
    = Type'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Type) where
    tMsg f (Type'newtype_ s) = (Type'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Type msg)) where
    fromStruct struct = (Std_.pure (Type'newtype_ struct))
instance (Classes.ToStruct msg (Type msg)) where
    toStruct (Type'newtype_ struct) = struct
instance (Untyped.HasMessage (Type msg)) where
    type InMessage (Type msg) = msg
    message (Type'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type msg)) where
    messageDefault msg = (Type'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Type msg)) where
    fromPtr msg ptr = (Type'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Type (Message.MutMsg s))) where
    toPtr msg (Type'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Type (Message.MutMsg s))) where
    new msg = (Type'newtype_ <$> (Untyped.allocStruct msg 3 1))
instance (Basics.ListElem msg (Type msg)) where
    newtype List msg (Type msg)
        = Type'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Type'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Type'List_ l) = (Untyped.ListStruct l)
    length (Type'List_ l) = (Untyped.length l)
    index i (Type'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Type (Message.MutMsg s))) where
    setIndex (Type'newtype_ elt) i (Type'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Type'List_ <$> (Untyped.allocCompositeList msg 3 1 len))
data Type' msg
    = Type'void 
    | Type'bool 
    | Type'int8 
    | Type'int16 
    | Type'int32 
    | Type'int64 
    | Type'uint8 
    | Type'uint16 
    | Type'uint32 
    | Type'uint64 
    | Type'float32 
    | Type'float64 
    | Type'text 
    | Type'data_ 
    | Type'list (Type'list msg)
    | Type'enum (Type'enum msg)
    | Type'struct (Type'struct msg)
    | Type'interface (Type'interface msg)
    | Type'anyPointer (Type'anyPointer msg)
    | Type'unknown' Std_.Word16
instance (Classes.FromStruct msg (Type' msg)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 0)
        case tag of
            0 ->
                (Std_.pure Type'void)
            1 ->
                (Std_.pure Type'bool)
            2 ->
                (Std_.pure Type'int8)
            3 ->
                (Std_.pure Type'int16)
            4 ->
                (Std_.pure Type'int32)
            5 ->
                (Std_.pure Type'int64)
            6 ->
                (Std_.pure Type'uint8)
            7 ->
                (Std_.pure Type'uint16)
            8 ->
                (Std_.pure Type'uint32)
            9 ->
                (Std_.pure Type'uint64)
            10 ->
                (Std_.pure Type'float32)
            11 ->
                (Std_.pure Type'float64)
            12 ->
                (Std_.pure Type'text)
            13 ->
                (Std_.pure Type'data_)
            14 ->
                (Type'list <$> (Classes.fromStruct struct))
            15 ->
                (Type'enum <$> (Classes.fromStruct struct))
            16 ->
                (Type'struct <$> (Classes.fromStruct struct))
            17 ->
                (Type'interface <$> (Classes.fromStruct struct))
            18 ->
                (Type'anyPointer <$> (Classes.fromStruct struct))
            _ ->
                (Std_.pure (Type'unknown' (Std_.fromIntegral tag)))
        )
get_Type' :: ((Untyped.ReadCtx m msg)) => (Type msg) -> (m (Type' msg))
get_Type' (Type'newtype_ struct) = (Classes.fromStruct struct)
set_Type'void :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> () -> (m ())
set_Type'void (Type'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'bool :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> () -> (m ())
set_Type'bool (Type'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'int8 :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> () -> (m ())
set_Type'int8 (Type'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'int16 :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> () -> (m ())
set_Type'int16 (Type'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'int32 :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> () -> (m ())
set_Type'int32 (Type'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (4 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'int64 :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> () -> (m ())
set_Type'int64 (Type'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (5 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'uint8 :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> () -> (m ())
set_Type'uint8 (Type'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (6 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'uint16 :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> () -> (m ())
set_Type'uint16 (Type'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (7 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'uint32 :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> () -> (m ())
set_Type'uint32 (Type'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (8 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'uint64 :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> () -> (m ())
set_Type'uint64 (Type'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (9 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'float32 :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> () -> (m ())
set_Type'float32 (Type'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (10 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'float64 :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> () -> (m ())
set_Type'float64 (Type'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (11 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'text :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> () -> (m ())
set_Type'text (Type'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (12 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'data_ :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> () -> (m ())
set_Type'data_ (Type'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (13 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'list :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> (m (Type'list (Message.MutMsg s)))
set_Type'list (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (14 :: Std_.Word16) 0 0 0)
    (Classes.fromStruct struct)
    )
set_Type'enum :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> (m (Type'enum (Message.MutMsg s)))
set_Type'enum (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (15 :: Std_.Word16) 0 0 0)
    (Classes.fromStruct struct)
    )
set_Type'struct :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> (m (Type'struct (Message.MutMsg s)))
set_Type'struct (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (16 :: Std_.Word16) 0 0 0)
    (Classes.fromStruct struct)
    )
set_Type'interface :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> (m (Type'interface (Message.MutMsg s)))
set_Type'interface (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (17 :: Std_.Word16) 0 0 0)
    (Classes.fromStruct struct)
    )
set_Type'anyPointer :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> (m (Type'anyPointer (Message.MutMsg s)))
set_Type'anyPointer (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (18 :: Std_.Word16) 0 0 0)
    (Classes.fromStruct struct)
    )
set_Type'unknown' :: ((Untyped.RWCtx m s)) => (Type (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Type'unknown' (Type'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
newtype Type'list msg
    = Type'list'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Type'list) where
    tMsg f (Type'list'newtype_ s) = (Type'list'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Type'list msg)) where
    fromStruct struct = (Std_.pure (Type'list'newtype_ struct))
instance (Classes.ToStruct msg (Type'list msg)) where
    toStruct (Type'list'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'list msg)) where
    type InMessage (Type'list msg) = msg
    message (Type'list'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'list msg)) where
    messageDefault msg = (Type'list'newtype_ (Untyped.messageDefault msg))
get_Type'list'elementType :: ((Untyped.ReadCtx m msg)) => (Type'list msg) -> (m (Type msg))
get_Type'list'elementType (Type'list'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Type'list'elementType :: ((Untyped.RWCtx m s)) => (Type'list (Message.MutMsg s)) -> (Type (Message.MutMsg s)) -> (m ())
set_Type'list'elementType (Type'list'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Type'list'elementType :: ((Untyped.ReadCtx m msg)) => (Type'list msg) -> (m Std_.Bool)
has_Type'list'elementType (Type'list'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Type'list'elementType :: ((Untyped.RWCtx m s)) => (Type'list (Message.MutMsg s)) -> (m (Type (Message.MutMsg s)))
new_Type'list'elementType struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Type'list'elementType struct result)
    (Std_.pure result)
    )
newtype Type'enum msg
    = Type'enum'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Type'enum) where
    tMsg f (Type'enum'newtype_ s) = (Type'enum'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Type'enum msg)) where
    fromStruct struct = (Std_.pure (Type'enum'newtype_ struct))
instance (Classes.ToStruct msg (Type'enum msg)) where
    toStruct (Type'enum'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'enum msg)) where
    type InMessage (Type'enum msg) = msg
    message (Type'enum'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'enum msg)) where
    messageDefault msg = (Type'enum'newtype_ (Untyped.messageDefault msg))
get_Type'enum'typeId :: ((Untyped.ReadCtx m msg)) => (Type'enum msg) -> (m Std_.Word64)
get_Type'enum'typeId (Type'enum'newtype_ struct) = (GenHelpers.getWordField struct 1 0 0)
set_Type'enum'typeId :: ((Untyped.RWCtx m s)) => (Type'enum (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_Type'enum'typeId (Type'enum'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
get_Type'enum'brand :: ((Untyped.ReadCtx m msg)) => (Type'enum msg) -> (m (Brand msg))
get_Type'enum'brand (Type'enum'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Type'enum'brand :: ((Untyped.RWCtx m s)) => (Type'enum (Message.MutMsg s)) -> (Brand (Message.MutMsg s)) -> (m ())
set_Type'enum'brand (Type'enum'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Type'enum'brand :: ((Untyped.ReadCtx m msg)) => (Type'enum msg) -> (m Std_.Bool)
has_Type'enum'brand (Type'enum'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Type'enum'brand :: ((Untyped.RWCtx m s)) => (Type'enum (Message.MutMsg s)) -> (m (Brand (Message.MutMsg s)))
new_Type'enum'brand struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Type'enum'brand struct result)
    (Std_.pure result)
    )
newtype Type'struct msg
    = Type'struct'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Type'struct) where
    tMsg f (Type'struct'newtype_ s) = (Type'struct'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Type'struct msg)) where
    fromStruct struct = (Std_.pure (Type'struct'newtype_ struct))
instance (Classes.ToStruct msg (Type'struct msg)) where
    toStruct (Type'struct'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'struct msg)) where
    type InMessage (Type'struct msg) = msg
    message (Type'struct'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'struct msg)) where
    messageDefault msg = (Type'struct'newtype_ (Untyped.messageDefault msg))
get_Type'struct'typeId :: ((Untyped.ReadCtx m msg)) => (Type'struct msg) -> (m Std_.Word64)
get_Type'struct'typeId (Type'struct'newtype_ struct) = (GenHelpers.getWordField struct 1 0 0)
set_Type'struct'typeId :: ((Untyped.RWCtx m s)) => (Type'struct (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_Type'struct'typeId (Type'struct'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
get_Type'struct'brand :: ((Untyped.ReadCtx m msg)) => (Type'struct msg) -> (m (Brand msg))
get_Type'struct'brand (Type'struct'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Type'struct'brand :: ((Untyped.RWCtx m s)) => (Type'struct (Message.MutMsg s)) -> (Brand (Message.MutMsg s)) -> (m ())
set_Type'struct'brand (Type'struct'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Type'struct'brand :: ((Untyped.ReadCtx m msg)) => (Type'struct msg) -> (m Std_.Bool)
has_Type'struct'brand (Type'struct'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Type'struct'brand :: ((Untyped.RWCtx m s)) => (Type'struct (Message.MutMsg s)) -> (m (Brand (Message.MutMsg s)))
new_Type'struct'brand struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Type'struct'brand struct result)
    (Std_.pure result)
    )
newtype Type'interface msg
    = Type'interface'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Type'interface) where
    tMsg f (Type'interface'newtype_ s) = (Type'interface'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Type'interface msg)) where
    fromStruct struct = (Std_.pure (Type'interface'newtype_ struct))
instance (Classes.ToStruct msg (Type'interface msg)) where
    toStruct (Type'interface'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'interface msg)) where
    type InMessage (Type'interface msg) = msg
    message (Type'interface'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'interface msg)) where
    messageDefault msg = (Type'interface'newtype_ (Untyped.messageDefault msg))
get_Type'interface'typeId :: ((Untyped.ReadCtx m msg)) => (Type'interface msg) -> (m Std_.Word64)
get_Type'interface'typeId (Type'interface'newtype_ struct) = (GenHelpers.getWordField struct 1 0 0)
set_Type'interface'typeId :: ((Untyped.RWCtx m s)) => (Type'interface (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_Type'interface'typeId (Type'interface'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
get_Type'interface'brand :: ((Untyped.ReadCtx m msg)) => (Type'interface msg) -> (m (Brand msg))
get_Type'interface'brand (Type'interface'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Type'interface'brand :: ((Untyped.RWCtx m s)) => (Type'interface (Message.MutMsg s)) -> (Brand (Message.MutMsg s)) -> (m ())
set_Type'interface'brand (Type'interface'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Type'interface'brand :: ((Untyped.ReadCtx m msg)) => (Type'interface msg) -> (m Std_.Bool)
has_Type'interface'brand (Type'interface'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Type'interface'brand :: ((Untyped.RWCtx m s)) => (Type'interface (Message.MutMsg s)) -> (m (Brand (Message.MutMsg s)))
new_Type'interface'brand struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Type'interface'brand struct result)
    (Std_.pure result)
    )
newtype Type'anyPointer msg
    = Type'anyPointer'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Type'anyPointer) where
    tMsg f (Type'anyPointer'newtype_ s) = (Type'anyPointer'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Type'anyPointer msg)) where
    fromStruct struct = (Std_.pure (Type'anyPointer'newtype_ struct))
instance (Classes.ToStruct msg (Type'anyPointer msg)) where
    toStruct (Type'anyPointer'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'anyPointer msg)) where
    type InMessage (Type'anyPointer msg) = msg
    message (Type'anyPointer'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'anyPointer msg)) where
    messageDefault msg = (Type'anyPointer'newtype_ (Untyped.messageDefault msg))
data Type'anyPointer' msg
    = Type'anyPointer'unconstrained (Type'anyPointer'unconstrained msg)
    | Type'anyPointer'parameter (Type'anyPointer'parameter msg)
    | Type'anyPointer'implicitMethodParameter (Type'anyPointer'implicitMethodParameter msg)
    | Type'anyPointer'unknown' Std_.Word16
instance (Classes.FromStruct msg (Type'anyPointer' msg)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 4)
        case tag of
            0 ->
                (Type'anyPointer'unconstrained <$> (Classes.fromStruct struct))
            1 ->
                (Type'anyPointer'parameter <$> (Classes.fromStruct struct))
            2 ->
                (Type'anyPointer'implicitMethodParameter <$> (Classes.fromStruct struct))
            _ ->
                (Std_.pure (Type'anyPointer'unknown' (Std_.fromIntegral tag)))
        )
get_Type'anyPointer' :: ((Untyped.ReadCtx m msg)) => (Type'anyPointer msg) -> (m (Type'anyPointer' msg))
get_Type'anyPointer' (Type'anyPointer'newtype_ struct) = (Classes.fromStruct struct)
set_Type'anyPointer'unconstrained :: ((Untyped.RWCtx m s)) => (Type'anyPointer (Message.MutMsg s)) -> (m (Type'anyPointer'unconstrained (Message.MutMsg s)))
set_Type'anyPointer'unconstrained (Type'anyPointer'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 1 0 0)
    (Classes.fromStruct struct)
    )
set_Type'anyPointer'parameter :: ((Untyped.RWCtx m s)) => (Type'anyPointer (Message.MutMsg s)) -> (m (Type'anyPointer'parameter (Message.MutMsg s)))
set_Type'anyPointer'parameter (Type'anyPointer'newtype_ struct) = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 1 0 0)
    (Classes.fromStruct struct)
    )
set_Type'anyPointer'implicitMethodParameter :: ((Untyped.RWCtx m s)) => (Type'anyPointer (Message.MutMsg s)) -> (m (Type'anyPointer'implicitMethodParameter (Message.MutMsg s)))
set_Type'anyPointer'implicitMethodParameter (Type'anyPointer'newtype_ struct) = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 1 0 0)
    (Classes.fromStruct struct)
    )
set_Type'anyPointer'unknown' :: ((Untyped.RWCtx m s)) => (Type'anyPointer (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Type'anyPointer'unknown' (Type'anyPointer'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 0 0)
newtype Type'anyPointer'unconstrained msg
    = Type'anyPointer'unconstrained'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Type'anyPointer'unconstrained) where
    tMsg f (Type'anyPointer'unconstrained'newtype_ s) = (Type'anyPointer'unconstrained'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Type'anyPointer'unconstrained msg)) where
    fromStruct struct = (Std_.pure (Type'anyPointer'unconstrained'newtype_ struct))
instance (Classes.ToStruct msg (Type'anyPointer'unconstrained msg)) where
    toStruct (Type'anyPointer'unconstrained'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'anyPointer'unconstrained msg)) where
    type InMessage (Type'anyPointer'unconstrained msg) = msg
    message (Type'anyPointer'unconstrained'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'anyPointer'unconstrained msg)) where
    messageDefault msg = (Type'anyPointer'unconstrained'newtype_ (Untyped.messageDefault msg))
data Type'anyPointer'unconstrained' msg
    = Type'anyPointer'unconstrained'anyKind 
    | Type'anyPointer'unconstrained'struct 
    | Type'anyPointer'unconstrained'list 
    | Type'anyPointer'unconstrained'capability 
    | Type'anyPointer'unconstrained'unknown' Std_.Word16
instance (Classes.FromStruct msg (Type'anyPointer'unconstrained' msg)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 5)
        case tag of
            0 ->
                (Std_.pure Type'anyPointer'unconstrained'anyKind)
            1 ->
                (Std_.pure Type'anyPointer'unconstrained'struct)
            2 ->
                (Std_.pure Type'anyPointer'unconstrained'list)
            3 ->
                (Std_.pure Type'anyPointer'unconstrained'capability)
            _ ->
                (Std_.pure (Type'anyPointer'unconstrained'unknown' (Std_.fromIntegral tag)))
        )
get_Type'anyPointer'unconstrained' :: ((Untyped.ReadCtx m msg)) => (Type'anyPointer'unconstrained msg) -> (m (Type'anyPointer'unconstrained' msg))
get_Type'anyPointer'unconstrained' (Type'anyPointer'unconstrained'newtype_ struct) = (Classes.fromStruct struct)
set_Type'anyPointer'unconstrained'anyKind :: ((Untyped.RWCtx m s)) => (Type'anyPointer'unconstrained (Message.MutMsg s)) -> () -> (m ())
set_Type'anyPointer'unconstrained'anyKind (Type'anyPointer'unconstrained'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 1 16 0)
    (Std_.pure ())
    )
set_Type'anyPointer'unconstrained'struct :: ((Untyped.RWCtx m s)) => (Type'anyPointer'unconstrained (Message.MutMsg s)) -> () -> (m ())
set_Type'anyPointer'unconstrained'struct (Type'anyPointer'unconstrained'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 1 16 0)
    (Std_.pure ())
    )
set_Type'anyPointer'unconstrained'list :: ((Untyped.RWCtx m s)) => (Type'anyPointer'unconstrained (Message.MutMsg s)) -> () -> (m ())
set_Type'anyPointer'unconstrained'list (Type'anyPointer'unconstrained'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 1 16 0)
    (Std_.pure ())
    )
set_Type'anyPointer'unconstrained'capability :: ((Untyped.RWCtx m s)) => (Type'anyPointer'unconstrained (Message.MutMsg s)) -> () -> (m ())
set_Type'anyPointer'unconstrained'capability (Type'anyPointer'unconstrained'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 1 16 0)
    (Std_.pure ())
    )
set_Type'anyPointer'unconstrained'unknown' :: ((Untyped.RWCtx m s)) => (Type'anyPointer'unconstrained (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Type'anyPointer'unconstrained'unknown' (Type'anyPointer'unconstrained'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 16 0)
newtype Type'anyPointer'parameter msg
    = Type'anyPointer'parameter'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Type'anyPointer'parameter) where
    tMsg f (Type'anyPointer'parameter'newtype_ s) = (Type'anyPointer'parameter'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Type'anyPointer'parameter msg)) where
    fromStruct struct = (Std_.pure (Type'anyPointer'parameter'newtype_ struct))
instance (Classes.ToStruct msg (Type'anyPointer'parameter msg)) where
    toStruct (Type'anyPointer'parameter'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'anyPointer'parameter msg)) where
    type InMessage (Type'anyPointer'parameter msg) = msg
    message (Type'anyPointer'parameter'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'anyPointer'parameter msg)) where
    messageDefault msg = (Type'anyPointer'parameter'newtype_ (Untyped.messageDefault msg))
get_Type'anyPointer'parameter'scopeId :: ((Untyped.ReadCtx m msg)) => (Type'anyPointer'parameter msg) -> (m Std_.Word64)
get_Type'anyPointer'parameter'scopeId (Type'anyPointer'parameter'newtype_ struct) = (GenHelpers.getWordField struct 2 0 0)
set_Type'anyPointer'parameter'scopeId :: ((Untyped.RWCtx m s)) => (Type'anyPointer'parameter (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_Type'anyPointer'parameter'scopeId (Type'anyPointer'parameter'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 2 0 0)
get_Type'anyPointer'parameter'parameterIndex :: ((Untyped.ReadCtx m msg)) => (Type'anyPointer'parameter msg) -> (m Std_.Word16)
get_Type'anyPointer'parameter'parameterIndex (Type'anyPointer'parameter'newtype_ struct) = (GenHelpers.getWordField struct 1 16 0)
set_Type'anyPointer'parameter'parameterIndex :: ((Untyped.RWCtx m s)) => (Type'anyPointer'parameter (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Type'anyPointer'parameter'parameterIndex (Type'anyPointer'parameter'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 16 0)
newtype Type'anyPointer'implicitMethodParameter msg
    = Type'anyPointer'implicitMethodParameter'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Type'anyPointer'implicitMethodParameter) where
    tMsg f (Type'anyPointer'implicitMethodParameter'newtype_ s) = (Type'anyPointer'implicitMethodParameter'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Type'anyPointer'implicitMethodParameter msg)) where
    fromStruct struct = (Std_.pure (Type'anyPointer'implicitMethodParameter'newtype_ struct))
instance (Classes.ToStruct msg (Type'anyPointer'implicitMethodParameter msg)) where
    toStruct (Type'anyPointer'implicitMethodParameter'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'anyPointer'implicitMethodParameter msg)) where
    type InMessage (Type'anyPointer'implicitMethodParameter msg) = msg
    message (Type'anyPointer'implicitMethodParameter'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'anyPointer'implicitMethodParameter msg)) where
    messageDefault msg = (Type'anyPointer'implicitMethodParameter'newtype_ (Untyped.messageDefault msg))
get_Type'anyPointer'implicitMethodParameter'parameterIndex :: ((Untyped.ReadCtx m msg)) => (Type'anyPointer'implicitMethodParameter msg) -> (m Std_.Word16)
get_Type'anyPointer'implicitMethodParameter'parameterIndex (Type'anyPointer'implicitMethodParameter'newtype_ struct) = (GenHelpers.getWordField struct 1 16 0)
set_Type'anyPointer'implicitMethodParameter'parameterIndex :: ((Untyped.RWCtx m s)) => (Type'anyPointer'implicitMethodParameter (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Type'anyPointer'implicitMethodParameter'parameterIndex (Type'anyPointer'implicitMethodParameter'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 16 0)
newtype Brand msg
    = Brand'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Brand) where
    tMsg f (Brand'newtype_ s) = (Brand'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Brand msg)) where
    fromStruct struct = (Std_.pure (Brand'newtype_ struct))
instance (Classes.ToStruct msg (Brand msg)) where
    toStruct (Brand'newtype_ struct) = struct
instance (Untyped.HasMessage (Brand msg)) where
    type InMessage (Brand msg) = msg
    message (Brand'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Brand msg)) where
    messageDefault msg = (Brand'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Brand msg)) where
    fromPtr msg ptr = (Brand'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Brand (Message.MutMsg s))) where
    toPtr msg (Brand'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Brand (Message.MutMsg s))) where
    new msg = (Brand'newtype_ <$> (Untyped.allocStruct msg 0 1))
instance (Basics.ListElem msg (Brand msg)) where
    newtype List msg (Brand msg)
        = Brand'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Brand'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Brand'List_ l) = (Untyped.ListStruct l)
    length (Brand'List_ l) = (Untyped.length l)
    index i (Brand'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Brand (Message.MutMsg s))) where
    setIndex (Brand'newtype_ elt) i (Brand'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Brand'List_ <$> (Untyped.allocCompositeList msg 0 1 len))
get_Brand'scopes :: ((Untyped.ReadCtx m msg)) => (Brand msg) -> (m (Basics.List msg (Brand'Scope msg)))
get_Brand'scopes (Brand'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Brand'scopes :: ((Untyped.RWCtx m s)) => (Brand (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Brand'Scope (Message.MutMsg s))) -> (m ())
set_Brand'scopes (Brand'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Brand'scopes :: ((Untyped.ReadCtx m msg)) => (Brand msg) -> (m Std_.Bool)
has_Brand'scopes (Brand'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Brand'scopes :: ((Untyped.RWCtx m s)) => (Brand (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (Brand'Scope (Message.MutMsg s))))
new_Brand'scopes struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Brand'scopes struct result)
    (Std_.pure result)
    )
newtype Brand'Scope msg
    = Brand'Scope'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Brand'Scope) where
    tMsg f (Brand'Scope'newtype_ s) = (Brand'Scope'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Brand'Scope msg)) where
    fromStruct struct = (Std_.pure (Brand'Scope'newtype_ struct))
instance (Classes.ToStruct msg (Brand'Scope msg)) where
    toStruct (Brand'Scope'newtype_ struct) = struct
instance (Untyped.HasMessage (Brand'Scope msg)) where
    type InMessage (Brand'Scope msg) = msg
    message (Brand'Scope'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Brand'Scope msg)) where
    messageDefault msg = (Brand'Scope'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Brand'Scope msg)) where
    fromPtr msg ptr = (Brand'Scope'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Brand'Scope (Message.MutMsg s))) where
    toPtr msg (Brand'Scope'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Brand'Scope (Message.MutMsg s))) where
    new msg = (Brand'Scope'newtype_ <$> (Untyped.allocStruct msg 2 1))
instance (Basics.ListElem msg (Brand'Scope msg)) where
    newtype List msg (Brand'Scope msg)
        = Brand'Scope'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Brand'Scope'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Brand'Scope'List_ l) = (Untyped.ListStruct l)
    length (Brand'Scope'List_ l) = (Untyped.length l)
    index i (Brand'Scope'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Brand'Scope (Message.MutMsg s))) where
    setIndex (Brand'Scope'newtype_ elt) i (Brand'Scope'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Brand'Scope'List_ <$> (Untyped.allocCompositeList msg 2 1 len))
get_Brand'Scope'scopeId :: ((Untyped.ReadCtx m msg)) => (Brand'Scope msg) -> (m Std_.Word64)
get_Brand'Scope'scopeId (Brand'Scope'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Brand'Scope'scopeId :: ((Untyped.RWCtx m s)) => (Brand'Scope (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_Brand'Scope'scopeId (Brand'Scope'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
data Brand'Scope' msg
    = Brand'Scope'bind (Basics.List msg (Brand'Binding msg))
    | Brand'Scope'inherit 
    | Brand'Scope'unknown' Std_.Word16
instance (Classes.FromStruct msg (Brand'Scope' msg)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 4)
        case tag of
            0 ->
                (Brand'Scope'bind <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            1 ->
                (Std_.pure Brand'Scope'inherit)
            _ ->
                (Std_.pure (Brand'Scope'unknown' (Std_.fromIntegral tag)))
        )
get_Brand'Scope' :: ((Untyped.ReadCtx m msg)) => (Brand'Scope msg) -> (m (Brand'Scope' msg))
get_Brand'Scope' (Brand'Scope'newtype_ struct) = (Classes.fromStruct struct)
set_Brand'Scope'bind :: ((Untyped.RWCtx m s)) => (Brand'Scope (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Brand'Binding (Message.MutMsg s))) -> (m ())
set_Brand'Scope'bind (Brand'Scope'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 1 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Brand'Scope'inherit :: ((Untyped.RWCtx m s)) => (Brand'Scope (Message.MutMsg s)) -> () -> (m ())
set_Brand'Scope'inherit (Brand'Scope'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 1 0 0)
    (Std_.pure ())
    )
set_Brand'Scope'unknown' :: ((Untyped.RWCtx m s)) => (Brand'Scope (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Brand'Scope'unknown' (Brand'Scope'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 0 0)
newtype Brand'Binding msg
    = Brand'Binding'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Brand'Binding) where
    tMsg f (Brand'Binding'newtype_ s) = (Brand'Binding'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Brand'Binding msg)) where
    fromStruct struct = (Std_.pure (Brand'Binding'newtype_ struct))
instance (Classes.ToStruct msg (Brand'Binding msg)) where
    toStruct (Brand'Binding'newtype_ struct) = struct
instance (Untyped.HasMessage (Brand'Binding msg)) where
    type InMessage (Brand'Binding msg) = msg
    message (Brand'Binding'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Brand'Binding msg)) where
    messageDefault msg = (Brand'Binding'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Brand'Binding msg)) where
    fromPtr msg ptr = (Brand'Binding'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Brand'Binding (Message.MutMsg s))) where
    toPtr msg (Brand'Binding'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Brand'Binding (Message.MutMsg s))) where
    new msg = (Brand'Binding'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem msg (Brand'Binding msg)) where
    newtype List msg (Brand'Binding msg)
        = Brand'Binding'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Brand'Binding'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Brand'Binding'List_ l) = (Untyped.ListStruct l)
    length (Brand'Binding'List_ l) = (Untyped.length l)
    index i (Brand'Binding'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Brand'Binding (Message.MutMsg s))) where
    setIndex (Brand'Binding'newtype_ elt) i (Brand'Binding'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Brand'Binding'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
data Brand'Binding' msg
    = Brand'Binding'unbound 
    | Brand'Binding'type_ (Type msg)
    | Brand'Binding'unknown' Std_.Word16
instance (Classes.FromStruct msg (Brand'Binding' msg)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 0)
        case tag of
            0 ->
                (Std_.pure Brand'Binding'unbound)
            1 ->
                (Brand'Binding'type_ <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            _ ->
                (Std_.pure (Brand'Binding'unknown' (Std_.fromIntegral tag)))
        )
get_Brand'Binding' :: ((Untyped.ReadCtx m msg)) => (Brand'Binding msg) -> (m (Brand'Binding' msg))
get_Brand'Binding' (Brand'Binding'newtype_ struct) = (Classes.fromStruct struct)
set_Brand'Binding'unbound :: ((Untyped.RWCtx m s)) => (Brand'Binding (Message.MutMsg s)) -> () -> (m ())
set_Brand'Binding'unbound (Brand'Binding'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Brand'Binding'type_ :: ((Untyped.RWCtx m s)) => (Brand'Binding (Message.MutMsg s)) -> (Type (Message.MutMsg s)) -> (m ())
set_Brand'Binding'type_ (Brand'Binding'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Brand'Binding'unknown' :: ((Untyped.RWCtx m s)) => (Brand'Binding (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Brand'Binding'unknown' (Brand'Binding'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
newtype Value msg
    = Value'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Value) where
    tMsg f (Value'newtype_ s) = (Value'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Value msg)) where
    fromStruct struct = (Std_.pure (Value'newtype_ struct))
instance (Classes.ToStruct msg (Value msg)) where
    toStruct (Value'newtype_ struct) = struct
instance (Untyped.HasMessage (Value msg)) where
    type InMessage (Value msg) = msg
    message (Value'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Value msg)) where
    messageDefault msg = (Value'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Value msg)) where
    fromPtr msg ptr = (Value'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Value (Message.MutMsg s))) where
    toPtr msg (Value'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Value (Message.MutMsg s))) where
    new msg = (Value'newtype_ <$> (Untyped.allocStruct msg 2 1))
instance (Basics.ListElem msg (Value msg)) where
    newtype List msg (Value msg)
        = Value'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Value'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Value'List_ l) = (Untyped.ListStruct l)
    length (Value'List_ l) = (Untyped.length l)
    index i (Value'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Value (Message.MutMsg s))) where
    setIndex (Value'newtype_ elt) i (Value'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Value'List_ <$> (Untyped.allocCompositeList msg 2 1 len))
data Value' msg
    = Value'void 
    | Value'bool Std_.Bool
    | Value'int8 Std_.Int8
    | Value'int16 Std_.Int16
    | Value'int32 Std_.Int32
    | Value'int64 Std_.Int64
    | Value'uint8 Std_.Word8
    | Value'uint16 Std_.Word16
    | Value'uint32 Std_.Word32
    | Value'uint64 Std_.Word64
    | Value'float32 Std_.Float
    | Value'float64 Std_.Double
    | Value'text (Basics.Text msg)
    | Value'data_ (Basics.Data msg)
    | Value'list (Std_.Maybe (Untyped.Ptr msg))
    | Value'enum Std_.Word16
    | Value'struct (Std_.Maybe (Untyped.Ptr msg))
    | Value'interface 
    | Value'anyPointer (Std_.Maybe (Untyped.Ptr msg))
    | Value'unknown' Std_.Word16
instance (Classes.FromStruct msg (Value' msg)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 0)
        case tag of
            0 ->
                (Std_.pure Value'void)
            1 ->
                (Value'bool <$> (GenHelpers.getWordField struct 0 16 0))
            2 ->
                (Value'int8 <$> (GenHelpers.getWordField struct 0 16 0))
            3 ->
                (Value'int16 <$> (GenHelpers.getWordField struct 0 16 0))
            4 ->
                (Value'int32 <$> (GenHelpers.getWordField struct 0 32 0))
            5 ->
                (Value'int64 <$> (GenHelpers.getWordField struct 1 0 0))
            6 ->
                (Value'uint8 <$> (GenHelpers.getWordField struct 0 16 0))
            7 ->
                (Value'uint16 <$> (GenHelpers.getWordField struct 0 16 0))
            8 ->
                (Value'uint32 <$> (GenHelpers.getWordField struct 0 32 0))
            9 ->
                (Value'uint64 <$> (GenHelpers.getWordField struct 1 0 0))
            10 ->
                (Value'float32 <$> (GenHelpers.getWordField struct 0 32 0))
            11 ->
                (Value'float64 <$> (GenHelpers.getWordField struct 1 0 0))
            12 ->
                (Value'text <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            13 ->
                (Value'data_ <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            14 ->
                (Value'list <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            15 ->
                (Value'enum <$> (GenHelpers.getWordField struct 0 16 0))
            16 ->
                (Value'struct <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            17 ->
                (Std_.pure Value'interface)
            18 ->
                (Value'anyPointer <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            _ ->
                (Std_.pure (Value'unknown' (Std_.fromIntegral tag)))
        )
get_Value' :: ((Untyped.ReadCtx m msg)) => (Value msg) -> (m (Value' msg))
get_Value' (Value'newtype_ struct) = (Classes.fromStruct struct)
set_Value'void :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> () -> (m ())
set_Value'void (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Value'bool :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> Std_.Bool -> (m ())
set_Value'bool (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 0 16 0)
    )
set_Value'int8 :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> Std_.Int8 -> (m ())
set_Value'int8 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word8) 0 16 0)
    )
set_Value'int16 :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> Std_.Int16 -> (m ())
set_Value'int16 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 16 0)
    )
set_Value'int32 :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> Std_.Int32 -> (m ())
set_Value'int32 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (4 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 32 0)
    )
set_Value'int64 :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> Std_.Int64 -> (m ())
set_Value'int64 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (5 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
    )
set_Value'uint8 :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> Std_.Word8 -> (m ())
set_Value'uint8 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (6 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word8) 0 16 0)
    )
set_Value'uint16 :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Value'uint16 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (7 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 16 0)
    )
set_Value'uint32 :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> Std_.Word32 -> (m ())
set_Value'uint32 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (8 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 32 0)
    )
set_Value'uint64 :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_Value'uint64 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (9 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
    )
set_Value'float32 :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> Std_.Float -> (m ())
set_Value'float32 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (10 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 32 0)
    )
set_Value'float64 :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> Std_.Double -> (m ())
set_Value'float64 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (11 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
    )
set_Value'text :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> (Basics.Text (Message.MutMsg s)) -> (m ())
set_Value'text (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (12 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Value'data_ :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> (Basics.Data (Message.MutMsg s)) -> (m ())
set_Value'data_ (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (13 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Value'list :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> (Std_.Maybe (Untyped.Ptr (Message.MutMsg s))) -> (m ())
set_Value'list (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (14 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Value'enum :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Value'enum (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (15 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 16 0)
    )
set_Value'struct :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> (Std_.Maybe (Untyped.Ptr (Message.MutMsg s))) -> (m ())
set_Value'struct (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (16 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Value'interface :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> () -> (m ())
set_Value'interface (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (17 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Value'anyPointer :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> (Std_.Maybe (Untyped.Ptr (Message.MutMsg s))) -> (m ())
set_Value'anyPointer (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (18 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Value'unknown' :: ((Untyped.RWCtx m s)) => (Value (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_Value'unknown' (Value'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
newtype Annotation msg
    = Annotation'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg Annotation) where
    tMsg f (Annotation'newtype_ s) = (Annotation'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (Annotation msg)) where
    fromStruct struct = (Std_.pure (Annotation'newtype_ struct))
instance (Classes.ToStruct msg (Annotation msg)) where
    toStruct (Annotation'newtype_ struct) = struct
instance (Untyped.HasMessage (Annotation msg)) where
    type InMessage (Annotation msg) = msg
    message (Annotation'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Annotation msg)) where
    messageDefault msg = (Annotation'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Annotation msg)) where
    fromPtr msg ptr = (Annotation'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Annotation (Message.MutMsg s))) where
    toPtr msg (Annotation'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Annotation (Message.MutMsg s))) where
    new msg = (Annotation'newtype_ <$> (Untyped.allocStruct msg 1 2))
instance (Basics.ListElem msg (Annotation msg)) where
    newtype List msg (Annotation msg)
        = Annotation'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (Annotation'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Annotation'List_ l) = (Untyped.ListStruct l)
    length (Annotation'List_ l) = (Untyped.length l)
    index i (Annotation'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Annotation (Message.MutMsg s))) where
    setIndex (Annotation'newtype_ elt) i (Annotation'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Annotation'List_ <$> (Untyped.allocCompositeList msg 1 2 len))
get_Annotation'id :: ((Untyped.ReadCtx m msg)) => (Annotation msg) -> (m Std_.Word64)
get_Annotation'id (Annotation'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Annotation'id :: ((Untyped.RWCtx m s)) => (Annotation (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_Annotation'id (Annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
get_Annotation'value :: ((Untyped.ReadCtx m msg)) => (Annotation msg) -> (m (Value msg))
get_Annotation'value (Annotation'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Annotation'value :: ((Untyped.RWCtx m s)) => (Annotation (Message.MutMsg s)) -> (Value (Message.MutMsg s)) -> (m ())
set_Annotation'value (Annotation'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Annotation'value :: ((Untyped.ReadCtx m msg)) => (Annotation msg) -> (m Std_.Bool)
has_Annotation'value (Annotation'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Annotation'value :: ((Untyped.RWCtx m s)) => (Annotation (Message.MutMsg s)) -> (m (Value (Message.MutMsg s)))
new_Annotation'value struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Annotation'value struct result)
    (Std_.pure result)
    )
get_Annotation'brand :: ((Untyped.ReadCtx m msg)) => (Annotation msg) -> (m (Brand msg))
get_Annotation'brand (Annotation'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Annotation'brand :: ((Untyped.RWCtx m s)) => (Annotation (Message.MutMsg s)) -> (Brand (Message.MutMsg s)) -> (m ())
set_Annotation'brand (Annotation'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Annotation'brand :: ((Untyped.ReadCtx m msg)) => (Annotation msg) -> (m Std_.Bool)
has_Annotation'brand (Annotation'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Annotation'brand :: ((Untyped.RWCtx m s)) => (Annotation (Message.MutMsg s)) -> (m (Brand (Message.MutMsg s)))
new_Annotation'brand struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Annotation'brand struct result)
    (Std_.pure result)
    )
data ElementSize 
    = ElementSize'empty 
    | ElementSize'bit 
    | ElementSize'byte 
    | ElementSize'twoBytes 
    | ElementSize'fourBytes 
    | ElementSize'eightBytes 
    | ElementSize'pointer 
    | ElementSize'inlineComposite 
    | ElementSize'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Read
            ,Std_.Eq
            ,Generics.Generic)
instance (Classes.IsWord ElementSize) where
    fromWord n = case ((Std_.fromIntegral n) :: Std_.Word16) of
        0 ->
            ElementSize'empty
        1 ->
            ElementSize'bit
        2 ->
            ElementSize'byte
        3 ->
            ElementSize'twoBytes
        4 ->
            ElementSize'fourBytes
        5 ->
            ElementSize'eightBytes
        6 ->
            ElementSize'pointer
        7 ->
            ElementSize'inlineComposite
        tag ->
            (ElementSize'unknown' tag)
    toWord (ElementSize'empty) = 0
    toWord (ElementSize'bit) = 1
    toWord (ElementSize'byte) = 2
    toWord (ElementSize'twoBytes) = 3
    toWord (ElementSize'fourBytes) = 4
    toWord (ElementSize'eightBytes) = 5
    toWord (ElementSize'pointer) = 6
    toWord (ElementSize'inlineComposite) = 7
    toWord (ElementSize'unknown' tag) = (Std_.fromIntegral tag)
instance (Std_.Enum ElementSize) where
    fromEnum x = (Std_.fromIntegral (Classes.toWord x))
    toEnum x = (Classes.fromWord (Std_.fromIntegral x))
instance (Basics.ListElem msg ElementSize) where
    newtype List msg ElementSize
        = ElementSize'List_ (Untyped.ListOf msg Std_.Word16)
    index i (ElementSize'List_ l) = (Classes.fromWord <$> (Std_.fromIntegral <$> (Untyped.index i l)))
    listFromPtr msg ptr = (ElementSize'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (ElementSize'List_ l) = (Untyped.List16 l)
    length (ElementSize'List_ l) = (Untyped.length l)
instance (Classes.MutListElem s ElementSize) where
    setIndex elt i (ElementSize'List_ l) = (Untyped.setIndex (Std_.fromIntegral (Classes.toWord elt)) i l)
    newList msg size = (ElementSize'List_ <$> (Untyped.allocList16 msg size))
newtype CapnpVersion msg
    = CapnpVersion'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg CapnpVersion) where
    tMsg f (CapnpVersion'newtype_ s) = (CapnpVersion'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (CapnpVersion msg)) where
    fromStruct struct = (Std_.pure (CapnpVersion'newtype_ struct))
instance (Classes.ToStruct msg (CapnpVersion msg)) where
    toStruct (CapnpVersion'newtype_ struct) = struct
instance (Untyped.HasMessage (CapnpVersion msg)) where
    type InMessage (CapnpVersion msg) = msg
    message (CapnpVersion'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (CapnpVersion msg)) where
    messageDefault msg = (CapnpVersion'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (CapnpVersion msg)) where
    fromPtr msg ptr = (CapnpVersion'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (CapnpVersion (Message.MutMsg s))) where
    toPtr msg (CapnpVersion'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (CapnpVersion (Message.MutMsg s))) where
    new msg = (CapnpVersion'newtype_ <$> (Untyped.allocStruct msg 1 0))
instance (Basics.ListElem msg (CapnpVersion msg)) where
    newtype List msg (CapnpVersion msg)
        = CapnpVersion'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (CapnpVersion'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (CapnpVersion'List_ l) = (Untyped.ListStruct l)
    length (CapnpVersion'List_ l) = (Untyped.length l)
    index i (CapnpVersion'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (CapnpVersion (Message.MutMsg s))) where
    setIndex (CapnpVersion'newtype_ elt) i (CapnpVersion'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (CapnpVersion'List_ <$> (Untyped.allocCompositeList msg 1 0 len))
get_CapnpVersion'major :: ((Untyped.ReadCtx m msg)) => (CapnpVersion msg) -> (m Std_.Word16)
get_CapnpVersion'major (CapnpVersion'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_CapnpVersion'major :: ((Untyped.RWCtx m s)) => (CapnpVersion (Message.MutMsg s)) -> Std_.Word16 -> (m ())
set_CapnpVersion'major (CapnpVersion'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
get_CapnpVersion'minor :: ((Untyped.ReadCtx m msg)) => (CapnpVersion msg) -> (m Std_.Word8)
get_CapnpVersion'minor (CapnpVersion'newtype_ struct) = (GenHelpers.getWordField struct 0 16 0)
set_CapnpVersion'minor :: ((Untyped.RWCtx m s)) => (CapnpVersion (Message.MutMsg s)) -> Std_.Word8 -> (m ())
set_CapnpVersion'minor (CapnpVersion'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word8) 0 16 0)
get_CapnpVersion'micro :: ((Untyped.ReadCtx m msg)) => (CapnpVersion msg) -> (m Std_.Word8)
get_CapnpVersion'micro (CapnpVersion'newtype_ struct) = (GenHelpers.getWordField struct 0 24 0)
set_CapnpVersion'micro :: ((Untyped.RWCtx m s)) => (CapnpVersion (Message.MutMsg s)) -> Std_.Word8 -> (m ())
set_CapnpVersion'micro (CapnpVersion'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word8) 0 24 0)
newtype CodeGeneratorRequest msg
    = CodeGeneratorRequest'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg CodeGeneratorRequest) where
    tMsg f (CodeGeneratorRequest'newtype_ s) = (CodeGeneratorRequest'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (CodeGeneratorRequest msg)) where
    fromStruct struct = (Std_.pure (CodeGeneratorRequest'newtype_ struct))
instance (Classes.ToStruct msg (CodeGeneratorRequest msg)) where
    toStruct (CodeGeneratorRequest'newtype_ struct) = struct
instance (Untyped.HasMessage (CodeGeneratorRequest msg)) where
    type InMessage (CodeGeneratorRequest msg) = msg
    message (CodeGeneratorRequest'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (CodeGeneratorRequest msg)) where
    messageDefault msg = (CodeGeneratorRequest'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (CodeGeneratorRequest msg)) where
    fromPtr msg ptr = (CodeGeneratorRequest'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (CodeGeneratorRequest (Message.MutMsg s))) where
    toPtr msg (CodeGeneratorRequest'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (CodeGeneratorRequest (Message.MutMsg s))) where
    new msg = (CodeGeneratorRequest'newtype_ <$> (Untyped.allocStruct msg 0 3))
instance (Basics.ListElem msg (CodeGeneratorRequest msg)) where
    newtype List msg (CodeGeneratorRequest msg)
        = CodeGeneratorRequest'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (CodeGeneratorRequest'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (CodeGeneratorRequest'List_ l) = (Untyped.ListStruct l)
    length (CodeGeneratorRequest'List_ l) = (Untyped.length l)
    index i (CodeGeneratorRequest'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (CodeGeneratorRequest (Message.MutMsg s))) where
    setIndex (CodeGeneratorRequest'newtype_ elt) i (CodeGeneratorRequest'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (CodeGeneratorRequest'List_ <$> (Untyped.allocCompositeList msg 0 3 len))
get_CodeGeneratorRequest'nodes :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest msg) -> (m (Basics.List msg (Node msg)))
get_CodeGeneratorRequest'nodes (CodeGeneratorRequest'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_CodeGeneratorRequest'nodes :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (Node (Message.MutMsg s))) -> (m ())
set_CodeGeneratorRequest'nodes (CodeGeneratorRequest'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_CodeGeneratorRequest'nodes :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest msg) -> (m Std_.Bool)
has_CodeGeneratorRequest'nodes (CodeGeneratorRequest'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_CodeGeneratorRequest'nodes :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (Node (Message.MutMsg s))))
new_CodeGeneratorRequest'nodes struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_CodeGeneratorRequest'nodes struct result)
    (Std_.pure result)
    )
get_CodeGeneratorRequest'requestedFiles :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest msg) -> (m (Basics.List msg (CodeGeneratorRequest'RequestedFile msg)))
get_CodeGeneratorRequest'requestedFiles (CodeGeneratorRequest'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_CodeGeneratorRequest'requestedFiles :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (CodeGeneratorRequest'RequestedFile (Message.MutMsg s))) -> (m ())
set_CodeGeneratorRequest'requestedFiles (CodeGeneratorRequest'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_CodeGeneratorRequest'requestedFiles :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest msg) -> (m Std_.Bool)
has_CodeGeneratorRequest'requestedFiles (CodeGeneratorRequest'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_CodeGeneratorRequest'requestedFiles :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (CodeGeneratorRequest'RequestedFile (Message.MutMsg s))))
new_CodeGeneratorRequest'requestedFiles struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_CodeGeneratorRequest'requestedFiles struct result)
    (Std_.pure result)
    )
get_CodeGeneratorRequest'capnpVersion :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest msg) -> (m (CapnpVersion msg))
get_CodeGeneratorRequest'capnpVersion (CodeGeneratorRequest'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 2 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_CodeGeneratorRequest'capnpVersion :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest (Message.MutMsg s)) -> (CapnpVersion (Message.MutMsg s)) -> (m ())
set_CodeGeneratorRequest'capnpVersion (CodeGeneratorRequest'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 2 struct)
    )
has_CodeGeneratorRequest'capnpVersion :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest msg) -> (m Std_.Bool)
has_CodeGeneratorRequest'capnpVersion (CodeGeneratorRequest'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 2 struct))
new_CodeGeneratorRequest'capnpVersion :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest (Message.MutMsg s)) -> (m (CapnpVersion (Message.MutMsg s)))
new_CodeGeneratorRequest'capnpVersion struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_CodeGeneratorRequest'capnpVersion struct result)
    (Std_.pure result)
    )
newtype CodeGeneratorRequest'RequestedFile msg
    = CodeGeneratorRequest'RequestedFile'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg CodeGeneratorRequest'RequestedFile) where
    tMsg f (CodeGeneratorRequest'RequestedFile'newtype_ s) = (CodeGeneratorRequest'RequestedFile'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (CodeGeneratorRequest'RequestedFile msg)) where
    fromStruct struct = (Std_.pure (CodeGeneratorRequest'RequestedFile'newtype_ struct))
instance (Classes.ToStruct msg (CodeGeneratorRequest'RequestedFile msg)) where
    toStruct (CodeGeneratorRequest'RequestedFile'newtype_ struct) = struct
instance (Untyped.HasMessage (CodeGeneratorRequest'RequestedFile msg)) where
    type InMessage (CodeGeneratorRequest'RequestedFile msg) = msg
    message (CodeGeneratorRequest'RequestedFile'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (CodeGeneratorRequest'RequestedFile msg)) where
    messageDefault msg = (CodeGeneratorRequest'RequestedFile'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (CodeGeneratorRequest'RequestedFile msg)) where
    fromPtr msg ptr = (CodeGeneratorRequest'RequestedFile'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (CodeGeneratorRequest'RequestedFile (Message.MutMsg s))) where
    toPtr msg (CodeGeneratorRequest'RequestedFile'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (CodeGeneratorRequest'RequestedFile (Message.MutMsg s))) where
    new msg = (CodeGeneratorRequest'RequestedFile'newtype_ <$> (Untyped.allocStruct msg 1 2))
instance (Basics.ListElem msg (CodeGeneratorRequest'RequestedFile msg)) where
    newtype List msg (CodeGeneratorRequest'RequestedFile msg)
        = CodeGeneratorRequest'RequestedFile'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (CodeGeneratorRequest'RequestedFile'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (CodeGeneratorRequest'RequestedFile'List_ l) = (Untyped.ListStruct l)
    length (CodeGeneratorRequest'RequestedFile'List_ l) = (Untyped.length l)
    index i (CodeGeneratorRequest'RequestedFile'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (CodeGeneratorRequest'RequestedFile (Message.MutMsg s))) where
    setIndex (CodeGeneratorRequest'RequestedFile'newtype_ elt) i (CodeGeneratorRequest'RequestedFile'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (CodeGeneratorRequest'RequestedFile'List_ <$> (Untyped.allocCompositeList msg 1 2 len))
get_CodeGeneratorRequest'RequestedFile'id :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest'RequestedFile msg) -> (m Std_.Word64)
get_CodeGeneratorRequest'RequestedFile'id (CodeGeneratorRequest'RequestedFile'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_CodeGeneratorRequest'RequestedFile'id :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest'RequestedFile (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_CodeGeneratorRequest'RequestedFile'id (CodeGeneratorRequest'RequestedFile'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
get_CodeGeneratorRequest'RequestedFile'filename :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest'RequestedFile msg) -> (m (Basics.Text msg))
get_CodeGeneratorRequest'RequestedFile'filename (CodeGeneratorRequest'RequestedFile'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_CodeGeneratorRequest'RequestedFile'filename :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest'RequestedFile (Message.MutMsg s)) -> (Basics.Text (Message.MutMsg s)) -> (m ())
set_CodeGeneratorRequest'RequestedFile'filename (CodeGeneratorRequest'RequestedFile'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_CodeGeneratorRequest'RequestedFile'filename :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest'RequestedFile msg) -> (m Std_.Bool)
has_CodeGeneratorRequest'RequestedFile'filename (CodeGeneratorRequest'RequestedFile'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_CodeGeneratorRequest'RequestedFile'filename :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest'RequestedFile (Message.MutMsg s)) -> Std_.Int -> (m (Basics.Text (Message.MutMsg s)))
new_CodeGeneratorRequest'RequestedFile'filename struct len = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_CodeGeneratorRequest'RequestedFile'filename struct result)
    (Std_.pure result)
    )
get_CodeGeneratorRequest'RequestedFile'imports :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest'RequestedFile msg) -> (m (Basics.List msg (CodeGeneratorRequest'RequestedFile'Import msg)))
get_CodeGeneratorRequest'RequestedFile'imports (CodeGeneratorRequest'RequestedFile'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_CodeGeneratorRequest'RequestedFile'imports :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest'RequestedFile (Message.MutMsg s)) -> (Basics.List (Message.MutMsg s) (CodeGeneratorRequest'RequestedFile'Import (Message.MutMsg s))) -> (m ())
set_CodeGeneratorRequest'RequestedFile'imports (CodeGeneratorRequest'RequestedFile'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_CodeGeneratorRequest'RequestedFile'imports :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest'RequestedFile msg) -> (m Std_.Bool)
has_CodeGeneratorRequest'RequestedFile'imports (CodeGeneratorRequest'RequestedFile'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_CodeGeneratorRequest'RequestedFile'imports :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest'RequestedFile (Message.MutMsg s)) -> Std_.Int -> (m (Basics.List (Message.MutMsg s) (CodeGeneratorRequest'RequestedFile'Import (Message.MutMsg s))))
new_CodeGeneratorRequest'RequestedFile'imports struct len = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_CodeGeneratorRequest'RequestedFile'imports struct result)
    (Std_.pure result)
    )
newtype CodeGeneratorRequest'RequestedFile'Import msg
    = CodeGeneratorRequest'RequestedFile'Import'newtype_ (Untyped.Struct msg)
instance (Untyped.TraverseMsg CodeGeneratorRequest'RequestedFile'Import) where
    tMsg f (CodeGeneratorRequest'RequestedFile'Import'newtype_ s) = (CodeGeneratorRequest'RequestedFile'Import'newtype_ <$> (Untyped.tMsg f s))
instance (Classes.FromStruct msg (CodeGeneratorRequest'RequestedFile'Import msg)) where
    fromStruct struct = (Std_.pure (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct))
instance (Classes.ToStruct msg (CodeGeneratorRequest'RequestedFile'Import msg)) where
    toStruct (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) = struct
instance (Untyped.HasMessage (CodeGeneratorRequest'RequestedFile'Import msg)) where
    type InMessage (CodeGeneratorRequest'RequestedFile'Import msg) = msg
    message (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (CodeGeneratorRequest'RequestedFile'Import msg)) where
    messageDefault msg = (CodeGeneratorRequest'RequestedFile'Import'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (CodeGeneratorRequest'RequestedFile'Import msg)) where
    fromPtr msg ptr = (CodeGeneratorRequest'RequestedFile'Import'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (CodeGeneratorRequest'RequestedFile'Import (Message.MutMsg s))) where
    toPtr msg (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (CodeGeneratorRequest'RequestedFile'Import (Message.MutMsg s))) where
    new msg = (CodeGeneratorRequest'RequestedFile'Import'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem msg (CodeGeneratorRequest'RequestedFile'Import msg)) where
    newtype List msg (CodeGeneratorRequest'RequestedFile'Import msg)
        = CodeGeneratorRequest'RequestedFile'Import'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (CodeGeneratorRequest'RequestedFile'Import'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (CodeGeneratorRequest'RequestedFile'Import'List_ l) = (Untyped.ListStruct l)
    length (CodeGeneratorRequest'RequestedFile'Import'List_ l) = (Untyped.length l)
    index i (CodeGeneratorRequest'RequestedFile'Import'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (CodeGeneratorRequest'RequestedFile'Import (Message.MutMsg s))) where
    setIndex (CodeGeneratorRequest'RequestedFile'Import'newtype_ elt) i (CodeGeneratorRequest'RequestedFile'Import'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (CodeGeneratorRequest'RequestedFile'Import'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_CodeGeneratorRequest'RequestedFile'Import'id :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest'RequestedFile'Import msg) -> (m Std_.Word64)
get_CodeGeneratorRequest'RequestedFile'Import'id (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_CodeGeneratorRequest'RequestedFile'Import'id :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest'RequestedFile'Import (Message.MutMsg s)) -> Std_.Word64 -> (m ())
set_CodeGeneratorRequest'RequestedFile'Import'id (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
get_CodeGeneratorRequest'RequestedFile'Import'name :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest'RequestedFile'Import msg) -> (m (Basics.Text msg))
get_CodeGeneratorRequest'RequestedFile'Import'name (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_CodeGeneratorRequest'RequestedFile'Import'name :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest'RequestedFile'Import (Message.MutMsg s)) -> (Basics.Text (Message.MutMsg s)) -> (m ())
set_CodeGeneratorRequest'RequestedFile'Import'name (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_CodeGeneratorRequest'RequestedFile'Import'name :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest'RequestedFile'Import msg) -> (m Std_.Bool)
has_CodeGeneratorRequest'RequestedFile'Import'name (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_CodeGeneratorRequest'RequestedFile'Import'name :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest'RequestedFile'Import (Message.MutMsg s)) -> Std_.Int -> (m (Basics.Text (Message.MutMsg s)))
new_CodeGeneratorRequest'RequestedFile'Import'name struct len = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_CodeGeneratorRequest'RequestedFile'Import'name struct result)
    (Std_.pure result)
    )