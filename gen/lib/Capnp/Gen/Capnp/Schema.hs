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
module Capnp.Gen.Capnp.Schema where
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
newtype Node msg
    = Node'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Node msg)) where
    fromStruct struct = (Std_.pure (Node'newtype_ struct))
instance (Classes.ToStruct msg (Node msg)) where
    toStruct (Node'newtype_ struct) = struct
instance (Untyped.HasMessage (Node mut) mut) where
    message (Node'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node mut) mut) where
    messageDefault msg = (Node'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Node msg)) where
    fromPtr msg ptr = (Node'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Node (Message.Mut s))) where
    toPtr msg (Node'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Node (Message.Mut s))) where
    new msg = (Node'newtype_ <$> (Untyped.allocStruct msg 5 6))
instance (Basics.ListElem mut (Node mut)) where
    newtype List mut (Node mut)
        = Node'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Node'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Node'List_ l) = (Untyped.ListStruct l)
    length (Node'List_ l) = (Untyped.length l)
    index i (Node'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Node (Message.Mut s))) where
    setIndex (Node'newtype_ elt) i (Node'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Node'List_ <$> (Untyped.allocCompositeList msg 5 6 len))
get_Node'id :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Word64)
get_Node'id (Node'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Node'id :: ((Untyped.RWCtx m s)) => (Node (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Node'id (Node'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
get_Node'displayName :: ((Untyped.ReadCtx m msg)
                        ,(Classes.FromPtr msg (Basics.Text msg))) => (Node msg) -> (m (Basics.Text msg))
get_Node'displayName (Node'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'displayName :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (Node (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_Node'displayName (Node'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Node'displayName :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Bool)
has_Node'displayName (Node'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Node'displayName :: ((Untyped.RWCtx m s)) => Std_.Int -> (Node (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_Node'displayName len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Node'displayName struct result)
    (Std_.pure result)
    )
get_Node'displayNamePrefixLength :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Word32)
get_Node'displayNamePrefixLength (Node'newtype_ struct) = (GenHelpers.getWordField struct 1 0 0)
set_Node'displayNamePrefixLength :: ((Untyped.RWCtx m s)) => (Node (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Node'displayNamePrefixLength (Node'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 1 0 0)
get_Node'scopeId :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Word64)
get_Node'scopeId (Node'newtype_ struct) = (GenHelpers.getWordField struct 2 0 0)
set_Node'scopeId :: ((Untyped.RWCtx m s)) => (Node (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Node'scopeId (Node'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 2 0 0)
get_Node'nestedNodes :: ((Untyped.ReadCtx m msg)
                        ,(Classes.FromPtr msg (Basics.List msg (Node'NestedNode msg)))) => (Node msg) -> (m (Basics.List msg (Node'NestedNode msg)))
get_Node'nestedNodes (Node'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'nestedNodes :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Node'NestedNode (Message.Mut s))))) => (Node (Message.Mut s)) -> (Basics.List (Message.Mut s) (Node'NestedNode (Message.Mut s))) -> (m ())
set_Node'nestedNodes (Node'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Node'nestedNodes :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Bool)
has_Node'nestedNodes (Node'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Node'nestedNodes :: ((Untyped.RWCtx m s)) => Std_.Int -> (Node (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Node'NestedNode (Message.Mut s))))
new_Node'nestedNodes len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'nestedNodes struct result)
    (Std_.pure result)
    )
get_Node'annotations :: ((Untyped.ReadCtx m msg)
                        ,(Classes.FromPtr msg (Basics.List msg (Annotation msg)))) => (Node msg) -> (m (Basics.List msg (Annotation msg)))
get_Node'annotations (Node'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 2 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'annotations :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Annotation (Message.Mut s))))) => (Node (Message.Mut s)) -> (Basics.List (Message.Mut s) (Annotation (Message.Mut s))) -> (m ())
set_Node'annotations (Node'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 2 struct)
    )
has_Node'annotations :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Bool)
has_Node'annotations (Node'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 2 struct))
new_Node'annotations :: ((Untyped.RWCtx m s)) => Std_.Int -> (Node (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Annotation (Message.Mut s))))
new_Node'annotations len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'annotations struct result)
    (Std_.pure result)
    )
get_Node'parameters :: ((Untyped.ReadCtx m msg)
                       ,(Classes.FromPtr msg (Basics.List msg (Node'Parameter msg)))) => (Node msg) -> (m (Basics.List msg (Node'Parameter msg)))
get_Node'parameters (Node'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 5 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'parameters :: ((Untyped.RWCtx m s)
                       ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Node'Parameter (Message.Mut s))))) => (Node (Message.Mut s)) -> (Basics.List (Message.Mut s) (Node'Parameter (Message.Mut s))) -> (m ())
set_Node'parameters (Node'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 5 struct)
    )
has_Node'parameters :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Bool)
has_Node'parameters (Node'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 5 struct))
new_Node'parameters :: ((Untyped.RWCtx m s)) => Std_.Int -> (Node (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Node'Parameter (Message.Mut s))))
new_Node'parameters len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'parameters struct result)
    (Std_.pure result)
    )
get_Node'isGeneric :: ((Untyped.ReadCtx m msg)) => (Node msg) -> (m Std_.Bool)
get_Node'isGeneric (Node'newtype_ struct) = (GenHelpers.getWordField struct 4 32 0)
set_Node'isGeneric :: ((Untyped.RWCtx m s)) => (Node (Message.Mut s)) -> Std_.Bool -> (m ())
set_Node'isGeneric (Node'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 4 32 0)
data Node' (mut :: Message.Mutability)
    = Node'file 
    | Node'struct (Node'struct mut)
    | Node'enum (Node'enum mut)
    | Node'interface (Node'interface mut)
    | Node'const (Node'const mut)
    | Node'annotation (Node'annotation mut)
    | Node'unknown' Std_.Word16
instance (Classes.FromStruct mut (Node' mut)) where
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
get_Node' :: ((Untyped.ReadCtx m msg)
             ,(Classes.FromStruct msg (Node' msg))) => (Node msg) -> (m (Node' msg))
get_Node' (Node'newtype_ struct) = (Classes.fromStruct struct)
set_Node'file :: ((Untyped.RWCtx m s)) => (Node (Message.Mut s)) -> (m ())
set_Node'file (Node'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 1 32 0)
    (Std_.pure ())
    )
set_Node'struct :: ((Untyped.RWCtx m s)
                   ,(Classes.FromStruct (Message.Mut s) (Node'struct (Message.Mut s)))) => (Node (Message.Mut s)) -> (m (Node'struct (Message.Mut s)))
set_Node'struct (Node'newtype_ struct) = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 1 32 0)
    (Classes.fromStruct struct)
    )
set_Node'enum :: ((Untyped.RWCtx m s)
                 ,(Classes.FromStruct (Message.Mut s) (Node'enum (Message.Mut s)))) => (Node (Message.Mut s)) -> (m (Node'enum (Message.Mut s)))
set_Node'enum (Node'newtype_ struct) = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 1 32 0)
    (Classes.fromStruct struct)
    )
set_Node'interface :: ((Untyped.RWCtx m s)
                      ,(Classes.FromStruct (Message.Mut s) (Node'interface (Message.Mut s)))) => (Node (Message.Mut s)) -> (m (Node'interface (Message.Mut s)))
set_Node'interface (Node'newtype_ struct) = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 1 32 0)
    (Classes.fromStruct struct)
    )
set_Node'const :: ((Untyped.RWCtx m s)
                  ,(Classes.FromStruct (Message.Mut s) (Node'const (Message.Mut s)))) => (Node (Message.Mut s)) -> (m (Node'const (Message.Mut s)))
set_Node'const (Node'newtype_ struct) = (do
    (GenHelpers.setWordField struct (4 :: Std_.Word16) 1 32 0)
    (Classes.fromStruct struct)
    )
set_Node'annotation :: ((Untyped.RWCtx m s)
                       ,(Classes.FromStruct (Message.Mut s) (Node'annotation (Message.Mut s)))) => (Node (Message.Mut s)) -> (m (Node'annotation (Message.Mut s)))
set_Node'annotation (Node'newtype_ struct) = (do
    (GenHelpers.setWordField struct (5 :: Std_.Word16) 1 32 0)
    (Classes.fromStruct struct)
    )
set_Node'unknown' :: ((Untyped.RWCtx m s)) => (Node (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Node'unknown' (Node'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 32 0)
newtype Node'struct msg
    = Node'struct'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Node'struct msg)) where
    fromStruct struct = (Std_.pure (Node'struct'newtype_ struct))
instance (Classes.ToStruct msg (Node'struct msg)) where
    toStruct (Node'struct'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'struct mut) mut) where
    message (Node'struct'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'struct mut) mut) where
    messageDefault msg = (Node'struct'newtype_ <$> (Untyped.messageDefault msg))
get_Node'struct'dataWordCount :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m Std_.Word16)
get_Node'struct'dataWordCount (Node'struct'newtype_ struct) = (GenHelpers.getWordField struct 1 48 0)
set_Node'struct'dataWordCount :: ((Untyped.RWCtx m s)) => (Node'struct (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Node'struct'dataWordCount (Node'struct'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 48 0)
get_Node'struct'pointerCount :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m Std_.Word16)
get_Node'struct'pointerCount (Node'struct'newtype_ struct) = (GenHelpers.getWordField struct 3 0 0)
set_Node'struct'pointerCount :: ((Untyped.RWCtx m s)) => (Node'struct (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Node'struct'pointerCount (Node'struct'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 3 0 0)
get_Node'struct'preferredListEncoding :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m ElementSize)
get_Node'struct'preferredListEncoding (Node'struct'newtype_ struct) = (GenHelpers.getWordField struct 3 16 0)
set_Node'struct'preferredListEncoding :: ((Untyped.RWCtx m s)) => (Node'struct (Message.Mut s)) -> ElementSize -> (m ())
set_Node'struct'preferredListEncoding (Node'struct'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 3 16 0)
get_Node'struct'isGroup :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m Std_.Bool)
get_Node'struct'isGroup (Node'struct'newtype_ struct) = (GenHelpers.getWordField struct 3 32 0)
set_Node'struct'isGroup :: ((Untyped.RWCtx m s)) => (Node'struct (Message.Mut s)) -> Std_.Bool -> (m ())
set_Node'struct'isGroup (Node'struct'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 3 32 0)
get_Node'struct'discriminantCount :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m Std_.Word16)
get_Node'struct'discriminantCount (Node'struct'newtype_ struct) = (GenHelpers.getWordField struct 3 48 0)
set_Node'struct'discriminantCount :: ((Untyped.RWCtx m s)) => (Node'struct (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Node'struct'discriminantCount (Node'struct'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 3 48 0)
get_Node'struct'discriminantOffset :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m Std_.Word32)
get_Node'struct'discriminantOffset (Node'struct'newtype_ struct) = (GenHelpers.getWordField struct 4 0 0)
set_Node'struct'discriminantOffset :: ((Untyped.RWCtx m s)) => (Node'struct (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Node'struct'discriminantOffset (Node'struct'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 4 0 0)
get_Node'struct'fields :: ((Untyped.ReadCtx m msg)
                          ,(Classes.FromPtr msg (Basics.List msg (Field msg)))) => (Node'struct msg) -> (m (Basics.List msg (Field msg)))
get_Node'struct'fields (Node'struct'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'struct'fields :: ((Untyped.RWCtx m s)
                          ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Field (Message.Mut s))))) => (Node'struct (Message.Mut s)) -> (Basics.List (Message.Mut s) (Field (Message.Mut s))) -> (m ())
set_Node'struct'fields (Node'struct'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_Node'struct'fields :: ((Untyped.ReadCtx m msg)) => (Node'struct msg) -> (m Std_.Bool)
has_Node'struct'fields (Node'struct'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_Node'struct'fields :: ((Untyped.RWCtx m s)) => Std_.Int -> (Node'struct (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Field (Message.Mut s))))
new_Node'struct'fields len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'struct'fields struct result)
    (Std_.pure result)
    )
newtype Node'enum msg
    = Node'enum'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Node'enum msg)) where
    fromStruct struct = (Std_.pure (Node'enum'newtype_ struct))
instance (Classes.ToStruct msg (Node'enum msg)) where
    toStruct (Node'enum'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'enum mut) mut) where
    message (Node'enum'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'enum mut) mut) where
    messageDefault msg = (Node'enum'newtype_ <$> (Untyped.messageDefault msg))
get_Node'enum'enumerants :: ((Untyped.ReadCtx m msg)
                            ,(Classes.FromPtr msg (Basics.List msg (Enumerant msg)))) => (Node'enum msg) -> (m (Basics.List msg (Enumerant msg)))
get_Node'enum'enumerants (Node'enum'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'enum'enumerants :: ((Untyped.RWCtx m s)
                            ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Enumerant (Message.Mut s))))) => (Node'enum (Message.Mut s)) -> (Basics.List (Message.Mut s) (Enumerant (Message.Mut s))) -> (m ())
set_Node'enum'enumerants (Node'enum'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_Node'enum'enumerants :: ((Untyped.ReadCtx m msg)) => (Node'enum msg) -> (m Std_.Bool)
has_Node'enum'enumerants (Node'enum'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_Node'enum'enumerants :: ((Untyped.RWCtx m s)) => Std_.Int -> (Node'enum (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Enumerant (Message.Mut s))))
new_Node'enum'enumerants len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'enum'enumerants struct result)
    (Std_.pure result)
    )
newtype Node'interface msg
    = Node'interface'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Node'interface msg)) where
    fromStruct struct = (Std_.pure (Node'interface'newtype_ struct))
instance (Classes.ToStruct msg (Node'interface msg)) where
    toStruct (Node'interface'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'interface mut) mut) where
    message (Node'interface'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'interface mut) mut) where
    messageDefault msg = (Node'interface'newtype_ <$> (Untyped.messageDefault msg))
get_Node'interface'methods :: ((Untyped.ReadCtx m msg)
                              ,(Classes.FromPtr msg (Basics.List msg (Method msg)))) => (Node'interface msg) -> (m (Basics.List msg (Method msg)))
get_Node'interface'methods (Node'interface'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'interface'methods :: ((Untyped.RWCtx m s)
                              ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Method (Message.Mut s))))) => (Node'interface (Message.Mut s)) -> (Basics.List (Message.Mut s) (Method (Message.Mut s))) -> (m ())
set_Node'interface'methods (Node'interface'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_Node'interface'methods :: ((Untyped.ReadCtx m msg)) => (Node'interface msg) -> (m Std_.Bool)
has_Node'interface'methods (Node'interface'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_Node'interface'methods :: ((Untyped.RWCtx m s)) => Std_.Int -> (Node'interface (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Method (Message.Mut s))))
new_Node'interface'methods len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'interface'methods struct result)
    (Std_.pure result)
    )
get_Node'interface'superclasses :: ((Untyped.ReadCtx m msg)
                                   ,(Classes.FromPtr msg (Basics.List msg (Superclass msg)))) => (Node'interface msg) -> (m (Basics.List msg (Superclass msg)))
get_Node'interface'superclasses (Node'interface'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 4 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'interface'superclasses :: ((Untyped.RWCtx m s)
                                   ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Superclass (Message.Mut s))))) => (Node'interface (Message.Mut s)) -> (Basics.List (Message.Mut s) (Superclass (Message.Mut s))) -> (m ())
set_Node'interface'superclasses (Node'interface'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 4 struct)
    )
has_Node'interface'superclasses :: ((Untyped.ReadCtx m msg)) => (Node'interface msg) -> (m Std_.Bool)
has_Node'interface'superclasses (Node'interface'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 4 struct))
new_Node'interface'superclasses :: ((Untyped.RWCtx m s)) => Std_.Int -> (Node'interface (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Superclass (Message.Mut s))))
new_Node'interface'superclasses len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'interface'superclasses struct result)
    (Std_.pure result)
    )
newtype Node'const msg
    = Node'const'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Node'const msg)) where
    fromStruct struct = (Std_.pure (Node'const'newtype_ struct))
instance (Classes.ToStruct msg (Node'const msg)) where
    toStruct (Node'const'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'const mut) mut) where
    message (Node'const'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'const mut) mut) where
    messageDefault msg = (Node'const'newtype_ <$> (Untyped.messageDefault msg))
get_Node'const'type_ :: ((Untyped.ReadCtx m msg)
                        ,(Classes.FromPtr msg (Type msg))) => (Node'const msg) -> (m (Type msg))
get_Node'const'type_ (Node'const'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'const'type_ :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Type (Message.Mut s)))) => (Node'const (Message.Mut s)) -> (Type (Message.Mut s)) -> (m ())
set_Node'const'type_ (Node'const'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_Node'const'type_ :: ((Untyped.ReadCtx m msg)) => (Node'const msg) -> (m Std_.Bool)
has_Node'const'type_ (Node'const'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_Node'const'type_ :: ((Untyped.RWCtx m s)) => (Node'const (Message.Mut s)) -> (m (Type (Message.Mut s)))
new_Node'const'type_ struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Node'const'type_ struct result)
    (Std_.pure result)
    )
get_Node'const'value :: ((Untyped.ReadCtx m msg)
                        ,(Classes.FromPtr msg (Value msg))) => (Node'const msg) -> (m (Value msg))
get_Node'const'value (Node'const'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 4 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'const'value :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Value (Message.Mut s)))) => (Node'const (Message.Mut s)) -> (Value (Message.Mut s)) -> (m ())
set_Node'const'value (Node'const'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 4 struct)
    )
has_Node'const'value :: ((Untyped.ReadCtx m msg)) => (Node'const msg) -> (m Std_.Bool)
has_Node'const'value (Node'const'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 4 struct))
new_Node'const'value :: ((Untyped.RWCtx m s)) => (Node'const (Message.Mut s)) -> (m (Value (Message.Mut s)))
new_Node'const'value struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Node'const'value struct result)
    (Std_.pure result)
    )
newtype Node'annotation msg
    = Node'annotation'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Node'annotation msg)) where
    fromStruct struct = (Std_.pure (Node'annotation'newtype_ struct))
instance (Classes.ToStruct msg (Node'annotation msg)) where
    toStruct (Node'annotation'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'annotation mut) mut) where
    message (Node'annotation'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'annotation mut) mut) where
    messageDefault msg = (Node'annotation'newtype_ <$> (Untyped.messageDefault msg))
get_Node'annotation'type_ :: ((Untyped.ReadCtx m msg)
                             ,(Classes.FromPtr msg (Type msg))) => (Node'annotation msg) -> (m (Type msg))
get_Node'annotation'type_ (Node'annotation'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'annotation'type_ :: ((Untyped.RWCtx m s)
                             ,(Classes.ToPtr s (Type (Message.Mut s)))) => (Node'annotation (Message.Mut s)) -> (Type (Message.Mut s)) -> (m ())
set_Node'annotation'type_ (Node'annotation'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_Node'annotation'type_ :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
has_Node'annotation'type_ (Node'annotation'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_Node'annotation'type_ :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.Mut s)) -> (m (Type (Message.Mut s)))
new_Node'annotation'type_ struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Node'annotation'type_ struct result)
    (Std_.pure result)
    )
get_Node'annotation'targetsFile :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsFile (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 48 0)
set_Node'annotation'targetsFile :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.Mut s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsFile (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 48 0)
get_Node'annotation'targetsConst :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsConst (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 49 0)
set_Node'annotation'targetsConst :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.Mut s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsConst (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 49 0)
get_Node'annotation'targetsEnum :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsEnum (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 50 0)
set_Node'annotation'targetsEnum :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.Mut s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsEnum (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 50 0)
get_Node'annotation'targetsEnumerant :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsEnumerant (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 51 0)
set_Node'annotation'targetsEnumerant :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.Mut s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsEnumerant (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 51 0)
get_Node'annotation'targetsStruct :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsStruct (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 52 0)
set_Node'annotation'targetsStruct :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.Mut s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsStruct (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 52 0)
get_Node'annotation'targetsField :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsField (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 53 0)
set_Node'annotation'targetsField :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.Mut s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsField (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 53 0)
get_Node'annotation'targetsUnion :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsUnion (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 54 0)
set_Node'annotation'targetsUnion :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.Mut s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsUnion (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 54 0)
get_Node'annotation'targetsGroup :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsGroup (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 55 0)
set_Node'annotation'targetsGroup :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.Mut s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsGroup (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 55 0)
get_Node'annotation'targetsInterface :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsInterface (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 56 0)
set_Node'annotation'targetsInterface :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.Mut s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsInterface (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 56 0)
get_Node'annotation'targetsMethod :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsMethod (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 57 0)
set_Node'annotation'targetsMethod :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.Mut s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsMethod (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 57 0)
get_Node'annotation'targetsParam :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsParam (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 58 0)
set_Node'annotation'targetsParam :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.Mut s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsParam (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 58 0)
get_Node'annotation'targetsAnnotation :: ((Untyped.ReadCtx m msg)) => (Node'annotation msg) -> (m Std_.Bool)
get_Node'annotation'targetsAnnotation (Node'annotation'newtype_ struct) = (GenHelpers.getWordField struct 1 59 0)
set_Node'annotation'targetsAnnotation :: ((Untyped.RWCtx m s)) => (Node'annotation (Message.Mut s)) -> Std_.Bool -> (m ())
set_Node'annotation'targetsAnnotation (Node'annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 1 59 0)
newtype Node'Parameter msg
    = Node'Parameter'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Node'Parameter msg)) where
    fromStruct struct = (Std_.pure (Node'Parameter'newtype_ struct))
instance (Classes.ToStruct msg (Node'Parameter msg)) where
    toStruct (Node'Parameter'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'Parameter mut) mut) where
    message (Node'Parameter'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'Parameter mut) mut) where
    messageDefault msg = (Node'Parameter'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Node'Parameter msg)) where
    fromPtr msg ptr = (Node'Parameter'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Node'Parameter (Message.Mut s))) where
    toPtr msg (Node'Parameter'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Node'Parameter (Message.Mut s))) where
    new msg = (Node'Parameter'newtype_ <$> (Untyped.allocStruct msg 0 1))
instance (Basics.ListElem mut (Node'Parameter mut)) where
    newtype List mut (Node'Parameter mut)
        = Node'Parameter'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Node'Parameter'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Node'Parameter'List_ l) = (Untyped.ListStruct l)
    length (Node'Parameter'List_ l) = (Untyped.length l)
    index i (Node'Parameter'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Node'Parameter (Message.Mut s))) where
    setIndex (Node'Parameter'newtype_ elt) i (Node'Parameter'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Node'Parameter'List_ <$> (Untyped.allocCompositeList msg 0 1 len))
get_Node'Parameter'name :: ((Untyped.ReadCtx m msg)
                           ,(Classes.FromPtr msg (Basics.Text msg))) => (Node'Parameter msg) -> (m (Basics.Text msg))
get_Node'Parameter'name (Node'Parameter'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'Parameter'name :: ((Untyped.RWCtx m s)
                           ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (Node'Parameter (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_Node'Parameter'name (Node'Parameter'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Node'Parameter'name :: ((Untyped.ReadCtx m msg)) => (Node'Parameter msg) -> (m Std_.Bool)
has_Node'Parameter'name (Node'Parameter'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Node'Parameter'name :: ((Untyped.RWCtx m s)) => Std_.Int -> (Node'Parameter (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_Node'Parameter'name len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Node'Parameter'name struct result)
    (Std_.pure result)
    )
newtype Node'NestedNode msg
    = Node'NestedNode'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Node'NestedNode msg)) where
    fromStruct struct = (Std_.pure (Node'NestedNode'newtype_ struct))
instance (Classes.ToStruct msg (Node'NestedNode msg)) where
    toStruct (Node'NestedNode'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'NestedNode mut) mut) where
    message (Node'NestedNode'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'NestedNode mut) mut) where
    messageDefault msg = (Node'NestedNode'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Node'NestedNode msg)) where
    fromPtr msg ptr = (Node'NestedNode'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Node'NestedNode (Message.Mut s))) where
    toPtr msg (Node'NestedNode'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Node'NestedNode (Message.Mut s))) where
    new msg = (Node'NestedNode'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem mut (Node'NestedNode mut)) where
    newtype List mut (Node'NestedNode mut)
        = Node'NestedNode'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Node'NestedNode'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Node'NestedNode'List_ l) = (Untyped.ListStruct l)
    length (Node'NestedNode'List_ l) = (Untyped.length l)
    index i (Node'NestedNode'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Node'NestedNode (Message.Mut s))) where
    setIndex (Node'NestedNode'newtype_ elt) i (Node'NestedNode'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Node'NestedNode'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_Node'NestedNode'name :: ((Untyped.ReadCtx m msg)
                            ,(Classes.FromPtr msg (Basics.Text msg))) => (Node'NestedNode msg) -> (m (Basics.Text msg))
get_Node'NestedNode'name (Node'NestedNode'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'NestedNode'name :: ((Untyped.RWCtx m s)
                            ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (Node'NestedNode (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_Node'NestedNode'name (Node'NestedNode'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Node'NestedNode'name :: ((Untyped.ReadCtx m msg)) => (Node'NestedNode msg) -> (m Std_.Bool)
has_Node'NestedNode'name (Node'NestedNode'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Node'NestedNode'name :: ((Untyped.RWCtx m s)) => Std_.Int -> (Node'NestedNode (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_Node'NestedNode'name len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Node'NestedNode'name struct result)
    (Std_.pure result)
    )
get_Node'NestedNode'id :: ((Untyped.ReadCtx m msg)) => (Node'NestedNode msg) -> (m Std_.Word64)
get_Node'NestedNode'id (Node'NestedNode'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Node'NestedNode'id :: ((Untyped.RWCtx m s)) => (Node'NestedNode (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Node'NestedNode'id (Node'NestedNode'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
newtype Node'SourceInfo msg
    = Node'SourceInfo'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Node'SourceInfo msg)) where
    fromStruct struct = (Std_.pure (Node'SourceInfo'newtype_ struct))
instance (Classes.ToStruct msg (Node'SourceInfo msg)) where
    toStruct (Node'SourceInfo'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'SourceInfo mut) mut) where
    message (Node'SourceInfo'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'SourceInfo mut) mut) where
    messageDefault msg = (Node'SourceInfo'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Node'SourceInfo msg)) where
    fromPtr msg ptr = (Node'SourceInfo'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Node'SourceInfo (Message.Mut s))) where
    toPtr msg (Node'SourceInfo'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Node'SourceInfo (Message.Mut s))) where
    new msg = (Node'SourceInfo'newtype_ <$> (Untyped.allocStruct msg 1 2))
instance (Basics.ListElem mut (Node'SourceInfo mut)) where
    newtype List mut (Node'SourceInfo mut)
        = Node'SourceInfo'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Node'SourceInfo'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Node'SourceInfo'List_ l) = (Untyped.ListStruct l)
    length (Node'SourceInfo'List_ l) = (Untyped.length l)
    index i (Node'SourceInfo'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Node'SourceInfo (Message.Mut s))) where
    setIndex (Node'SourceInfo'newtype_ elt) i (Node'SourceInfo'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Node'SourceInfo'List_ <$> (Untyped.allocCompositeList msg 1 2 len))
get_Node'SourceInfo'id :: ((Untyped.ReadCtx m msg)) => (Node'SourceInfo msg) -> (m Std_.Word64)
get_Node'SourceInfo'id (Node'SourceInfo'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Node'SourceInfo'id :: ((Untyped.RWCtx m s)) => (Node'SourceInfo (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Node'SourceInfo'id (Node'SourceInfo'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
get_Node'SourceInfo'docComment :: ((Untyped.ReadCtx m msg)
                                  ,(Classes.FromPtr msg (Basics.Text msg))) => (Node'SourceInfo msg) -> (m (Basics.Text msg))
get_Node'SourceInfo'docComment (Node'SourceInfo'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'SourceInfo'docComment :: ((Untyped.RWCtx m s)
                                  ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (Node'SourceInfo (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_Node'SourceInfo'docComment (Node'SourceInfo'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Node'SourceInfo'docComment :: ((Untyped.ReadCtx m msg)) => (Node'SourceInfo msg) -> (m Std_.Bool)
has_Node'SourceInfo'docComment (Node'SourceInfo'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Node'SourceInfo'docComment :: ((Untyped.RWCtx m s)) => Std_.Int -> (Node'SourceInfo (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_Node'SourceInfo'docComment len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Node'SourceInfo'docComment struct result)
    (Std_.pure result)
    )
get_Node'SourceInfo'members :: ((Untyped.ReadCtx m msg)
                               ,(Classes.FromPtr msg (Basics.List msg (Node'SourceInfo'Member msg)))) => (Node'SourceInfo msg) -> (m (Basics.List msg (Node'SourceInfo'Member msg)))
get_Node'SourceInfo'members (Node'SourceInfo'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'SourceInfo'members :: ((Untyped.RWCtx m s)
                               ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Node'SourceInfo'Member (Message.Mut s))))) => (Node'SourceInfo (Message.Mut s)) -> (Basics.List (Message.Mut s) (Node'SourceInfo'Member (Message.Mut s))) -> (m ())
set_Node'SourceInfo'members (Node'SourceInfo'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Node'SourceInfo'members :: ((Untyped.ReadCtx m msg)) => (Node'SourceInfo msg) -> (m Std_.Bool)
has_Node'SourceInfo'members (Node'SourceInfo'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Node'SourceInfo'members :: ((Untyped.RWCtx m s)) => Std_.Int -> (Node'SourceInfo (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Node'SourceInfo'Member (Message.Mut s))))
new_Node'SourceInfo'members len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Node'SourceInfo'members struct result)
    (Std_.pure result)
    )
newtype Node'SourceInfo'Member msg
    = Node'SourceInfo'Member'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Node'SourceInfo'Member msg)) where
    fromStruct struct = (Std_.pure (Node'SourceInfo'Member'newtype_ struct))
instance (Classes.ToStruct msg (Node'SourceInfo'Member msg)) where
    toStruct (Node'SourceInfo'Member'newtype_ struct) = struct
instance (Untyped.HasMessage (Node'SourceInfo'Member mut) mut) where
    message (Node'SourceInfo'Member'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Node'SourceInfo'Member mut) mut) where
    messageDefault msg = (Node'SourceInfo'Member'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Node'SourceInfo'Member msg)) where
    fromPtr msg ptr = (Node'SourceInfo'Member'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Node'SourceInfo'Member (Message.Mut s))) where
    toPtr msg (Node'SourceInfo'Member'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Node'SourceInfo'Member (Message.Mut s))) where
    new msg = (Node'SourceInfo'Member'newtype_ <$> (Untyped.allocStruct msg 0 1))
instance (Basics.ListElem mut (Node'SourceInfo'Member mut)) where
    newtype List mut (Node'SourceInfo'Member mut)
        = Node'SourceInfo'Member'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Node'SourceInfo'Member'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Node'SourceInfo'Member'List_ l) = (Untyped.ListStruct l)
    length (Node'SourceInfo'Member'List_ l) = (Untyped.length l)
    index i (Node'SourceInfo'Member'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Node'SourceInfo'Member (Message.Mut s))) where
    setIndex (Node'SourceInfo'Member'newtype_ elt) i (Node'SourceInfo'Member'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Node'SourceInfo'Member'List_ <$> (Untyped.allocCompositeList msg 0 1 len))
get_Node'SourceInfo'Member'docComment :: ((Untyped.ReadCtx m msg)
                                         ,(Classes.FromPtr msg (Basics.Text msg))) => (Node'SourceInfo'Member msg) -> (m (Basics.Text msg))
get_Node'SourceInfo'Member'docComment (Node'SourceInfo'Member'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Node'SourceInfo'Member'docComment :: ((Untyped.RWCtx m s)
                                         ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (Node'SourceInfo'Member (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_Node'SourceInfo'Member'docComment (Node'SourceInfo'Member'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Node'SourceInfo'Member'docComment :: ((Untyped.ReadCtx m msg)) => (Node'SourceInfo'Member msg) -> (m Std_.Bool)
has_Node'SourceInfo'Member'docComment (Node'SourceInfo'Member'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Node'SourceInfo'Member'docComment :: ((Untyped.RWCtx m s)) => Std_.Int -> (Node'SourceInfo'Member (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_Node'SourceInfo'Member'docComment len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Node'SourceInfo'Member'docComment struct result)
    (Std_.pure result)
    )
newtype Field msg
    = Field'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Field msg)) where
    fromStruct struct = (Std_.pure (Field'newtype_ struct))
instance (Classes.ToStruct msg (Field msg)) where
    toStruct (Field'newtype_ struct) = struct
instance (Untyped.HasMessage (Field mut) mut) where
    message (Field'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Field mut) mut) where
    messageDefault msg = (Field'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Field msg)) where
    fromPtr msg ptr = (Field'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Field (Message.Mut s))) where
    toPtr msg (Field'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Field (Message.Mut s))) where
    new msg = (Field'newtype_ <$> (Untyped.allocStruct msg 3 4))
instance (Basics.ListElem mut (Field mut)) where
    newtype List mut (Field mut)
        = Field'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Field'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Field'List_ l) = (Untyped.ListStruct l)
    length (Field'List_ l) = (Untyped.length l)
    index i (Field'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Field (Message.Mut s))) where
    setIndex (Field'newtype_ elt) i (Field'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Field'List_ <$> (Untyped.allocCompositeList msg 3 4 len))
get_Field'name :: ((Untyped.ReadCtx m msg)
                  ,(Classes.FromPtr msg (Basics.Text msg))) => (Field msg) -> (m (Basics.Text msg))
get_Field'name (Field'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Field'name :: ((Untyped.RWCtx m s)
                  ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (Field (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_Field'name (Field'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Field'name :: ((Untyped.ReadCtx m msg)) => (Field msg) -> (m Std_.Bool)
has_Field'name (Field'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Field'name :: ((Untyped.RWCtx m s)) => Std_.Int -> (Field (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_Field'name len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Field'name struct result)
    (Std_.pure result)
    )
get_Field'codeOrder :: ((Untyped.ReadCtx m msg)) => (Field msg) -> (m Std_.Word16)
get_Field'codeOrder (Field'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Field'codeOrder :: ((Untyped.RWCtx m s)) => (Field (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Field'codeOrder (Field'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
get_Field'annotations :: ((Untyped.ReadCtx m msg)
                         ,(Classes.FromPtr msg (Basics.List msg (Annotation msg)))) => (Field msg) -> (m (Basics.List msg (Annotation msg)))
get_Field'annotations (Field'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Field'annotations :: ((Untyped.RWCtx m s)
                         ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Annotation (Message.Mut s))))) => (Field (Message.Mut s)) -> (Basics.List (Message.Mut s) (Annotation (Message.Mut s))) -> (m ())
set_Field'annotations (Field'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Field'annotations :: ((Untyped.ReadCtx m msg)) => (Field msg) -> (m Std_.Bool)
has_Field'annotations (Field'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Field'annotations :: ((Untyped.RWCtx m s)) => Std_.Int -> (Field (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Annotation (Message.Mut s))))
new_Field'annotations len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Field'annotations struct result)
    (Std_.pure result)
    )
get_Field'discriminantValue :: ((Untyped.ReadCtx m msg)) => (Field msg) -> (m Std_.Word16)
get_Field'discriminantValue (Field'newtype_ struct) = (GenHelpers.getWordField struct 0 16 65535)
set_Field'discriminantValue :: ((Untyped.RWCtx m s)) => (Field (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Field'discriminantValue (Field'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 16 65535)
get_Field'ordinal :: ((Untyped.ReadCtx m msg)
                     ,(Classes.FromStruct msg (Field'ordinal msg))) => (Field msg) -> (m (Field'ordinal msg))
get_Field'ordinal (Field'newtype_ struct) = (Classes.fromStruct struct)
data Field' (mut :: Message.Mutability)
    = Field'slot (Field'slot mut)
    | Field'group (Field'group mut)
    | Field'unknown' Std_.Word16
instance (Classes.FromStruct mut (Field' mut)) where
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
get_Field' :: ((Untyped.ReadCtx m msg)
              ,(Classes.FromStruct msg (Field' msg))) => (Field msg) -> (m (Field' msg))
get_Field' (Field'newtype_ struct) = (Classes.fromStruct struct)
set_Field'slot :: ((Untyped.RWCtx m s)
                  ,(Classes.FromStruct (Message.Mut s) (Field'slot (Message.Mut s)))) => (Field (Message.Mut s)) -> (m (Field'slot (Message.Mut s)))
set_Field'slot (Field'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 1 0 0)
    (Classes.fromStruct struct)
    )
set_Field'group :: ((Untyped.RWCtx m s)
                   ,(Classes.FromStruct (Message.Mut s) (Field'group (Message.Mut s)))) => (Field (Message.Mut s)) -> (m (Field'group (Message.Mut s)))
set_Field'group (Field'newtype_ struct) = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 1 0 0)
    (Classes.fromStruct struct)
    )
set_Field'unknown' :: ((Untyped.RWCtx m s)) => (Field (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Field'unknown' (Field'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 0 0)
newtype Field'slot msg
    = Field'slot'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Field'slot msg)) where
    fromStruct struct = (Std_.pure (Field'slot'newtype_ struct))
instance (Classes.ToStruct msg (Field'slot msg)) where
    toStruct (Field'slot'newtype_ struct) = struct
instance (Untyped.HasMessage (Field'slot mut) mut) where
    message (Field'slot'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Field'slot mut) mut) where
    messageDefault msg = (Field'slot'newtype_ <$> (Untyped.messageDefault msg))
get_Field'slot'offset :: ((Untyped.ReadCtx m msg)) => (Field'slot msg) -> (m Std_.Word32)
get_Field'slot'offset (Field'slot'newtype_ struct) = (GenHelpers.getWordField struct 0 32 0)
set_Field'slot'offset :: ((Untyped.RWCtx m s)) => (Field'slot (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Field'slot'offset (Field'slot'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 32 0)
get_Field'slot'type_ :: ((Untyped.ReadCtx m msg)
                        ,(Classes.FromPtr msg (Type msg))) => (Field'slot msg) -> (m (Type msg))
get_Field'slot'type_ (Field'slot'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 2 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Field'slot'type_ :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Type (Message.Mut s)))) => (Field'slot (Message.Mut s)) -> (Type (Message.Mut s)) -> (m ())
set_Field'slot'type_ (Field'slot'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 2 struct)
    )
has_Field'slot'type_ :: ((Untyped.ReadCtx m msg)) => (Field'slot msg) -> (m Std_.Bool)
has_Field'slot'type_ (Field'slot'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 2 struct))
new_Field'slot'type_ :: ((Untyped.RWCtx m s)) => (Field'slot (Message.Mut s)) -> (m (Type (Message.Mut s)))
new_Field'slot'type_ struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Field'slot'type_ struct result)
    (Std_.pure result)
    )
get_Field'slot'defaultValue :: ((Untyped.ReadCtx m msg)
                               ,(Classes.FromPtr msg (Value msg))) => (Field'slot msg) -> (m (Value msg))
get_Field'slot'defaultValue (Field'slot'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Field'slot'defaultValue :: ((Untyped.RWCtx m s)
                               ,(Classes.ToPtr s (Value (Message.Mut s)))) => (Field'slot (Message.Mut s)) -> (Value (Message.Mut s)) -> (m ())
set_Field'slot'defaultValue (Field'slot'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_Field'slot'defaultValue :: ((Untyped.ReadCtx m msg)) => (Field'slot msg) -> (m Std_.Bool)
has_Field'slot'defaultValue (Field'slot'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_Field'slot'defaultValue :: ((Untyped.RWCtx m s)) => (Field'slot (Message.Mut s)) -> (m (Value (Message.Mut s)))
new_Field'slot'defaultValue struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Field'slot'defaultValue struct result)
    (Std_.pure result)
    )
get_Field'slot'hadExplicitDefault :: ((Untyped.ReadCtx m msg)) => (Field'slot msg) -> (m Std_.Bool)
get_Field'slot'hadExplicitDefault (Field'slot'newtype_ struct) = (GenHelpers.getWordField struct 2 0 0)
set_Field'slot'hadExplicitDefault :: ((Untyped.RWCtx m s)) => (Field'slot (Message.Mut s)) -> Std_.Bool -> (m ())
set_Field'slot'hadExplicitDefault (Field'slot'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 2 0 0)
newtype Field'group msg
    = Field'group'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Field'group msg)) where
    fromStruct struct = (Std_.pure (Field'group'newtype_ struct))
instance (Classes.ToStruct msg (Field'group msg)) where
    toStruct (Field'group'newtype_ struct) = struct
instance (Untyped.HasMessage (Field'group mut) mut) where
    message (Field'group'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Field'group mut) mut) where
    messageDefault msg = (Field'group'newtype_ <$> (Untyped.messageDefault msg))
get_Field'group'typeId :: ((Untyped.ReadCtx m msg)) => (Field'group msg) -> (m Std_.Word64)
get_Field'group'typeId (Field'group'newtype_ struct) = (GenHelpers.getWordField struct 2 0 0)
set_Field'group'typeId :: ((Untyped.RWCtx m s)) => (Field'group (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Field'group'typeId (Field'group'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 2 0 0)
newtype Field'ordinal msg
    = Field'ordinal'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Field'ordinal msg)) where
    fromStruct struct = (Std_.pure (Field'ordinal'newtype_ struct))
instance (Classes.ToStruct msg (Field'ordinal msg)) where
    toStruct (Field'ordinal'newtype_ struct) = struct
instance (Untyped.HasMessage (Field'ordinal mut) mut) where
    message (Field'ordinal'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Field'ordinal mut) mut) where
    messageDefault msg = (Field'ordinal'newtype_ <$> (Untyped.messageDefault msg))
data Field'ordinal' (mut :: Message.Mutability)
    = Field'ordinal'implicit 
    | Field'ordinal'explicit Std_.Word16
    | Field'ordinal'unknown' Std_.Word16
instance (Classes.FromStruct mut (Field'ordinal' mut)) where
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
get_Field'ordinal' :: ((Untyped.ReadCtx m msg)
                      ,(Classes.FromStruct msg (Field'ordinal' msg))) => (Field'ordinal msg) -> (m (Field'ordinal' msg))
get_Field'ordinal' (Field'ordinal'newtype_ struct) = (Classes.fromStruct struct)
set_Field'ordinal'implicit :: ((Untyped.RWCtx m s)) => (Field'ordinal (Message.Mut s)) -> (m ())
set_Field'ordinal'implicit (Field'ordinal'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 1 16 0)
    (Std_.pure ())
    )
set_Field'ordinal'explicit :: ((Untyped.RWCtx m s)) => (Field'ordinal (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Field'ordinal'explicit (Field'ordinal'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 1 16 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 32 0)
    )
set_Field'ordinal'unknown' :: ((Untyped.RWCtx m s)) => (Field'ordinal (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Field'ordinal'unknown' (Field'ordinal'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 16 0)
field'noDiscriminant :: Std_.Word16
field'noDiscriminant  = (Classes.fromWord 65535)
newtype Enumerant msg
    = Enumerant'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Enumerant msg)) where
    fromStruct struct = (Std_.pure (Enumerant'newtype_ struct))
instance (Classes.ToStruct msg (Enumerant msg)) where
    toStruct (Enumerant'newtype_ struct) = struct
instance (Untyped.HasMessage (Enumerant mut) mut) where
    message (Enumerant'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Enumerant mut) mut) where
    messageDefault msg = (Enumerant'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Enumerant msg)) where
    fromPtr msg ptr = (Enumerant'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Enumerant (Message.Mut s))) where
    toPtr msg (Enumerant'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Enumerant (Message.Mut s))) where
    new msg = (Enumerant'newtype_ <$> (Untyped.allocStruct msg 1 2))
instance (Basics.ListElem mut (Enumerant mut)) where
    newtype List mut (Enumerant mut)
        = Enumerant'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Enumerant'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Enumerant'List_ l) = (Untyped.ListStruct l)
    length (Enumerant'List_ l) = (Untyped.length l)
    index i (Enumerant'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Enumerant (Message.Mut s))) where
    setIndex (Enumerant'newtype_ elt) i (Enumerant'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Enumerant'List_ <$> (Untyped.allocCompositeList msg 1 2 len))
get_Enumerant'name :: ((Untyped.ReadCtx m msg)
                      ,(Classes.FromPtr msg (Basics.Text msg))) => (Enumerant msg) -> (m (Basics.Text msg))
get_Enumerant'name (Enumerant'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Enumerant'name :: ((Untyped.RWCtx m s)
                      ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (Enumerant (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_Enumerant'name (Enumerant'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Enumerant'name :: ((Untyped.ReadCtx m msg)) => (Enumerant msg) -> (m Std_.Bool)
has_Enumerant'name (Enumerant'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Enumerant'name :: ((Untyped.RWCtx m s)) => Std_.Int -> (Enumerant (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_Enumerant'name len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Enumerant'name struct result)
    (Std_.pure result)
    )
get_Enumerant'codeOrder :: ((Untyped.ReadCtx m msg)) => (Enumerant msg) -> (m Std_.Word16)
get_Enumerant'codeOrder (Enumerant'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Enumerant'codeOrder :: ((Untyped.RWCtx m s)) => (Enumerant (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Enumerant'codeOrder (Enumerant'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
get_Enumerant'annotations :: ((Untyped.ReadCtx m msg)
                             ,(Classes.FromPtr msg (Basics.List msg (Annotation msg)))) => (Enumerant msg) -> (m (Basics.List msg (Annotation msg)))
get_Enumerant'annotations (Enumerant'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Enumerant'annotations :: ((Untyped.RWCtx m s)
                             ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Annotation (Message.Mut s))))) => (Enumerant (Message.Mut s)) -> (Basics.List (Message.Mut s) (Annotation (Message.Mut s))) -> (m ())
set_Enumerant'annotations (Enumerant'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Enumerant'annotations :: ((Untyped.ReadCtx m msg)) => (Enumerant msg) -> (m Std_.Bool)
has_Enumerant'annotations (Enumerant'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Enumerant'annotations :: ((Untyped.RWCtx m s)) => Std_.Int -> (Enumerant (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Annotation (Message.Mut s))))
new_Enumerant'annotations len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Enumerant'annotations struct result)
    (Std_.pure result)
    )
newtype Superclass msg
    = Superclass'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Superclass msg)) where
    fromStruct struct = (Std_.pure (Superclass'newtype_ struct))
instance (Classes.ToStruct msg (Superclass msg)) where
    toStruct (Superclass'newtype_ struct) = struct
instance (Untyped.HasMessage (Superclass mut) mut) where
    message (Superclass'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Superclass mut) mut) where
    messageDefault msg = (Superclass'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Superclass msg)) where
    fromPtr msg ptr = (Superclass'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Superclass (Message.Mut s))) where
    toPtr msg (Superclass'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Superclass (Message.Mut s))) where
    new msg = (Superclass'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem mut (Superclass mut)) where
    newtype List mut (Superclass mut)
        = Superclass'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Superclass'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Superclass'List_ l) = (Untyped.ListStruct l)
    length (Superclass'List_ l) = (Untyped.length l)
    index i (Superclass'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Superclass (Message.Mut s))) where
    setIndex (Superclass'newtype_ elt) i (Superclass'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Superclass'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_Superclass'id :: ((Untyped.ReadCtx m msg)) => (Superclass msg) -> (m Std_.Word64)
get_Superclass'id (Superclass'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Superclass'id :: ((Untyped.RWCtx m s)) => (Superclass (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Superclass'id (Superclass'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
get_Superclass'brand :: ((Untyped.ReadCtx m msg)
                        ,(Classes.FromPtr msg (Brand msg))) => (Superclass msg) -> (m (Brand msg))
get_Superclass'brand (Superclass'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Superclass'brand :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Brand (Message.Mut s)))) => (Superclass (Message.Mut s)) -> (Brand (Message.Mut s)) -> (m ())
set_Superclass'brand (Superclass'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Superclass'brand :: ((Untyped.ReadCtx m msg)) => (Superclass msg) -> (m Std_.Bool)
has_Superclass'brand (Superclass'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Superclass'brand :: ((Untyped.RWCtx m s)) => (Superclass (Message.Mut s)) -> (m (Brand (Message.Mut s)))
new_Superclass'brand struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Superclass'brand struct result)
    (Std_.pure result)
    )
newtype Method msg
    = Method'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Method msg)) where
    fromStruct struct = (Std_.pure (Method'newtype_ struct))
instance (Classes.ToStruct msg (Method msg)) where
    toStruct (Method'newtype_ struct) = struct
instance (Untyped.HasMessage (Method mut) mut) where
    message (Method'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Method mut) mut) where
    messageDefault msg = (Method'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Method msg)) where
    fromPtr msg ptr = (Method'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Method (Message.Mut s))) where
    toPtr msg (Method'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Method (Message.Mut s))) where
    new msg = (Method'newtype_ <$> (Untyped.allocStruct msg 3 5))
instance (Basics.ListElem mut (Method mut)) where
    newtype List mut (Method mut)
        = Method'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Method'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Method'List_ l) = (Untyped.ListStruct l)
    length (Method'List_ l) = (Untyped.length l)
    index i (Method'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Method (Message.Mut s))) where
    setIndex (Method'newtype_ elt) i (Method'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Method'List_ <$> (Untyped.allocCompositeList msg 3 5 len))
get_Method'name :: ((Untyped.ReadCtx m msg)
                   ,(Classes.FromPtr msg (Basics.Text msg))) => (Method msg) -> (m (Basics.Text msg))
get_Method'name (Method'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Method'name :: ((Untyped.RWCtx m s)
                   ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (Method (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_Method'name (Method'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Method'name :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Bool)
has_Method'name (Method'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Method'name :: ((Untyped.RWCtx m s)) => Std_.Int -> (Method (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_Method'name len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Method'name struct result)
    (Std_.pure result)
    )
get_Method'codeOrder :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Word16)
get_Method'codeOrder (Method'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Method'codeOrder :: ((Untyped.RWCtx m s)) => (Method (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Method'codeOrder (Method'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
get_Method'paramStructType :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Word64)
get_Method'paramStructType (Method'newtype_ struct) = (GenHelpers.getWordField struct 1 0 0)
set_Method'paramStructType :: ((Untyped.RWCtx m s)) => (Method (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Method'paramStructType (Method'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
get_Method'resultStructType :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Word64)
get_Method'resultStructType (Method'newtype_ struct) = (GenHelpers.getWordField struct 2 0 0)
set_Method'resultStructType :: ((Untyped.RWCtx m s)) => (Method (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Method'resultStructType (Method'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 2 0 0)
get_Method'annotations :: ((Untyped.ReadCtx m msg)
                          ,(Classes.FromPtr msg (Basics.List msg (Annotation msg)))) => (Method msg) -> (m (Basics.List msg (Annotation msg)))
get_Method'annotations (Method'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Method'annotations :: ((Untyped.RWCtx m s)
                          ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Annotation (Message.Mut s))))) => (Method (Message.Mut s)) -> (Basics.List (Message.Mut s) (Annotation (Message.Mut s))) -> (m ())
set_Method'annotations (Method'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Method'annotations :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Bool)
has_Method'annotations (Method'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Method'annotations :: ((Untyped.RWCtx m s)) => Std_.Int -> (Method (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Annotation (Message.Mut s))))
new_Method'annotations len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Method'annotations struct result)
    (Std_.pure result)
    )
get_Method'paramBrand :: ((Untyped.ReadCtx m msg)
                         ,(Classes.FromPtr msg (Brand msg))) => (Method msg) -> (m (Brand msg))
get_Method'paramBrand (Method'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 2 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Method'paramBrand :: ((Untyped.RWCtx m s)
                         ,(Classes.ToPtr s (Brand (Message.Mut s)))) => (Method (Message.Mut s)) -> (Brand (Message.Mut s)) -> (m ())
set_Method'paramBrand (Method'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 2 struct)
    )
has_Method'paramBrand :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Bool)
has_Method'paramBrand (Method'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 2 struct))
new_Method'paramBrand :: ((Untyped.RWCtx m s)) => (Method (Message.Mut s)) -> (m (Brand (Message.Mut s)))
new_Method'paramBrand struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Method'paramBrand struct result)
    (Std_.pure result)
    )
get_Method'resultBrand :: ((Untyped.ReadCtx m msg)
                          ,(Classes.FromPtr msg (Brand msg))) => (Method msg) -> (m (Brand msg))
get_Method'resultBrand (Method'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Method'resultBrand :: ((Untyped.RWCtx m s)
                          ,(Classes.ToPtr s (Brand (Message.Mut s)))) => (Method (Message.Mut s)) -> (Brand (Message.Mut s)) -> (m ())
set_Method'resultBrand (Method'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_Method'resultBrand :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Bool)
has_Method'resultBrand (Method'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_Method'resultBrand :: ((Untyped.RWCtx m s)) => (Method (Message.Mut s)) -> (m (Brand (Message.Mut s)))
new_Method'resultBrand struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Method'resultBrand struct result)
    (Std_.pure result)
    )
get_Method'implicitParameters :: ((Untyped.ReadCtx m msg)
                                 ,(Classes.FromPtr msg (Basics.List msg (Node'Parameter msg)))) => (Method msg) -> (m (Basics.List msg (Node'Parameter msg)))
get_Method'implicitParameters (Method'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 4 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Method'implicitParameters :: ((Untyped.RWCtx m s)
                                 ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Node'Parameter (Message.Mut s))))) => (Method (Message.Mut s)) -> (Basics.List (Message.Mut s) (Node'Parameter (Message.Mut s))) -> (m ())
set_Method'implicitParameters (Method'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 4 struct)
    )
has_Method'implicitParameters :: ((Untyped.ReadCtx m msg)) => (Method msg) -> (m Std_.Bool)
has_Method'implicitParameters (Method'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 4 struct))
new_Method'implicitParameters :: ((Untyped.RWCtx m s)) => Std_.Int -> (Method (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Node'Parameter (Message.Mut s))))
new_Method'implicitParameters len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Method'implicitParameters struct result)
    (Std_.pure result)
    )
newtype Type msg
    = Type'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Type msg)) where
    fromStruct struct = (Std_.pure (Type'newtype_ struct))
instance (Classes.ToStruct msg (Type msg)) where
    toStruct (Type'newtype_ struct) = struct
instance (Untyped.HasMessage (Type mut) mut) where
    message (Type'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type mut) mut) where
    messageDefault msg = (Type'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Type msg)) where
    fromPtr msg ptr = (Type'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Type (Message.Mut s))) where
    toPtr msg (Type'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Type (Message.Mut s))) where
    new msg = (Type'newtype_ <$> (Untyped.allocStruct msg 3 1))
instance (Basics.ListElem mut (Type mut)) where
    newtype List mut (Type mut)
        = Type'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Type'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Type'List_ l) = (Untyped.ListStruct l)
    length (Type'List_ l) = (Untyped.length l)
    index i (Type'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Type (Message.Mut s))) where
    setIndex (Type'newtype_ elt) i (Type'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Type'List_ <$> (Untyped.allocCompositeList msg 3 1 len))
data Type' (mut :: Message.Mutability)
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
    | Type'list (Type'list mut)
    | Type'enum (Type'enum mut)
    | Type'struct (Type'struct mut)
    | Type'interface (Type'interface mut)
    | Type'anyPointer (Type'anyPointer mut)
    | Type'unknown' Std_.Word16
instance (Classes.FromStruct mut (Type' mut)) where
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
get_Type' :: ((Untyped.ReadCtx m msg)
             ,(Classes.FromStruct msg (Type' msg))) => (Type msg) -> (m (Type' msg))
get_Type' (Type'newtype_ struct) = (Classes.fromStruct struct)
set_Type'void :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> (m ())
set_Type'void (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'bool :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> (m ())
set_Type'bool (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'int8 :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> (m ())
set_Type'int8 (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'int16 :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> (m ())
set_Type'int16 (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'int32 :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> (m ())
set_Type'int32 (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (4 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'int64 :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> (m ())
set_Type'int64 (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (5 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'uint8 :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> (m ())
set_Type'uint8 (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (6 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'uint16 :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> (m ())
set_Type'uint16 (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (7 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'uint32 :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> (m ())
set_Type'uint32 (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (8 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'uint64 :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> (m ())
set_Type'uint64 (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (9 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'float32 :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> (m ())
set_Type'float32 (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (10 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'float64 :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> (m ())
set_Type'float64 (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (11 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'text :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> (m ())
set_Type'text (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (12 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'data_ :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> (m ())
set_Type'data_ (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (13 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Type'list :: ((Untyped.RWCtx m s)
                 ,(Classes.FromStruct (Message.Mut s) (Type'list (Message.Mut s)))) => (Type (Message.Mut s)) -> (m (Type'list (Message.Mut s)))
set_Type'list (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (14 :: Std_.Word16) 0 0 0)
    (Classes.fromStruct struct)
    )
set_Type'enum :: ((Untyped.RWCtx m s)
                 ,(Classes.FromStruct (Message.Mut s) (Type'enum (Message.Mut s)))) => (Type (Message.Mut s)) -> (m (Type'enum (Message.Mut s)))
set_Type'enum (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (15 :: Std_.Word16) 0 0 0)
    (Classes.fromStruct struct)
    )
set_Type'struct :: ((Untyped.RWCtx m s)
                   ,(Classes.FromStruct (Message.Mut s) (Type'struct (Message.Mut s)))) => (Type (Message.Mut s)) -> (m (Type'struct (Message.Mut s)))
set_Type'struct (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (16 :: Std_.Word16) 0 0 0)
    (Classes.fromStruct struct)
    )
set_Type'interface :: ((Untyped.RWCtx m s)
                      ,(Classes.FromStruct (Message.Mut s) (Type'interface (Message.Mut s)))) => (Type (Message.Mut s)) -> (m (Type'interface (Message.Mut s)))
set_Type'interface (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (17 :: Std_.Word16) 0 0 0)
    (Classes.fromStruct struct)
    )
set_Type'anyPointer :: ((Untyped.RWCtx m s)
                       ,(Classes.FromStruct (Message.Mut s) (Type'anyPointer (Message.Mut s)))) => (Type (Message.Mut s)) -> (m (Type'anyPointer (Message.Mut s)))
set_Type'anyPointer (Type'newtype_ struct) = (do
    (GenHelpers.setWordField struct (18 :: Std_.Word16) 0 0 0)
    (Classes.fromStruct struct)
    )
set_Type'unknown' :: ((Untyped.RWCtx m s)) => (Type (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Type'unknown' (Type'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
newtype Type'list msg
    = Type'list'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Type'list msg)) where
    fromStruct struct = (Std_.pure (Type'list'newtype_ struct))
instance (Classes.ToStruct msg (Type'list msg)) where
    toStruct (Type'list'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'list mut) mut) where
    message (Type'list'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'list mut) mut) where
    messageDefault msg = (Type'list'newtype_ <$> (Untyped.messageDefault msg))
get_Type'list'elementType :: ((Untyped.ReadCtx m msg)
                             ,(Classes.FromPtr msg (Type msg))) => (Type'list msg) -> (m (Type msg))
get_Type'list'elementType (Type'list'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Type'list'elementType :: ((Untyped.RWCtx m s)
                             ,(Classes.ToPtr s (Type (Message.Mut s)))) => (Type'list (Message.Mut s)) -> (Type (Message.Mut s)) -> (m ())
set_Type'list'elementType (Type'list'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Type'list'elementType :: ((Untyped.ReadCtx m msg)) => (Type'list msg) -> (m Std_.Bool)
has_Type'list'elementType (Type'list'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Type'list'elementType :: ((Untyped.RWCtx m s)) => (Type'list (Message.Mut s)) -> (m (Type (Message.Mut s)))
new_Type'list'elementType struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Type'list'elementType struct result)
    (Std_.pure result)
    )
newtype Type'enum msg
    = Type'enum'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Type'enum msg)) where
    fromStruct struct = (Std_.pure (Type'enum'newtype_ struct))
instance (Classes.ToStruct msg (Type'enum msg)) where
    toStruct (Type'enum'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'enum mut) mut) where
    message (Type'enum'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'enum mut) mut) where
    messageDefault msg = (Type'enum'newtype_ <$> (Untyped.messageDefault msg))
get_Type'enum'typeId :: ((Untyped.ReadCtx m msg)) => (Type'enum msg) -> (m Std_.Word64)
get_Type'enum'typeId (Type'enum'newtype_ struct) = (GenHelpers.getWordField struct 1 0 0)
set_Type'enum'typeId :: ((Untyped.RWCtx m s)) => (Type'enum (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Type'enum'typeId (Type'enum'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
get_Type'enum'brand :: ((Untyped.ReadCtx m msg)
                       ,(Classes.FromPtr msg (Brand msg))) => (Type'enum msg) -> (m (Brand msg))
get_Type'enum'brand (Type'enum'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Type'enum'brand :: ((Untyped.RWCtx m s)
                       ,(Classes.ToPtr s (Brand (Message.Mut s)))) => (Type'enum (Message.Mut s)) -> (Brand (Message.Mut s)) -> (m ())
set_Type'enum'brand (Type'enum'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Type'enum'brand :: ((Untyped.ReadCtx m msg)) => (Type'enum msg) -> (m Std_.Bool)
has_Type'enum'brand (Type'enum'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Type'enum'brand :: ((Untyped.RWCtx m s)) => (Type'enum (Message.Mut s)) -> (m (Brand (Message.Mut s)))
new_Type'enum'brand struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Type'enum'brand struct result)
    (Std_.pure result)
    )
newtype Type'struct msg
    = Type'struct'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Type'struct msg)) where
    fromStruct struct = (Std_.pure (Type'struct'newtype_ struct))
instance (Classes.ToStruct msg (Type'struct msg)) where
    toStruct (Type'struct'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'struct mut) mut) where
    message (Type'struct'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'struct mut) mut) where
    messageDefault msg = (Type'struct'newtype_ <$> (Untyped.messageDefault msg))
get_Type'struct'typeId :: ((Untyped.ReadCtx m msg)) => (Type'struct msg) -> (m Std_.Word64)
get_Type'struct'typeId (Type'struct'newtype_ struct) = (GenHelpers.getWordField struct 1 0 0)
set_Type'struct'typeId :: ((Untyped.RWCtx m s)) => (Type'struct (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Type'struct'typeId (Type'struct'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
get_Type'struct'brand :: ((Untyped.ReadCtx m msg)
                         ,(Classes.FromPtr msg (Brand msg))) => (Type'struct msg) -> (m (Brand msg))
get_Type'struct'brand (Type'struct'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Type'struct'brand :: ((Untyped.RWCtx m s)
                         ,(Classes.ToPtr s (Brand (Message.Mut s)))) => (Type'struct (Message.Mut s)) -> (Brand (Message.Mut s)) -> (m ())
set_Type'struct'brand (Type'struct'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Type'struct'brand :: ((Untyped.ReadCtx m msg)) => (Type'struct msg) -> (m Std_.Bool)
has_Type'struct'brand (Type'struct'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Type'struct'brand :: ((Untyped.RWCtx m s)) => (Type'struct (Message.Mut s)) -> (m (Brand (Message.Mut s)))
new_Type'struct'brand struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Type'struct'brand struct result)
    (Std_.pure result)
    )
newtype Type'interface msg
    = Type'interface'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Type'interface msg)) where
    fromStruct struct = (Std_.pure (Type'interface'newtype_ struct))
instance (Classes.ToStruct msg (Type'interface msg)) where
    toStruct (Type'interface'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'interface mut) mut) where
    message (Type'interface'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'interface mut) mut) where
    messageDefault msg = (Type'interface'newtype_ <$> (Untyped.messageDefault msg))
get_Type'interface'typeId :: ((Untyped.ReadCtx m msg)) => (Type'interface msg) -> (m Std_.Word64)
get_Type'interface'typeId (Type'interface'newtype_ struct) = (GenHelpers.getWordField struct 1 0 0)
set_Type'interface'typeId :: ((Untyped.RWCtx m s)) => (Type'interface (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Type'interface'typeId (Type'interface'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
get_Type'interface'brand :: ((Untyped.ReadCtx m msg)
                            ,(Classes.FromPtr msg (Brand msg))) => (Type'interface msg) -> (m (Brand msg))
get_Type'interface'brand (Type'interface'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Type'interface'brand :: ((Untyped.RWCtx m s)
                            ,(Classes.ToPtr s (Brand (Message.Mut s)))) => (Type'interface (Message.Mut s)) -> (Brand (Message.Mut s)) -> (m ())
set_Type'interface'brand (Type'interface'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Type'interface'brand :: ((Untyped.ReadCtx m msg)) => (Type'interface msg) -> (m Std_.Bool)
has_Type'interface'brand (Type'interface'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Type'interface'brand :: ((Untyped.RWCtx m s)) => (Type'interface (Message.Mut s)) -> (m (Brand (Message.Mut s)))
new_Type'interface'brand struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Type'interface'brand struct result)
    (Std_.pure result)
    )
newtype Type'anyPointer msg
    = Type'anyPointer'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Type'anyPointer msg)) where
    fromStruct struct = (Std_.pure (Type'anyPointer'newtype_ struct))
instance (Classes.ToStruct msg (Type'anyPointer msg)) where
    toStruct (Type'anyPointer'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'anyPointer mut) mut) where
    message (Type'anyPointer'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'anyPointer mut) mut) where
    messageDefault msg = (Type'anyPointer'newtype_ <$> (Untyped.messageDefault msg))
data Type'anyPointer' (mut :: Message.Mutability)
    = Type'anyPointer'unconstrained (Type'anyPointer'unconstrained mut)
    | Type'anyPointer'parameter (Type'anyPointer'parameter mut)
    | Type'anyPointer'implicitMethodParameter (Type'anyPointer'implicitMethodParameter mut)
    | Type'anyPointer'unknown' Std_.Word16
instance (Classes.FromStruct mut (Type'anyPointer' mut)) where
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
get_Type'anyPointer' :: ((Untyped.ReadCtx m msg)
                        ,(Classes.FromStruct msg (Type'anyPointer' msg))) => (Type'anyPointer msg) -> (m (Type'anyPointer' msg))
get_Type'anyPointer' (Type'anyPointer'newtype_ struct) = (Classes.fromStruct struct)
set_Type'anyPointer'unconstrained :: ((Untyped.RWCtx m s)
                                     ,(Classes.FromStruct (Message.Mut s) (Type'anyPointer'unconstrained (Message.Mut s)))) => (Type'anyPointer (Message.Mut s)) -> (m (Type'anyPointer'unconstrained (Message.Mut s)))
set_Type'anyPointer'unconstrained (Type'anyPointer'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 1 0 0)
    (Classes.fromStruct struct)
    )
set_Type'anyPointer'parameter :: ((Untyped.RWCtx m s)
                                 ,(Classes.FromStruct (Message.Mut s) (Type'anyPointer'parameter (Message.Mut s)))) => (Type'anyPointer (Message.Mut s)) -> (m (Type'anyPointer'parameter (Message.Mut s)))
set_Type'anyPointer'parameter (Type'anyPointer'newtype_ struct) = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 1 0 0)
    (Classes.fromStruct struct)
    )
set_Type'anyPointer'implicitMethodParameter :: ((Untyped.RWCtx m s)
                                               ,(Classes.FromStruct (Message.Mut s) (Type'anyPointer'implicitMethodParameter (Message.Mut s)))) => (Type'anyPointer (Message.Mut s)) -> (m (Type'anyPointer'implicitMethodParameter (Message.Mut s)))
set_Type'anyPointer'implicitMethodParameter (Type'anyPointer'newtype_ struct) = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 1 0 0)
    (Classes.fromStruct struct)
    )
set_Type'anyPointer'unknown' :: ((Untyped.RWCtx m s)) => (Type'anyPointer (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Type'anyPointer'unknown' (Type'anyPointer'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 0 0)
newtype Type'anyPointer'unconstrained msg
    = Type'anyPointer'unconstrained'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Type'anyPointer'unconstrained msg)) where
    fromStruct struct = (Std_.pure (Type'anyPointer'unconstrained'newtype_ struct))
instance (Classes.ToStruct msg (Type'anyPointer'unconstrained msg)) where
    toStruct (Type'anyPointer'unconstrained'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'anyPointer'unconstrained mut) mut) where
    message (Type'anyPointer'unconstrained'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'anyPointer'unconstrained mut) mut) where
    messageDefault msg = (Type'anyPointer'unconstrained'newtype_ <$> (Untyped.messageDefault msg))
data Type'anyPointer'unconstrained' (mut :: Message.Mutability)
    = Type'anyPointer'unconstrained'anyKind 
    | Type'anyPointer'unconstrained'struct 
    | Type'anyPointer'unconstrained'list 
    | Type'anyPointer'unconstrained'capability 
    | Type'anyPointer'unconstrained'unknown' Std_.Word16
instance (Classes.FromStruct mut (Type'anyPointer'unconstrained' mut)) where
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
get_Type'anyPointer'unconstrained' :: ((Untyped.ReadCtx m msg)
                                      ,(Classes.FromStruct msg (Type'anyPointer'unconstrained' msg))) => (Type'anyPointer'unconstrained msg) -> (m (Type'anyPointer'unconstrained' msg))
get_Type'anyPointer'unconstrained' (Type'anyPointer'unconstrained'newtype_ struct) = (Classes.fromStruct struct)
set_Type'anyPointer'unconstrained'anyKind :: ((Untyped.RWCtx m s)) => (Type'anyPointer'unconstrained (Message.Mut s)) -> (m ())
set_Type'anyPointer'unconstrained'anyKind (Type'anyPointer'unconstrained'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 1 16 0)
    (Std_.pure ())
    )
set_Type'anyPointer'unconstrained'struct :: ((Untyped.RWCtx m s)) => (Type'anyPointer'unconstrained (Message.Mut s)) -> (m ())
set_Type'anyPointer'unconstrained'struct (Type'anyPointer'unconstrained'newtype_ struct) = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 1 16 0)
    (Std_.pure ())
    )
set_Type'anyPointer'unconstrained'list :: ((Untyped.RWCtx m s)) => (Type'anyPointer'unconstrained (Message.Mut s)) -> (m ())
set_Type'anyPointer'unconstrained'list (Type'anyPointer'unconstrained'newtype_ struct) = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 1 16 0)
    (Std_.pure ())
    )
set_Type'anyPointer'unconstrained'capability :: ((Untyped.RWCtx m s)) => (Type'anyPointer'unconstrained (Message.Mut s)) -> (m ())
set_Type'anyPointer'unconstrained'capability (Type'anyPointer'unconstrained'newtype_ struct) = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 1 16 0)
    (Std_.pure ())
    )
set_Type'anyPointer'unconstrained'unknown' :: ((Untyped.RWCtx m s)) => (Type'anyPointer'unconstrained (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Type'anyPointer'unconstrained'unknown' (Type'anyPointer'unconstrained'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 16 0)
newtype Type'anyPointer'parameter msg
    = Type'anyPointer'parameter'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Type'anyPointer'parameter msg)) where
    fromStruct struct = (Std_.pure (Type'anyPointer'parameter'newtype_ struct))
instance (Classes.ToStruct msg (Type'anyPointer'parameter msg)) where
    toStruct (Type'anyPointer'parameter'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'anyPointer'parameter mut) mut) where
    message (Type'anyPointer'parameter'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'anyPointer'parameter mut) mut) where
    messageDefault msg = (Type'anyPointer'parameter'newtype_ <$> (Untyped.messageDefault msg))
get_Type'anyPointer'parameter'scopeId :: ((Untyped.ReadCtx m msg)) => (Type'anyPointer'parameter msg) -> (m Std_.Word64)
get_Type'anyPointer'parameter'scopeId (Type'anyPointer'parameter'newtype_ struct) = (GenHelpers.getWordField struct 2 0 0)
set_Type'anyPointer'parameter'scopeId :: ((Untyped.RWCtx m s)) => (Type'anyPointer'parameter (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Type'anyPointer'parameter'scopeId (Type'anyPointer'parameter'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 2 0 0)
get_Type'anyPointer'parameter'parameterIndex :: ((Untyped.ReadCtx m msg)) => (Type'anyPointer'parameter msg) -> (m Std_.Word16)
get_Type'anyPointer'parameter'parameterIndex (Type'anyPointer'parameter'newtype_ struct) = (GenHelpers.getWordField struct 1 16 0)
set_Type'anyPointer'parameter'parameterIndex :: ((Untyped.RWCtx m s)) => (Type'anyPointer'parameter (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Type'anyPointer'parameter'parameterIndex (Type'anyPointer'parameter'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 16 0)
newtype Type'anyPointer'implicitMethodParameter msg
    = Type'anyPointer'implicitMethodParameter'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Type'anyPointer'implicitMethodParameter msg)) where
    fromStruct struct = (Std_.pure (Type'anyPointer'implicitMethodParameter'newtype_ struct))
instance (Classes.ToStruct msg (Type'anyPointer'implicitMethodParameter msg)) where
    toStruct (Type'anyPointer'implicitMethodParameter'newtype_ struct) = struct
instance (Untyped.HasMessage (Type'anyPointer'implicitMethodParameter mut) mut) where
    message (Type'anyPointer'implicitMethodParameter'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Type'anyPointer'implicitMethodParameter mut) mut) where
    messageDefault msg = (Type'anyPointer'implicitMethodParameter'newtype_ <$> (Untyped.messageDefault msg))
get_Type'anyPointer'implicitMethodParameter'parameterIndex :: ((Untyped.ReadCtx m msg)) => (Type'anyPointer'implicitMethodParameter msg) -> (m Std_.Word16)
get_Type'anyPointer'implicitMethodParameter'parameterIndex (Type'anyPointer'implicitMethodParameter'newtype_ struct) = (GenHelpers.getWordField struct 1 16 0)
set_Type'anyPointer'implicitMethodParameter'parameterIndex :: ((Untyped.RWCtx m s)) => (Type'anyPointer'implicitMethodParameter (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Type'anyPointer'implicitMethodParameter'parameterIndex (Type'anyPointer'implicitMethodParameter'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 16 0)
newtype Brand msg
    = Brand'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Brand msg)) where
    fromStruct struct = (Std_.pure (Brand'newtype_ struct))
instance (Classes.ToStruct msg (Brand msg)) where
    toStruct (Brand'newtype_ struct) = struct
instance (Untyped.HasMessage (Brand mut) mut) where
    message (Brand'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Brand mut) mut) where
    messageDefault msg = (Brand'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Brand msg)) where
    fromPtr msg ptr = (Brand'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Brand (Message.Mut s))) where
    toPtr msg (Brand'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Brand (Message.Mut s))) where
    new msg = (Brand'newtype_ <$> (Untyped.allocStruct msg 0 1))
instance (Basics.ListElem mut (Brand mut)) where
    newtype List mut (Brand mut)
        = Brand'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Brand'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Brand'List_ l) = (Untyped.ListStruct l)
    length (Brand'List_ l) = (Untyped.length l)
    index i (Brand'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Brand (Message.Mut s))) where
    setIndex (Brand'newtype_ elt) i (Brand'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Brand'List_ <$> (Untyped.allocCompositeList msg 0 1 len))
get_Brand'scopes :: ((Untyped.ReadCtx m msg)
                    ,(Classes.FromPtr msg (Basics.List msg (Brand'Scope msg)))) => (Brand msg) -> (m (Basics.List msg (Brand'Scope msg)))
get_Brand'scopes (Brand'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Brand'scopes :: ((Untyped.RWCtx m s)
                    ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Brand'Scope (Message.Mut s))))) => (Brand (Message.Mut s)) -> (Basics.List (Message.Mut s) (Brand'Scope (Message.Mut s))) -> (m ())
set_Brand'scopes (Brand'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Brand'scopes :: ((Untyped.ReadCtx m msg)) => (Brand msg) -> (m Std_.Bool)
has_Brand'scopes (Brand'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Brand'scopes :: ((Untyped.RWCtx m s)) => Std_.Int -> (Brand (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Brand'Scope (Message.Mut s))))
new_Brand'scopes len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Brand'scopes struct result)
    (Std_.pure result)
    )
newtype Brand'Scope msg
    = Brand'Scope'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Brand'Scope msg)) where
    fromStruct struct = (Std_.pure (Brand'Scope'newtype_ struct))
instance (Classes.ToStruct msg (Brand'Scope msg)) where
    toStruct (Brand'Scope'newtype_ struct) = struct
instance (Untyped.HasMessage (Brand'Scope mut) mut) where
    message (Brand'Scope'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Brand'Scope mut) mut) where
    messageDefault msg = (Brand'Scope'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Brand'Scope msg)) where
    fromPtr msg ptr = (Brand'Scope'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Brand'Scope (Message.Mut s))) where
    toPtr msg (Brand'Scope'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Brand'Scope (Message.Mut s))) where
    new msg = (Brand'Scope'newtype_ <$> (Untyped.allocStruct msg 2 1))
instance (Basics.ListElem mut (Brand'Scope mut)) where
    newtype List mut (Brand'Scope mut)
        = Brand'Scope'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Brand'Scope'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Brand'Scope'List_ l) = (Untyped.ListStruct l)
    length (Brand'Scope'List_ l) = (Untyped.length l)
    index i (Brand'Scope'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Brand'Scope (Message.Mut s))) where
    setIndex (Brand'Scope'newtype_ elt) i (Brand'Scope'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Brand'Scope'List_ <$> (Untyped.allocCompositeList msg 2 1 len))
get_Brand'Scope'scopeId :: ((Untyped.ReadCtx m msg)) => (Brand'Scope msg) -> (m Std_.Word64)
get_Brand'Scope'scopeId (Brand'Scope'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Brand'Scope'scopeId :: ((Untyped.RWCtx m s)) => (Brand'Scope (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Brand'Scope'scopeId (Brand'Scope'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
data Brand'Scope' (mut :: Message.Mutability)
    = Brand'Scope'bind (Basics.List mut (Brand'Binding mut))
    | Brand'Scope'inherit 
    | Brand'Scope'unknown' Std_.Word16
instance (Classes.FromStruct mut (Brand'Scope' mut)) where
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
get_Brand'Scope' :: ((Untyped.ReadCtx m msg)
                    ,(Classes.FromStruct msg (Brand'Scope' msg))) => (Brand'Scope msg) -> (m (Brand'Scope' msg))
get_Brand'Scope' (Brand'Scope'newtype_ struct) = (Classes.fromStruct struct)
set_Brand'Scope'bind :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Brand'Binding (Message.Mut s))))) => (Brand'Scope (Message.Mut s)) -> (Basics.List (Message.Mut s) (Brand'Binding (Message.Mut s))) -> (m ())
set_Brand'Scope'bind (Brand'Scope'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 1 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Brand'Scope'inherit :: ((Untyped.RWCtx m s)) => (Brand'Scope (Message.Mut s)) -> (m ())
set_Brand'Scope'inherit (Brand'Scope'newtype_ struct) = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 1 0 0)
    (Std_.pure ())
    )
set_Brand'Scope'unknown' :: ((Untyped.RWCtx m s)) => (Brand'Scope (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Brand'Scope'unknown' (Brand'Scope'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 1 0 0)
newtype Brand'Binding msg
    = Brand'Binding'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Brand'Binding msg)) where
    fromStruct struct = (Std_.pure (Brand'Binding'newtype_ struct))
instance (Classes.ToStruct msg (Brand'Binding msg)) where
    toStruct (Brand'Binding'newtype_ struct) = struct
instance (Untyped.HasMessage (Brand'Binding mut) mut) where
    message (Brand'Binding'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Brand'Binding mut) mut) where
    messageDefault msg = (Brand'Binding'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Brand'Binding msg)) where
    fromPtr msg ptr = (Brand'Binding'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Brand'Binding (Message.Mut s))) where
    toPtr msg (Brand'Binding'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Brand'Binding (Message.Mut s))) where
    new msg = (Brand'Binding'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem mut (Brand'Binding mut)) where
    newtype List mut (Brand'Binding mut)
        = Brand'Binding'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Brand'Binding'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Brand'Binding'List_ l) = (Untyped.ListStruct l)
    length (Brand'Binding'List_ l) = (Untyped.length l)
    index i (Brand'Binding'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Brand'Binding (Message.Mut s))) where
    setIndex (Brand'Binding'newtype_ elt) i (Brand'Binding'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Brand'Binding'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
data Brand'Binding' (mut :: Message.Mutability)
    = Brand'Binding'unbound 
    | Brand'Binding'type_ (Type mut)
    | Brand'Binding'unknown' Std_.Word16
instance (Classes.FromStruct mut (Brand'Binding' mut)) where
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
get_Brand'Binding' :: ((Untyped.ReadCtx m msg)
                      ,(Classes.FromStruct msg (Brand'Binding' msg))) => (Brand'Binding msg) -> (m (Brand'Binding' msg))
get_Brand'Binding' (Brand'Binding'newtype_ struct) = (Classes.fromStruct struct)
set_Brand'Binding'unbound :: ((Untyped.RWCtx m s)) => (Brand'Binding (Message.Mut s)) -> (m ())
set_Brand'Binding'unbound (Brand'Binding'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Brand'Binding'type_ :: ((Untyped.RWCtx m s)
                           ,(Classes.ToPtr s (Type (Message.Mut s)))) => (Brand'Binding (Message.Mut s)) -> (Type (Message.Mut s)) -> (m ())
set_Brand'Binding'type_ (Brand'Binding'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Brand'Binding'unknown' :: ((Untyped.RWCtx m s)) => (Brand'Binding (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Brand'Binding'unknown' (Brand'Binding'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
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
    | Value'text (Basics.Text mut)
    | Value'data_ (Basics.Data mut)
    | Value'list (Std_.Maybe (Untyped.Ptr mut))
    | Value'enum Std_.Word16
    | Value'struct (Std_.Maybe (Untyped.Ptr mut))
    | Value'interface 
    | Value'anyPointer (Std_.Maybe (Untyped.Ptr mut))
    | Value'unknown' Std_.Word16
instance (Classes.FromStruct mut (Value' mut)) where
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
get_Value' :: ((Untyped.ReadCtx m msg)
              ,(Classes.FromStruct msg (Value' msg))) => (Value msg) -> (m (Value' msg))
get_Value' (Value'newtype_ struct) = (Classes.fromStruct struct)
set_Value'void :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> (m ())
set_Value'void (Value'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Value'bool :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Bool -> (m ())
set_Value'bool (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 0 16 0)
    )
set_Value'int8 :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Int8 -> (m ())
set_Value'int8 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word8) 0 16 0)
    )
set_Value'int16 :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Int16 -> (m ())
set_Value'int16 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 16 0)
    )
set_Value'int32 :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Int32 -> (m ())
set_Value'int32 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (4 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 32 0)
    )
set_Value'int64 :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Int64 -> (m ())
set_Value'int64 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (5 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
    )
set_Value'uint8 :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Word8 -> (m ())
set_Value'uint8 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (6 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word8) 0 16 0)
    )
set_Value'uint16 :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Value'uint16 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (7 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 16 0)
    )
set_Value'uint32 :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Value'uint32 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (8 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 32 0)
    )
set_Value'uint64 :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Value'uint64 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (9 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
    )
set_Value'float32 :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Float -> (m ())
set_Value'float32 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (10 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 32 0)
    )
set_Value'float64 :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Double -> (m ())
set_Value'float64 (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (11 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
    )
set_Value'text :: ((Untyped.RWCtx m s)
                  ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (Value (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_Value'text (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (12 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Value'data_ :: ((Untyped.RWCtx m s)
                   ,(Classes.ToPtr s (Basics.Data (Message.Mut s)))) => (Value (Message.Mut s)) -> (Basics.Data (Message.Mut s)) -> (m ())
set_Value'data_ (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (13 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Value'list :: ((Untyped.RWCtx m s)
                  ,(Classes.ToPtr s (Std_.Maybe (Untyped.Ptr (Message.Mut s))))) => (Value (Message.Mut s)) -> (Std_.Maybe (Untyped.Ptr (Message.Mut s))) -> (m ())
set_Value'list (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (14 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Value'enum :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Value'enum (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (15 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 16 0)
    )
set_Value'struct :: ((Untyped.RWCtx m s)
                    ,(Classes.ToPtr s (Std_.Maybe (Untyped.Ptr (Message.Mut s))))) => (Value (Message.Mut s)) -> (Std_.Maybe (Untyped.Ptr (Message.Mut s))) -> (m ())
set_Value'struct (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (16 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Value'interface :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> (m ())
set_Value'interface (Value'newtype_ struct) = (do
    (GenHelpers.setWordField struct (17 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_Value'anyPointer :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Std_.Maybe (Untyped.Ptr (Message.Mut s))))) => (Value (Message.Mut s)) -> (Std_.Maybe (Untyped.Ptr (Message.Mut s))) -> (m ())
set_Value'anyPointer (Value'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (18 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Value'unknown' :: ((Untyped.RWCtx m s)) => (Value (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Value'unknown' (Value'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
newtype Annotation msg
    = Annotation'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Annotation msg)) where
    fromStruct struct = (Std_.pure (Annotation'newtype_ struct))
instance (Classes.ToStruct msg (Annotation msg)) where
    toStruct (Annotation'newtype_ struct) = struct
instance (Untyped.HasMessage (Annotation mut) mut) where
    message (Annotation'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Annotation mut) mut) where
    messageDefault msg = (Annotation'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Annotation msg)) where
    fromPtr msg ptr = (Annotation'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Annotation (Message.Mut s))) where
    toPtr msg (Annotation'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Annotation (Message.Mut s))) where
    new msg = (Annotation'newtype_ <$> (Untyped.allocStruct msg 1 2))
instance (Basics.ListElem mut (Annotation mut)) where
    newtype List mut (Annotation mut)
        = Annotation'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Annotation'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Annotation'List_ l) = (Untyped.ListStruct l)
    length (Annotation'List_ l) = (Untyped.length l)
    index i (Annotation'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Annotation (Message.Mut s))) where
    setIndex (Annotation'newtype_ elt) i (Annotation'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Annotation'List_ <$> (Untyped.allocCompositeList msg 1 2 len))
get_Annotation'id :: ((Untyped.ReadCtx m msg)) => (Annotation msg) -> (m Std_.Word64)
get_Annotation'id (Annotation'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Annotation'id :: ((Untyped.RWCtx m s)) => (Annotation (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Annotation'id (Annotation'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
get_Annotation'value :: ((Untyped.ReadCtx m msg)
                        ,(Classes.FromPtr msg (Value msg))) => (Annotation msg) -> (m (Value msg))
get_Annotation'value (Annotation'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Annotation'value :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Value (Message.Mut s)))) => (Annotation (Message.Mut s)) -> (Value (Message.Mut s)) -> (m ())
set_Annotation'value (Annotation'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Annotation'value :: ((Untyped.ReadCtx m msg)) => (Annotation msg) -> (m Std_.Bool)
has_Annotation'value (Annotation'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Annotation'value :: ((Untyped.RWCtx m s)) => (Annotation (Message.Mut s)) -> (m (Value (Message.Mut s)))
new_Annotation'value struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Annotation'value struct result)
    (Std_.pure result)
    )
get_Annotation'brand :: ((Untyped.ReadCtx m msg)
                        ,(Classes.FromPtr msg (Brand msg))) => (Annotation msg) -> (m (Brand msg))
get_Annotation'brand (Annotation'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Annotation'brand :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Brand (Message.Mut s)))) => (Annotation (Message.Mut s)) -> (Brand (Message.Mut s)) -> (m ())
set_Annotation'brand (Annotation'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Annotation'brand :: ((Untyped.ReadCtx m msg)) => (Annotation msg) -> (m Std_.Bool)
has_Annotation'brand (Annotation'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Annotation'brand :: ((Untyped.RWCtx m s)) => (Annotation (Message.Mut s)) -> (m (Brand (Message.Mut s)))
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
instance (Basics.ListElem mut ElementSize) where
    newtype List mut ElementSize
        = ElementSize'List_ (Untyped.ListOf mut Std_.Word16)
    index i (ElementSize'List_ l) = (Classes.fromWord <$> (Std_.fromIntegral <$> (Untyped.index i l)))
    listFromPtr msg ptr = (ElementSize'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (ElementSize'List_ l) = (Untyped.List16 l)
    length (ElementSize'List_ l) = (Untyped.length l)
instance (Classes.MutListElem s ElementSize) where
    setIndex elt i (ElementSize'List_ l) = (Untyped.setIndex (Std_.fromIntegral (Classes.toWord elt)) i l)
    newList msg size = (ElementSize'List_ <$> (Untyped.allocList16 msg size))
newtype CapnpVersion msg
    = CapnpVersion'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (CapnpVersion msg)) where
    fromStruct struct = (Std_.pure (CapnpVersion'newtype_ struct))
instance (Classes.ToStruct msg (CapnpVersion msg)) where
    toStruct (CapnpVersion'newtype_ struct) = struct
instance (Untyped.HasMessage (CapnpVersion mut) mut) where
    message (CapnpVersion'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (CapnpVersion mut) mut) where
    messageDefault msg = (CapnpVersion'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (CapnpVersion msg)) where
    fromPtr msg ptr = (CapnpVersion'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (CapnpVersion (Message.Mut s))) where
    toPtr msg (CapnpVersion'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (CapnpVersion (Message.Mut s))) where
    new msg = (CapnpVersion'newtype_ <$> (Untyped.allocStruct msg 1 0))
instance (Basics.ListElem mut (CapnpVersion mut)) where
    newtype List mut (CapnpVersion mut)
        = CapnpVersion'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (CapnpVersion'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (CapnpVersion'List_ l) = (Untyped.ListStruct l)
    length (CapnpVersion'List_ l) = (Untyped.length l)
    index i (CapnpVersion'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (CapnpVersion (Message.Mut s))) where
    setIndex (CapnpVersion'newtype_ elt) i (CapnpVersion'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (CapnpVersion'List_ <$> (Untyped.allocCompositeList msg 1 0 len))
get_CapnpVersion'major :: ((Untyped.ReadCtx m msg)) => (CapnpVersion msg) -> (m Std_.Word16)
get_CapnpVersion'major (CapnpVersion'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_CapnpVersion'major :: ((Untyped.RWCtx m s)) => (CapnpVersion (Message.Mut s)) -> Std_.Word16 -> (m ())
set_CapnpVersion'major (CapnpVersion'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
get_CapnpVersion'minor :: ((Untyped.ReadCtx m msg)) => (CapnpVersion msg) -> (m Std_.Word8)
get_CapnpVersion'minor (CapnpVersion'newtype_ struct) = (GenHelpers.getWordField struct 0 16 0)
set_CapnpVersion'minor :: ((Untyped.RWCtx m s)) => (CapnpVersion (Message.Mut s)) -> Std_.Word8 -> (m ())
set_CapnpVersion'minor (CapnpVersion'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word8) 0 16 0)
get_CapnpVersion'micro :: ((Untyped.ReadCtx m msg)) => (CapnpVersion msg) -> (m Std_.Word8)
get_CapnpVersion'micro (CapnpVersion'newtype_ struct) = (GenHelpers.getWordField struct 0 24 0)
set_CapnpVersion'micro :: ((Untyped.RWCtx m s)) => (CapnpVersion (Message.Mut s)) -> Std_.Word8 -> (m ())
set_CapnpVersion'micro (CapnpVersion'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word8) 0 24 0)
newtype CodeGeneratorRequest msg
    = CodeGeneratorRequest'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (CodeGeneratorRequest msg)) where
    fromStruct struct = (Std_.pure (CodeGeneratorRequest'newtype_ struct))
instance (Classes.ToStruct msg (CodeGeneratorRequest msg)) where
    toStruct (CodeGeneratorRequest'newtype_ struct) = struct
instance (Untyped.HasMessage (CodeGeneratorRequest mut) mut) where
    message (CodeGeneratorRequest'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (CodeGeneratorRequest mut) mut) where
    messageDefault msg = (CodeGeneratorRequest'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (CodeGeneratorRequest msg)) where
    fromPtr msg ptr = (CodeGeneratorRequest'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (CodeGeneratorRequest (Message.Mut s))) where
    toPtr msg (CodeGeneratorRequest'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (CodeGeneratorRequest (Message.Mut s))) where
    new msg = (CodeGeneratorRequest'newtype_ <$> (Untyped.allocStruct msg 0 4))
instance (Basics.ListElem mut (CodeGeneratorRequest mut)) where
    newtype List mut (CodeGeneratorRequest mut)
        = CodeGeneratorRequest'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (CodeGeneratorRequest'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (CodeGeneratorRequest'List_ l) = (Untyped.ListStruct l)
    length (CodeGeneratorRequest'List_ l) = (Untyped.length l)
    index i (CodeGeneratorRequest'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (CodeGeneratorRequest (Message.Mut s))) where
    setIndex (CodeGeneratorRequest'newtype_ elt) i (CodeGeneratorRequest'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (CodeGeneratorRequest'List_ <$> (Untyped.allocCompositeList msg 0 4 len))
get_CodeGeneratorRequest'nodes :: ((Untyped.ReadCtx m msg)
                                  ,(Classes.FromPtr msg (Basics.List msg (Node msg)))) => (CodeGeneratorRequest msg) -> (m (Basics.List msg (Node msg)))
get_CodeGeneratorRequest'nodes (CodeGeneratorRequest'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_CodeGeneratorRequest'nodes :: ((Untyped.RWCtx m s)
                                  ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Node (Message.Mut s))))) => (CodeGeneratorRequest (Message.Mut s)) -> (Basics.List (Message.Mut s) (Node (Message.Mut s))) -> (m ())
set_CodeGeneratorRequest'nodes (CodeGeneratorRequest'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_CodeGeneratorRequest'nodes :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest msg) -> (m Std_.Bool)
has_CodeGeneratorRequest'nodes (CodeGeneratorRequest'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_CodeGeneratorRequest'nodes :: ((Untyped.RWCtx m s)) => Std_.Int -> (CodeGeneratorRequest (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Node (Message.Mut s))))
new_CodeGeneratorRequest'nodes len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_CodeGeneratorRequest'nodes struct result)
    (Std_.pure result)
    )
get_CodeGeneratorRequest'requestedFiles :: ((Untyped.ReadCtx m msg)
                                           ,(Classes.FromPtr msg (Basics.List msg (CodeGeneratorRequest'RequestedFile msg)))) => (CodeGeneratorRequest msg) -> (m (Basics.List msg (CodeGeneratorRequest'RequestedFile msg)))
get_CodeGeneratorRequest'requestedFiles (CodeGeneratorRequest'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_CodeGeneratorRequest'requestedFiles :: ((Untyped.RWCtx m s)
                                           ,(Classes.ToPtr s (Basics.List (Message.Mut s) (CodeGeneratorRequest'RequestedFile (Message.Mut s))))) => (CodeGeneratorRequest (Message.Mut s)) -> (Basics.List (Message.Mut s) (CodeGeneratorRequest'RequestedFile (Message.Mut s))) -> (m ())
set_CodeGeneratorRequest'requestedFiles (CodeGeneratorRequest'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_CodeGeneratorRequest'requestedFiles :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest msg) -> (m Std_.Bool)
has_CodeGeneratorRequest'requestedFiles (CodeGeneratorRequest'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_CodeGeneratorRequest'requestedFiles :: ((Untyped.RWCtx m s)) => Std_.Int -> (CodeGeneratorRequest (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (CodeGeneratorRequest'RequestedFile (Message.Mut s))))
new_CodeGeneratorRequest'requestedFiles len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_CodeGeneratorRequest'requestedFiles struct result)
    (Std_.pure result)
    )
get_CodeGeneratorRequest'capnpVersion :: ((Untyped.ReadCtx m msg)
                                         ,(Classes.FromPtr msg (CapnpVersion msg))) => (CodeGeneratorRequest msg) -> (m (CapnpVersion msg))
get_CodeGeneratorRequest'capnpVersion (CodeGeneratorRequest'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 2 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_CodeGeneratorRequest'capnpVersion :: ((Untyped.RWCtx m s)
                                         ,(Classes.ToPtr s (CapnpVersion (Message.Mut s)))) => (CodeGeneratorRequest (Message.Mut s)) -> (CapnpVersion (Message.Mut s)) -> (m ())
set_CodeGeneratorRequest'capnpVersion (CodeGeneratorRequest'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 2 struct)
    )
has_CodeGeneratorRequest'capnpVersion :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest msg) -> (m Std_.Bool)
has_CodeGeneratorRequest'capnpVersion (CodeGeneratorRequest'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 2 struct))
new_CodeGeneratorRequest'capnpVersion :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest (Message.Mut s)) -> (m (CapnpVersion (Message.Mut s)))
new_CodeGeneratorRequest'capnpVersion struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_CodeGeneratorRequest'capnpVersion struct result)
    (Std_.pure result)
    )
get_CodeGeneratorRequest'sourceInfo :: ((Untyped.ReadCtx m msg)
                                       ,(Classes.FromPtr msg (Basics.List msg (Node'SourceInfo msg)))) => (CodeGeneratorRequest msg) -> (m (Basics.List msg (Node'SourceInfo msg)))
get_CodeGeneratorRequest'sourceInfo (CodeGeneratorRequest'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 3 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_CodeGeneratorRequest'sourceInfo :: ((Untyped.RWCtx m s)
                                       ,(Classes.ToPtr s (Basics.List (Message.Mut s) (Node'SourceInfo (Message.Mut s))))) => (CodeGeneratorRequest (Message.Mut s)) -> (Basics.List (Message.Mut s) (Node'SourceInfo (Message.Mut s))) -> (m ())
set_CodeGeneratorRequest'sourceInfo (CodeGeneratorRequest'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 3 struct)
    )
has_CodeGeneratorRequest'sourceInfo :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest msg) -> (m Std_.Bool)
has_CodeGeneratorRequest'sourceInfo (CodeGeneratorRequest'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 3 struct))
new_CodeGeneratorRequest'sourceInfo :: ((Untyped.RWCtx m s)) => Std_.Int -> (CodeGeneratorRequest (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (Node'SourceInfo (Message.Mut s))))
new_CodeGeneratorRequest'sourceInfo len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_CodeGeneratorRequest'sourceInfo struct result)
    (Std_.pure result)
    )
newtype CodeGeneratorRequest'RequestedFile msg
    = CodeGeneratorRequest'RequestedFile'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (CodeGeneratorRequest'RequestedFile msg)) where
    fromStruct struct = (Std_.pure (CodeGeneratorRequest'RequestedFile'newtype_ struct))
instance (Classes.ToStruct msg (CodeGeneratorRequest'RequestedFile msg)) where
    toStruct (CodeGeneratorRequest'RequestedFile'newtype_ struct) = struct
instance (Untyped.HasMessage (CodeGeneratorRequest'RequestedFile mut) mut) where
    message (CodeGeneratorRequest'RequestedFile'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (CodeGeneratorRequest'RequestedFile mut) mut) where
    messageDefault msg = (CodeGeneratorRequest'RequestedFile'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (CodeGeneratorRequest'RequestedFile msg)) where
    fromPtr msg ptr = (CodeGeneratorRequest'RequestedFile'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (CodeGeneratorRequest'RequestedFile (Message.Mut s))) where
    toPtr msg (CodeGeneratorRequest'RequestedFile'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (CodeGeneratorRequest'RequestedFile (Message.Mut s))) where
    new msg = (CodeGeneratorRequest'RequestedFile'newtype_ <$> (Untyped.allocStruct msg 1 2))
instance (Basics.ListElem mut (CodeGeneratorRequest'RequestedFile mut)) where
    newtype List mut (CodeGeneratorRequest'RequestedFile mut)
        = CodeGeneratorRequest'RequestedFile'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (CodeGeneratorRequest'RequestedFile'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (CodeGeneratorRequest'RequestedFile'List_ l) = (Untyped.ListStruct l)
    length (CodeGeneratorRequest'RequestedFile'List_ l) = (Untyped.length l)
    index i (CodeGeneratorRequest'RequestedFile'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (CodeGeneratorRequest'RequestedFile (Message.Mut s))) where
    setIndex (CodeGeneratorRequest'RequestedFile'newtype_ elt) i (CodeGeneratorRequest'RequestedFile'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (CodeGeneratorRequest'RequestedFile'List_ <$> (Untyped.allocCompositeList msg 1 2 len))
get_CodeGeneratorRequest'RequestedFile'id :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest'RequestedFile msg) -> (m Std_.Word64)
get_CodeGeneratorRequest'RequestedFile'id (CodeGeneratorRequest'RequestedFile'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_CodeGeneratorRequest'RequestedFile'id :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest'RequestedFile (Message.Mut s)) -> Std_.Word64 -> (m ())
set_CodeGeneratorRequest'RequestedFile'id (CodeGeneratorRequest'RequestedFile'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
get_CodeGeneratorRequest'RequestedFile'filename :: ((Untyped.ReadCtx m msg)
                                                   ,(Classes.FromPtr msg (Basics.Text msg))) => (CodeGeneratorRequest'RequestedFile msg) -> (m (Basics.Text msg))
get_CodeGeneratorRequest'RequestedFile'filename (CodeGeneratorRequest'RequestedFile'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_CodeGeneratorRequest'RequestedFile'filename :: ((Untyped.RWCtx m s)
                                                   ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (CodeGeneratorRequest'RequestedFile (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_CodeGeneratorRequest'RequestedFile'filename (CodeGeneratorRequest'RequestedFile'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_CodeGeneratorRequest'RequestedFile'filename :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest'RequestedFile msg) -> (m Std_.Bool)
has_CodeGeneratorRequest'RequestedFile'filename (CodeGeneratorRequest'RequestedFile'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_CodeGeneratorRequest'RequestedFile'filename :: ((Untyped.RWCtx m s)) => Std_.Int -> (CodeGeneratorRequest'RequestedFile (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_CodeGeneratorRequest'RequestedFile'filename len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_CodeGeneratorRequest'RequestedFile'filename struct result)
    (Std_.pure result)
    )
get_CodeGeneratorRequest'RequestedFile'imports :: ((Untyped.ReadCtx m msg)
                                                  ,(Classes.FromPtr msg (Basics.List msg (CodeGeneratorRequest'RequestedFile'Import msg)))) => (CodeGeneratorRequest'RequestedFile msg) -> (m (Basics.List msg (CodeGeneratorRequest'RequestedFile'Import msg)))
get_CodeGeneratorRequest'RequestedFile'imports (CodeGeneratorRequest'RequestedFile'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_CodeGeneratorRequest'RequestedFile'imports :: ((Untyped.RWCtx m s)
                                                  ,(Classes.ToPtr s (Basics.List (Message.Mut s) (CodeGeneratorRequest'RequestedFile'Import (Message.Mut s))))) => (CodeGeneratorRequest'RequestedFile (Message.Mut s)) -> (Basics.List (Message.Mut s) (CodeGeneratorRequest'RequestedFile'Import (Message.Mut s))) -> (m ())
set_CodeGeneratorRequest'RequestedFile'imports (CodeGeneratorRequest'RequestedFile'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_CodeGeneratorRequest'RequestedFile'imports :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest'RequestedFile msg) -> (m Std_.Bool)
has_CodeGeneratorRequest'RequestedFile'imports (CodeGeneratorRequest'RequestedFile'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_CodeGeneratorRequest'RequestedFile'imports :: ((Untyped.RWCtx m s)) => Std_.Int -> (CodeGeneratorRequest'RequestedFile (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (CodeGeneratorRequest'RequestedFile'Import (Message.Mut s))))
new_CodeGeneratorRequest'RequestedFile'imports len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_CodeGeneratorRequest'RequestedFile'imports struct result)
    (Std_.pure result)
    )
newtype CodeGeneratorRequest'RequestedFile'Import msg
    = CodeGeneratorRequest'RequestedFile'Import'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (CodeGeneratorRequest'RequestedFile'Import msg)) where
    fromStruct struct = (Std_.pure (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct))
instance (Classes.ToStruct msg (CodeGeneratorRequest'RequestedFile'Import msg)) where
    toStruct (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) = struct
instance (Untyped.HasMessage (CodeGeneratorRequest'RequestedFile'Import mut) mut) where
    message (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (CodeGeneratorRequest'RequestedFile'Import mut) mut) where
    messageDefault msg = (CodeGeneratorRequest'RequestedFile'Import'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (CodeGeneratorRequest'RequestedFile'Import msg)) where
    fromPtr msg ptr = (CodeGeneratorRequest'RequestedFile'Import'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (CodeGeneratorRequest'RequestedFile'Import (Message.Mut s))) where
    toPtr msg (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (CodeGeneratorRequest'RequestedFile'Import (Message.Mut s))) where
    new msg = (CodeGeneratorRequest'RequestedFile'Import'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem mut (CodeGeneratorRequest'RequestedFile'Import mut)) where
    newtype List mut (CodeGeneratorRequest'RequestedFile'Import mut)
        = CodeGeneratorRequest'RequestedFile'Import'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (CodeGeneratorRequest'RequestedFile'Import'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (CodeGeneratorRequest'RequestedFile'Import'List_ l) = (Untyped.ListStruct l)
    length (CodeGeneratorRequest'RequestedFile'Import'List_ l) = (Untyped.length l)
    index i (CodeGeneratorRequest'RequestedFile'Import'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (CodeGeneratorRequest'RequestedFile'Import (Message.Mut s))) where
    setIndex (CodeGeneratorRequest'RequestedFile'Import'newtype_ elt) i (CodeGeneratorRequest'RequestedFile'Import'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (CodeGeneratorRequest'RequestedFile'Import'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_CodeGeneratorRequest'RequestedFile'Import'id :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest'RequestedFile'Import msg) -> (m Std_.Word64)
get_CodeGeneratorRequest'RequestedFile'Import'id (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_CodeGeneratorRequest'RequestedFile'Import'id :: ((Untyped.RWCtx m s)) => (CodeGeneratorRequest'RequestedFile'Import (Message.Mut s)) -> Std_.Word64 -> (m ())
set_CodeGeneratorRequest'RequestedFile'Import'id (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 0 0 0)
get_CodeGeneratorRequest'RequestedFile'Import'name :: ((Untyped.ReadCtx m msg)
                                                      ,(Classes.FromPtr msg (Basics.Text msg))) => (CodeGeneratorRequest'RequestedFile'Import msg) -> (m (Basics.Text msg))
get_CodeGeneratorRequest'RequestedFile'Import'name (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_CodeGeneratorRequest'RequestedFile'Import'name :: ((Untyped.RWCtx m s)
                                                      ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (CodeGeneratorRequest'RequestedFile'Import (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_CodeGeneratorRequest'RequestedFile'Import'name (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_CodeGeneratorRequest'RequestedFile'Import'name :: ((Untyped.ReadCtx m msg)) => (CodeGeneratorRequest'RequestedFile'Import msg) -> (m Std_.Bool)
has_CodeGeneratorRequest'RequestedFile'Import'name (CodeGeneratorRequest'RequestedFile'Import'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_CodeGeneratorRequest'RequestedFile'Import'name :: ((Untyped.RWCtx m s)) => Std_.Int -> (CodeGeneratorRequest'RequestedFile'Import (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_CodeGeneratorRequest'RequestedFile'Import'name len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_CodeGeneratorRequest'RequestedFile'Import'name struct result)
    (Std_.pure result)
    )