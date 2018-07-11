{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Capnp.Capnp.RpcTwoparty where

-- Code generated by capnpc-haskell. DO NOT EDIT.
-- Generated from schema file: capnp/rpc-twoparty.capnp

import Data.Int
import Data.Word
import qualified Data.Bits
import qualified Data.Maybe
import qualified Codec.Capnp
import qualified Data.Capnp.BuiltinTypes
import qualified Data.Capnp.BuiltinTypes.Generic
import qualified Data.Capnp.TraversalLimit
import qualified Data.Capnp.Untyped
import qualified Data.Capnp.Message.Mutable

import qualified Capnp.ById.Xbdf87d7bb8304e81

newtype JoinKeyPart msg = JoinKeyPart Data.Capnp.Untyped.Struct

instance Codec.Capnp.IsStruct (JoinKeyPart msg) where
    fromStruct = pure . JoinKeyPart
instance Codec.Capnp.IsPtr (JoinKeyPart msg) where
    fromPtr = Codec.Capnp.structPtr
instance Data.Capnp.BuiltinTypes.Generic.ListElem msg (JoinKeyPart msg) where
    newtype List msg (JoinKeyPart msg) = List_JoinKeyPart (Data.Capnp.Untyped.ListOf Data.Capnp.Untyped.Struct)
    length (List_JoinKeyPart l) = Data.Capnp.Untyped.length l
    index i (List_JoinKeyPart l) = JoinKeyPart <$> Data.Capnp.Untyped.index i l
instance Data.Capnp.BuiltinTypes.Generic.MutListElem s (JoinKeyPart (Data.Capnp.Message.Mutable.Message s)) where
    setIndex (JoinKeyPart elt) i (List_JoinKeyPart l) = error "TODO: Generate code for setIndex"

instance Codec.Capnp.IsPtr (Data.Capnp.Untyped.ListOf (JoinKeyPart msg)) where
    fromPtr = Codec.Capnp.structListPtr
get_JoinKeyPart'joinId :: Data.Capnp.Untyped.ReadCtx m => JoinKeyPart msg -> m Word32
get_JoinKeyPart'joinId (JoinKeyPart struct) = Codec.Capnp.getWordField struct 0 0 0

has_JoinKeyPart'joinId :: Data.Capnp.Untyped.ReadCtx m => JoinKeyPart msg -> m Bool
has_JoinKeyPart'joinId(JoinKeyPart struct) = pure $ 0 < Data.Capnp.Untyped.length (Data.Capnp.Untyped.dataSection struct)
get_JoinKeyPart'partCount :: Data.Capnp.Untyped.ReadCtx m => JoinKeyPart msg -> m Word16
get_JoinKeyPart'partCount (JoinKeyPart struct) = Codec.Capnp.getWordField struct 0 32 0

has_JoinKeyPart'partCount :: Data.Capnp.Untyped.ReadCtx m => JoinKeyPart msg -> m Bool
has_JoinKeyPart'partCount(JoinKeyPart struct) = pure $ 0 < Data.Capnp.Untyped.length (Data.Capnp.Untyped.dataSection struct)
get_JoinKeyPart'partNum :: Data.Capnp.Untyped.ReadCtx m => JoinKeyPart msg -> m Word16
get_JoinKeyPart'partNum (JoinKeyPart struct) = Codec.Capnp.getWordField struct 0 48 0

has_JoinKeyPart'partNum :: Data.Capnp.Untyped.ReadCtx m => JoinKeyPart msg -> m Bool
has_JoinKeyPart'partNum(JoinKeyPart struct) = pure $ 0 < Data.Capnp.Untyped.length (Data.Capnp.Untyped.dataSection struct)
newtype JoinResult msg = JoinResult Data.Capnp.Untyped.Struct

instance Codec.Capnp.IsStruct (JoinResult msg) where
    fromStruct = pure . JoinResult
instance Codec.Capnp.IsPtr (JoinResult msg) where
    fromPtr = Codec.Capnp.structPtr
instance Data.Capnp.BuiltinTypes.Generic.ListElem msg (JoinResult msg) where
    newtype List msg (JoinResult msg) = List_JoinResult (Data.Capnp.Untyped.ListOf Data.Capnp.Untyped.Struct)
    length (List_JoinResult l) = Data.Capnp.Untyped.length l
    index i (List_JoinResult l) = JoinResult <$> Data.Capnp.Untyped.index i l
instance Data.Capnp.BuiltinTypes.Generic.MutListElem s (JoinResult (Data.Capnp.Message.Mutable.Message s)) where
    setIndex (JoinResult elt) i (List_JoinResult l) = error "TODO: Generate code for setIndex"

instance Codec.Capnp.IsPtr (Data.Capnp.Untyped.ListOf (JoinResult msg)) where
    fromPtr = Codec.Capnp.structListPtr
get_JoinResult'joinId :: Data.Capnp.Untyped.ReadCtx m => JoinResult msg -> m Word32
get_JoinResult'joinId (JoinResult struct) = Codec.Capnp.getWordField struct 0 0 0

has_JoinResult'joinId :: Data.Capnp.Untyped.ReadCtx m => JoinResult msg -> m Bool
has_JoinResult'joinId(JoinResult struct) = pure $ 0 < Data.Capnp.Untyped.length (Data.Capnp.Untyped.dataSection struct)
get_JoinResult'succeeded :: Data.Capnp.Untyped.ReadCtx m => JoinResult msg -> m Bool
get_JoinResult'succeeded (JoinResult struct) = Codec.Capnp.getWordField struct 0 32 0

has_JoinResult'succeeded :: Data.Capnp.Untyped.ReadCtx m => JoinResult msg -> m Bool
has_JoinResult'succeeded(JoinResult struct) = pure $ 0 < Data.Capnp.Untyped.length (Data.Capnp.Untyped.dataSection struct)
get_JoinResult'cap :: Data.Capnp.Untyped.ReadCtx m => JoinResult msg -> m (Maybe Data.Capnp.Untyped.Ptr)
get_JoinResult'cap (JoinResult struct) =
    Data.Capnp.Untyped.getPtr 0 struct
    >>= Codec.Capnp.fromPtr (Data.Capnp.Untyped.message struct)


has_JoinResult'cap :: Data.Capnp.Untyped.ReadCtx m => JoinResult msg -> m Bool
has_JoinResult'cap(JoinResult struct) = Data.Maybe.isJust <$> Data.Capnp.Untyped.getPtr 0 struct
data Side
    = Side'server
    | Side'client
    | Side'unknown' Word16
instance Enum Side where
    toEnum = Codec.Capnp.fromWord . fromIntegral
    fromEnum = fromIntegral . Codec.Capnp.toWord


instance Codec.Capnp.IsWord Side where
    fromWord n = go (fromIntegral n :: Word16)
      where
        go 1 = Side'client
        go 0 = Side'server
        go tag = Side'unknown' (fromIntegral tag)
    toWord Side'client = 1
    toWord Side'server = 0
    toWord (Side'unknown' tag) = fromIntegral tag
instance Data.Capnp.BuiltinTypes.Generic.ListElem msg Side where
    newtype List msg Side = List_Side (Data.Capnp.Untyped.ListOf Word16)
    length (List_Side l) = Data.Capnp.Untyped.length l
    index i (List_Side l) = (Codec.Capnp.fromWord . fromIntegral) <$> Data.Capnp.Untyped.index i l
instance Data.Capnp.BuiltinTypes.Generic.MutListElem s Side where
    setIndex elt i (List_Side l) = error "TODO: generate code for setIndex"
instance Codec.Capnp.IsPtr (Data.Capnp.Untyped.ListOf Side) where
    fromPtr msg ptr = fmap
       (fmap (toEnum . (fromIntegral :: Word16 -> Int)))
       (Codec.Capnp.fromPtr msg ptr)

newtype ProvisionId msg = ProvisionId Data.Capnp.Untyped.Struct

instance Codec.Capnp.IsStruct (ProvisionId msg) where
    fromStruct = pure . ProvisionId
instance Codec.Capnp.IsPtr (ProvisionId msg) where
    fromPtr = Codec.Capnp.structPtr
instance Data.Capnp.BuiltinTypes.Generic.ListElem msg (ProvisionId msg) where
    newtype List msg (ProvisionId msg) = List_ProvisionId (Data.Capnp.Untyped.ListOf Data.Capnp.Untyped.Struct)
    length (List_ProvisionId l) = Data.Capnp.Untyped.length l
    index i (List_ProvisionId l) = ProvisionId <$> Data.Capnp.Untyped.index i l
instance Data.Capnp.BuiltinTypes.Generic.MutListElem s (ProvisionId (Data.Capnp.Message.Mutable.Message s)) where
    setIndex (ProvisionId elt) i (List_ProvisionId l) = error "TODO: Generate code for setIndex"

instance Codec.Capnp.IsPtr (Data.Capnp.Untyped.ListOf (ProvisionId msg)) where
    fromPtr = Codec.Capnp.structListPtr
get_ProvisionId'joinId :: Data.Capnp.Untyped.ReadCtx m => ProvisionId msg -> m Word32
get_ProvisionId'joinId (ProvisionId struct) = Codec.Capnp.getWordField struct 0 0 0

has_ProvisionId'joinId :: Data.Capnp.Untyped.ReadCtx m => ProvisionId msg -> m Bool
has_ProvisionId'joinId(ProvisionId struct) = pure $ 0 < Data.Capnp.Untyped.length (Data.Capnp.Untyped.dataSection struct)
newtype VatId msg = VatId Data.Capnp.Untyped.Struct

instance Codec.Capnp.IsStruct (VatId msg) where
    fromStruct = pure . VatId
instance Codec.Capnp.IsPtr (VatId msg) where
    fromPtr = Codec.Capnp.structPtr
instance Data.Capnp.BuiltinTypes.Generic.ListElem msg (VatId msg) where
    newtype List msg (VatId msg) = List_VatId (Data.Capnp.Untyped.ListOf Data.Capnp.Untyped.Struct)
    length (List_VatId l) = Data.Capnp.Untyped.length l
    index i (List_VatId l) = VatId <$> Data.Capnp.Untyped.index i l
instance Data.Capnp.BuiltinTypes.Generic.MutListElem s (VatId (Data.Capnp.Message.Mutable.Message s)) where
    setIndex (VatId elt) i (List_VatId l) = error "TODO: Generate code for setIndex"

instance Codec.Capnp.IsPtr (Data.Capnp.Untyped.ListOf (VatId msg)) where
    fromPtr = Codec.Capnp.structListPtr
get_VatId'side :: Data.Capnp.Untyped.ReadCtx m => VatId msg -> m Side
get_VatId'side (VatId struct) = Codec.Capnp.getWordField struct 0 0 0

has_VatId'side :: Data.Capnp.Untyped.ReadCtx m => VatId msg -> m Bool
has_VatId'side(VatId struct) = pure $ 0 < Data.Capnp.Untyped.length (Data.Capnp.Untyped.dataSection struct)