{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.Schema.New where
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
data Node 
type instance (R.ReprFor Node) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node) where
    typeId  = 16610026722781537303
instance (C.TypedStruct Node) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node) where
    type AllocHint Node = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node (C.Parsed Node))
instance (C.AllocateList Node) where
    type ListAllocHint Node = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node (C.Parsed Node))
data instance C.Parsed Node
    = Node 
        {id :: (RP.Parsed Std_.Word64)
        ,displayName :: (RP.Parsed Basics.Text)
        ,displayNamePrefixLength :: (RP.Parsed Std_.Word32)
        ,scopeId :: (RP.Parsed Std_.Word64)
        ,nestedNodes :: (RP.Parsed (R.List Node'NestedNode))
        ,annotations :: (RP.Parsed (R.List Annotation))
        ,parameters :: (RP.Parsed (R.List Node'Parameter))
        ,isGeneric :: (RP.Parsed Std_.Bool)
        ,union' :: (C.Parsed (GH.Which Node))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node))
deriving instance (Std_.Eq (C.Parsed Node))
instance (C.Parse Node (C.Parsed Node)) where
    parse raw_ = (Node <$> (GH.parseField #id raw_)
                       <*> (GH.parseField #displayName raw_)
                       <*> (GH.parseField #displayNamePrefixLength raw_)
                       <*> (GH.parseField #scopeId raw_)
                       <*> (GH.parseField #nestedNodes raw_)
                       <*> (GH.parseField #annotations raw_)
                       <*> (GH.parseField #parameters raw_)
                       <*> (GH.parseField #isGeneric raw_)
                       <*> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Node (C.Parsed Node)) where
    marshalInto raw_ Node{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #displayName displayName raw_)
        (GH.encodeField #displayNamePrefixLength displayNamePrefixLength raw_)
        (GH.encodeField #scopeId scopeId raw_)
        (GH.encodeField #nestedNodes nestedNodes raw_)
        (GH.encodeField #annotations annotations raw_)
        (GH.encodeField #parameters parameters raw_)
        (GH.encodeField #isGeneric isGeneric raw_)
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Node) where
    unionField  = (GH.dataField 32 1 16 0)
    data RawWhich Node mut_
        = RW_Node'file (R.Raw () mut_)
        | RW_Node'struct (R.Raw Node'struct mut_)
        | RW_Node'enum (R.Raw Node'enum mut_)
        | RW_Node'interface (R.Raw Node'interface mut_)
        | RW_Node'const (R.Raw Node'const mut_)
        | RW_Node'annotation (R.Raw Node'annotation mut_)
        | RW_Node'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Node'file <$> (GH.readVariant #file struct_))
        1 ->
            (RW_Node'struct <$> (GH.readVariant #struct struct_))
        2 ->
            (RW_Node'enum <$> (GH.readVariant #enum struct_))
        3 ->
            (RW_Node'interface <$> (GH.readVariant #interface struct_))
        4 ->
            (RW_Node'const <$> (GH.readVariant #const struct_))
        5 ->
            (RW_Node'annotation <$> (GH.readVariant #annotation struct_))
        _ ->
            (Std_.pure (RW_Node'unknown' tag_))
    data Which Node
instance (GH.HasVariant "file" GH.Slot Node ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance (GH.HasVariant "struct" GH.Group Node Node'struct) where
    variantByLabel  = (GH.Variant GH.groupField 1)
instance (GH.HasVariant "enum" GH.Group Node Node'enum) where
    variantByLabel  = (GH.Variant GH.groupField 2)
instance (GH.HasVariant "interface" GH.Group Node Node'interface) where
    variantByLabel  = (GH.Variant GH.groupField 3)
instance (GH.HasVariant "const" GH.Group Node Node'const) where
    variantByLabel  = (GH.Variant GH.groupField 4)
instance (GH.HasVariant "annotation" GH.Group Node Node'annotation) where
    variantByLabel  = (GH.Variant GH.groupField 5)
data instance C.Parsed (GH.Which Node)
    = Node'file 
    | Node'struct (RP.Parsed Node'struct)
    | Node'enum (RP.Parsed Node'enum)
    | Node'interface (RP.Parsed Node'interface)
    | Node'const (RP.Parsed Node'const)
    | Node'annotation (RP.Parsed Node'annotation)
    | Node'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Node)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Node)))
instance (C.Parse (GH.Which Node) (C.Parsed (GH.Which Node))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Node'file _) ->
                (Std_.pure Node'file)
            (RW_Node'struct rawArg_) ->
                (Node'struct <$> (C.parse rawArg_))
            (RW_Node'enum rawArg_) ->
                (Node'enum <$> (C.parse rawArg_))
            (RW_Node'interface rawArg_) ->
                (Node'interface <$> (C.parse rawArg_))
            (RW_Node'const rawArg_) ->
                (Node'const <$> (C.parse rawArg_))
            (RW_Node'annotation rawArg_) ->
                (Node'annotation <$> (C.parse rawArg_))
            (RW_Node'unknown' tag_) ->
                (Std_.pure (Node'unknown' tag_))
        )
instance (C.Marshal (GH.Which Node) (C.Parsed (GH.Which Node))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Node'file) ->
            (GH.encodeVariant #file () (GH.unionStruct raw_))
        (Node'struct arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #struct (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Node'enum arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #enum (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Node'interface arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #interface (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Node'const arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #const (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Node'annotation arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #annotation (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Node'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
instance (GH.HasField "id" GH.Slot Node Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "displayName" GH.Slot Node Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "displayNamePrefixLength" GH.Slot Node Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
instance (GH.HasField "scopeId" GH.Slot Node Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "nestedNodes" GH.Slot Node (R.List Node'NestedNode)) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "annotations" GH.Slot Node (R.List Annotation)) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "parameters" GH.Slot Node (R.List Node'Parameter)) where
    fieldByLabel  = (GH.ptrField 5)
instance (GH.HasField "isGeneric" GH.Slot Node Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 4 1 0)
data Node'struct 
type instance (R.ReprFor Node'struct) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'struct) where
    typeId  = 11430331134483579957
instance (C.TypedStruct Node'struct) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'struct) where
    type AllocHint Node'struct = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'struct (C.Parsed Node'struct))
instance (C.AllocateList Node'struct) where
    type ListAllocHint Node'struct = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'struct (C.Parsed Node'struct))
data instance C.Parsed Node'struct
    = Node'struct' 
        {dataWordCount :: (RP.Parsed Std_.Word16)
        ,pointerCount :: (RP.Parsed Std_.Word16)
        ,preferredListEncoding :: (RP.Parsed ElementSize)
        ,isGroup :: (RP.Parsed Std_.Bool)
        ,discriminantCount :: (RP.Parsed Std_.Word16)
        ,discriminantOffset :: (RP.Parsed Std_.Word32)
        ,fields :: (RP.Parsed (R.List Field))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'struct))
deriving instance (Std_.Eq (C.Parsed Node'struct))
instance (C.Parse Node'struct (C.Parsed Node'struct)) where
    parse raw_ = (Node'struct' <$> (GH.parseField #dataWordCount raw_)
                               <*> (GH.parseField #pointerCount raw_)
                               <*> (GH.parseField #preferredListEncoding raw_)
                               <*> (GH.parseField #isGroup raw_)
                               <*> (GH.parseField #discriminantCount raw_)
                               <*> (GH.parseField #discriminantOffset raw_)
                               <*> (GH.parseField #fields raw_))
instance (C.Marshal Node'struct (C.Parsed Node'struct)) where
    marshalInto raw_ Node'struct'{..} = (do
        (GH.encodeField #dataWordCount dataWordCount raw_)
        (GH.encodeField #pointerCount pointerCount raw_)
        (GH.encodeField #preferredListEncoding preferredListEncoding raw_)
        (GH.encodeField #isGroup isGroup raw_)
        (GH.encodeField #discriminantCount discriminantCount raw_)
        (GH.encodeField #discriminantOffset discriminantOffset raw_)
        (GH.encodeField #fields fields raw_)
        (Std_.pure ())
        )
instance (GH.HasField "dataWordCount" GH.Slot Node'struct Std_.Word16) where
    fieldByLabel  = (GH.dataField 48 1 16 0)
instance (GH.HasField "pointerCount" GH.Slot Node'struct Std_.Word16) where
    fieldByLabel  = (GH.dataField 0 3 16 0)
instance (GH.HasField "preferredListEncoding" GH.Slot Node'struct ElementSize) where
    fieldByLabel  = (GH.dataField 16 3 16 0)
instance (GH.HasField "isGroup" GH.Slot Node'struct Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 3 1 0)
instance (GH.HasField "discriminantCount" GH.Slot Node'struct Std_.Word16) where
    fieldByLabel  = (GH.dataField 48 3 16 0)
instance (GH.HasField "discriminantOffset" GH.Slot Node'struct Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 4 32 0)
instance (GH.HasField "fields" GH.Slot Node'struct (R.List Field)) where
    fieldByLabel  = (GH.ptrField 3)
data Node'enum 
type instance (R.ReprFor Node'enum) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'enum) where
    typeId  = 13063450714778629528
instance (C.TypedStruct Node'enum) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'enum) where
    type AllocHint Node'enum = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'enum (C.Parsed Node'enum))
instance (C.AllocateList Node'enum) where
    type ListAllocHint Node'enum = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'enum (C.Parsed Node'enum))
data instance C.Parsed Node'enum
    = Node'enum' 
        {enumerants :: (RP.Parsed (R.List Enumerant))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'enum))
deriving instance (Std_.Eq (C.Parsed Node'enum))
instance (C.Parse Node'enum (C.Parsed Node'enum)) where
    parse raw_ = (Node'enum' <$> (GH.parseField #enumerants raw_))
instance (C.Marshal Node'enum (C.Parsed Node'enum)) where
    marshalInto raw_ Node'enum'{..} = (do
        (GH.encodeField #enumerants enumerants raw_)
        (Std_.pure ())
        )
instance (GH.HasField "enumerants" GH.Slot Node'enum (R.List Enumerant)) where
    fieldByLabel  = (GH.ptrField 3)
data Node'interface 
type instance (R.ReprFor Node'interface) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'interface) where
    typeId  = 16728431493453586831
instance (C.TypedStruct Node'interface) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'interface) where
    type AllocHint Node'interface = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'interface (C.Parsed Node'interface))
instance (C.AllocateList Node'interface) where
    type ListAllocHint Node'interface = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'interface (C.Parsed Node'interface))
data instance C.Parsed Node'interface
    = Node'interface' 
        {methods :: (RP.Parsed (R.List Method))
        ,superclasses :: (RP.Parsed (R.List Superclass))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'interface))
deriving instance (Std_.Eq (C.Parsed Node'interface))
instance (C.Parse Node'interface (C.Parsed Node'interface)) where
    parse raw_ = (Node'interface' <$> (GH.parseField #methods raw_)
                                  <*> (GH.parseField #superclasses raw_))
instance (C.Marshal Node'interface (C.Parsed Node'interface)) where
    marshalInto raw_ Node'interface'{..} = (do
        (GH.encodeField #methods methods raw_)
        (GH.encodeField #superclasses superclasses raw_)
        (Std_.pure ())
        )
instance (GH.HasField "methods" GH.Slot Node'interface (R.List Method)) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "superclasses" GH.Slot Node'interface (R.List Superclass)) where
    fieldByLabel  = (GH.ptrField 4)
data Node'const 
type instance (R.ReprFor Node'const) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'const) where
    typeId  = 12793219851699983392
instance (C.TypedStruct Node'const) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'const) where
    type AllocHint Node'const = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'const (C.Parsed Node'const))
instance (C.AllocateList Node'const) where
    type ListAllocHint Node'const = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'const (C.Parsed Node'const))
data instance C.Parsed Node'const
    = Node'const' 
        {type_ :: (RP.Parsed Type)
        ,value :: (RP.Parsed Value)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'const))
deriving instance (Std_.Eq (C.Parsed Node'const))
instance (C.Parse Node'const (C.Parsed Node'const)) where
    parse raw_ = (Node'const' <$> (GH.parseField #type_ raw_)
                              <*> (GH.parseField #value raw_))
instance (C.Marshal Node'const (C.Parsed Node'const)) where
    marshalInto raw_ Node'const'{..} = (do
        (GH.encodeField #type_ type_ raw_)
        (GH.encodeField #value value raw_)
        (Std_.pure ())
        )
instance (GH.HasField "type_" GH.Slot Node'const Type) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "value" GH.Slot Node'const Value) where
    fieldByLabel  = (GH.ptrField 4)
data Node'annotation 
type instance (R.ReprFor Node'annotation) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'annotation) where
    typeId  = 17011813041836786320
instance (C.TypedStruct Node'annotation) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'annotation) where
    type AllocHint Node'annotation = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'annotation (C.Parsed Node'annotation))
instance (C.AllocateList Node'annotation) where
    type ListAllocHint Node'annotation = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'annotation (C.Parsed Node'annotation))
data instance C.Parsed Node'annotation
    = Node'annotation' 
        {type_ :: (RP.Parsed Type)
        ,targetsFile :: (RP.Parsed Std_.Bool)
        ,targetsConst :: (RP.Parsed Std_.Bool)
        ,targetsEnum :: (RP.Parsed Std_.Bool)
        ,targetsEnumerant :: (RP.Parsed Std_.Bool)
        ,targetsStruct :: (RP.Parsed Std_.Bool)
        ,targetsField :: (RP.Parsed Std_.Bool)
        ,targetsUnion :: (RP.Parsed Std_.Bool)
        ,targetsGroup :: (RP.Parsed Std_.Bool)
        ,targetsInterface :: (RP.Parsed Std_.Bool)
        ,targetsMethod :: (RP.Parsed Std_.Bool)
        ,targetsParam :: (RP.Parsed Std_.Bool)
        ,targetsAnnotation :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'annotation))
deriving instance (Std_.Eq (C.Parsed Node'annotation))
instance (C.Parse Node'annotation (C.Parsed Node'annotation)) where
    parse raw_ = (Node'annotation' <$> (GH.parseField #type_ raw_)
                                   <*> (GH.parseField #targetsFile raw_)
                                   <*> (GH.parseField #targetsConst raw_)
                                   <*> (GH.parseField #targetsEnum raw_)
                                   <*> (GH.parseField #targetsEnumerant raw_)
                                   <*> (GH.parseField #targetsStruct raw_)
                                   <*> (GH.parseField #targetsField raw_)
                                   <*> (GH.parseField #targetsUnion raw_)
                                   <*> (GH.parseField #targetsGroup raw_)
                                   <*> (GH.parseField #targetsInterface raw_)
                                   <*> (GH.parseField #targetsMethod raw_)
                                   <*> (GH.parseField #targetsParam raw_)
                                   <*> (GH.parseField #targetsAnnotation raw_))
instance (C.Marshal Node'annotation (C.Parsed Node'annotation)) where
    marshalInto raw_ Node'annotation'{..} = (do
        (GH.encodeField #type_ type_ raw_)
        (GH.encodeField #targetsFile targetsFile raw_)
        (GH.encodeField #targetsConst targetsConst raw_)
        (GH.encodeField #targetsEnum targetsEnum raw_)
        (GH.encodeField #targetsEnumerant targetsEnumerant raw_)
        (GH.encodeField #targetsStruct targetsStruct raw_)
        (GH.encodeField #targetsField targetsField raw_)
        (GH.encodeField #targetsUnion targetsUnion raw_)
        (GH.encodeField #targetsGroup targetsGroup raw_)
        (GH.encodeField #targetsInterface targetsInterface raw_)
        (GH.encodeField #targetsMethod targetsMethod raw_)
        (GH.encodeField #targetsParam targetsParam raw_)
        (GH.encodeField #targetsAnnotation targetsAnnotation raw_)
        (Std_.pure ())
        )
instance (GH.HasField "type_" GH.Slot Node'annotation Type) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "targetsFile" GH.Slot Node'annotation Std_.Bool) where
    fieldByLabel  = (GH.dataField 48 1 1 0)
instance (GH.HasField "targetsConst" GH.Slot Node'annotation Std_.Bool) where
    fieldByLabel  = (GH.dataField 49 1 1 0)
instance (GH.HasField "targetsEnum" GH.Slot Node'annotation Std_.Bool) where
    fieldByLabel  = (GH.dataField 50 1 1 0)
instance (GH.HasField "targetsEnumerant" GH.Slot Node'annotation Std_.Bool) where
    fieldByLabel  = (GH.dataField 51 1 1 0)
instance (GH.HasField "targetsStruct" GH.Slot Node'annotation Std_.Bool) where
    fieldByLabel  = (GH.dataField 52 1 1 0)
instance (GH.HasField "targetsField" GH.Slot Node'annotation Std_.Bool) where
    fieldByLabel  = (GH.dataField 53 1 1 0)
instance (GH.HasField "targetsUnion" GH.Slot Node'annotation Std_.Bool) where
    fieldByLabel  = (GH.dataField 54 1 1 0)
instance (GH.HasField "targetsGroup" GH.Slot Node'annotation Std_.Bool) where
    fieldByLabel  = (GH.dataField 55 1 1 0)
instance (GH.HasField "targetsInterface" GH.Slot Node'annotation Std_.Bool) where
    fieldByLabel  = (GH.dataField 56 1 1 0)
instance (GH.HasField "targetsMethod" GH.Slot Node'annotation Std_.Bool) where
    fieldByLabel  = (GH.dataField 57 1 1 0)
instance (GH.HasField "targetsParam" GH.Slot Node'annotation Std_.Bool) where
    fieldByLabel  = (GH.dataField 58 1 1 0)
instance (GH.HasField "targetsAnnotation" GH.Slot Node'annotation Std_.Bool) where
    fieldByLabel  = (GH.dataField 59 1 1 0)
data Node'Parameter 
type instance (R.ReprFor Node'Parameter) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'Parameter) where
    typeId  = 13353766412138554289
instance (C.TypedStruct Node'Parameter) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Node'Parameter) where
    type AllocHint Node'Parameter = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'Parameter (C.Parsed Node'Parameter))
instance (C.AllocateList Node'Parameter) where
    type ListAllocHint Node'Parameter = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'Parameter (C.Parsed Node'Parameter))
data instance C.Parsed Node'Parameter
    = Node'Parameter 
        {name :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'Parameter))
deriving instance (Std_.Eq (C.Parsed Node'Parameter))
instance (C.Parse Node'Parameter (C.Parsed Node'Parameter)) where
    parse raw_ = (Node'Parameter <$> (GH.parseField #name raw_))
instance (C.Marshal Node'Parameter (C.Parsed Node'Parameter)) where
    marshalInto raw_ Node'Parameter{..} = (do
        (GH.encodeField #name name raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot Node'Parameter Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Node'NestedNode 
type instance (R.ReprFor Node'NestedNode) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'NestedNode) where
    typeId  = 16050641862814319170
instance (C.TypedStruct Node'NestedNode) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Node'NestedNode) where
    type AllocHint Node'NestedNode = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'NestedNode (C.Parsed Node'NestedNode))
instance (C.AllocateList Node'NestedNode) where
    type ListAllocHint Node'NestedNode = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'NestedNode (C.Parsed Node'NestedNode))
data instance C.Parsed Node'NestedNode
    = Node'NestedNode 
        {name :: (RP.Parsed Basics.Text)
        ,id :: (RP.Parsed Std_.Word64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'NestedNode))
deriving instance (Std_.Eq (C.Parsed Node'NestedNode))
instance (C.Parse Node'NestedNode (C.Parsed Node'NestedNode)) where
    parse raw_ = (Node'NestedNode <$> (GH.parseField #name raw_)
                                  <*> (GH.parseField #id raw_))
instance (C.Marshal Node'NestedNode (C.Parsed Node'NestedNode)) where
    marshalInto raw_ Node'NestedNode{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #id id raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot Node'NestedNode Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "id" GH.Slot Node'NestedNode Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Node'SourceInfo 
type instance (R.ReprFor Node'SourceInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'SourceInfo) where
    typeId  = 17549997658772559790
instance (C.TypedStruct Node'SourceInfo) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Node'SourceInfo) where
    type AllocHint Node'SourceInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'SourceInfo (C.Parsed Node'SourceInfo))
instance (C.AllocateList Node'SourceInfo) where
    type ListAllocHint Node'SourceInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'SourceInfo (C.Parsed Node'SourceInfo))
data instance C.Parsed Node'SourceInfo
    = Node'SourceInfo 
        {id :: (RP.Parsed Std_.Word64)
        ,docComment :: (RP.Parsed Basics.Text)
        ,members :: (RP.Parsed (R.List Node'SourceInfo'Member))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'SourceInfo))
deriving instance (Std_.Eq (C.Parsed Node'SourceInfo))
instance (C.Parse Node'SourceInfo (C.Parsed Node'SourceInfo)) where
    parse raw_ = (Node'SourceInfo <$> (GH.parseField #id raw_)
                                  <*> (GH.parseField #docComment raw_)
                                  <*> (GH.parseField #members raw_))
instance (C.Marshal Node'SourceInfo (C.Parsed Node'SourceInfo)) where
    marshalInto raw_ Node'SourceInfo{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #docComment docComment raw_)
        (GH.encodeField #members members raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot Node'SourceInfo Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "docComment" GH.Slot Node'SourceInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "members" GH.Slot Node'SourceInfo (R.List Node'SourceInfo'Member)) where
    fieldByLabel  = (GH.ptrField 1)
data Node'SourceInfo'Member 
type instance (R.ReprFor Node'SourceInfo'Member) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'SourceInfo'Member) where
    typeId  = 14031686161526562722
instance (C.TypedStruct Node'SourceInfo'Member) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Node'SourceInfo'Member) where
    type AllocHint Node'SourceInfo'Member = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'SourceInfo'Member (C.Parsed Node'SourceInfo'Member))
instance (C.AllocateList Node'SourceInfo'Member) where
    type ListAllocHint Node'SourceInfo'Member = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'SourceInfo'Member (C.Parsed Node'SourceInfo'Member))
data instance C.Parsed Node'SourceInfo'Member
    = Node'SourceInfo'Member 
        {docComment :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'SourceInfo'Member))
deriving instance (Std_.Eq (C.Parsed Node'SourceInfo'Member))
instance (C.Parse Node'SourceInfo'Member (C.Parsed Node'SourceInfo'Member)) where
    parse raw_ = (Node'SourceInfo'Member <$> (GH.parseField #docComment raw_))
instance (C.Marshal Node'SourceInfo'Member (C.Parsed Node'SourceInfo'Member)) where
    marshalInto raw_ Node'SourceInfo'Member{..} = (do
        (GH.encodeField #docComment docComment raw_)
        (Std_.pure ())
        )
instance (GH.HasField "docComment" GH.Slot Node'SourceInfo'Member Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Field 
type instance (R.ReprFor Field) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Field) where
    typeId  = 11145653318641710175
instance (C.TypedStruct Field) where
    numStructWords  = 3
    numStructPtrs  = 4
instance (C.Allocate Field) where
    type AllocHint Field = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Field (C.Parsed Field))
instance (C.AllocateList Field) where
    type ListAllocHint Field = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Field (C.Parsed Field))
data instance C.Parsed Field
    = Field 
        {name :: (RP.Parsed Basics.Text)
        ,codeOrder :: (RP.Parsed Std_.Word16)
        ,annotations :: (RP.Parsed (R.List Annotation))
        ,discriminantValue :: (RP.Parsed Std_.Word16)
        ,ordinal :: (RP.Parsed Field'ordinal)
        ,union' :: (C.Parsed (GH.Which Field))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Field))
deriving instance (Std_.Eq (C.Parsed Field))
instance (C.Parse Field (C.Parsed Field)) where
    parse raw_ = (Field <$> (GH.parseField #name raw_)
                        <*> (GH.parseField #codeOrder raw_)
                        <*> (GH.parseField #annotations raw_)
                        <*> (GH.parseField #discriminantValue raw_)
                        <*> (GH.parseField #ordinal raw_)
                        <*> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Field (C.Parsed Field)) where
    marshalInto raw_ Field{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #codeOrder codeOrder raw_)
        (GH.encodeField #annotations annotations raw_)
        (GH.encodeField #discriminantValue discriminantValue raw_)
        (do
            group_ <- (GH.readField #ordinal raw_)
            (C.marshalInto group_ ordinal)
            )
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Field) where
    unionField  = (GH.dataField 0 1 16 0)
    data RawWhich Field mut_
        = RW_Field'slot (R.Raw Field'slot mut_)
        | RW_Field'group (R.Raw Field'group mut_)
        | RW_Field'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Field'slot <$> (GH.readVariant #slot struct_))
        1 ->
            (RW_Field'group <$> (GH.readVariant #group struct_))
        _ ->
            (Std_.pure (RW_Field'unknown' tag_))
    data Which Field
instance (GH.HasVariant "slot" GH.Group Field Field'slot) where
    variantByLabel  = (GH.Variant GH.groupField 0)
instance (GH.HasVariant "group" GH.Group Field Field'group) where
    variantByLabel  = (GH.Variant GH.groupField 1)
data instance C.Parsed (GH.Which Field)
    = Field'slot (RP.Parsed Field'slot)
    | Field'group (RP.Parsed Field'group)
    | Field'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Field)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Field)))
instance (C.Parse (GH.Which Field) (C.Parsed (GH.Which Field))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Field'slot rawArg_) ->
                (Field'slot <$> (C.parse rawArg_))
            (RW_Field'group rawArg_) ->
                (Field'group <$> (C.parse rawArg_))
            (RW_Field'unknown' tag_) ->
                (Std_.pure (Field'unknown' tag_))
        )
instance (C.Marshal (GH.Which Field) (C.Parsed (GH.Which Field))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Field'slot arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #slot (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Field'group arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #group (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Field'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
instance (GH.HasField "name" GH.Slot Field Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "codeOrder" GH.Slot Field Std_.Word16) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "annotations" GH.Slot Field (R.List Annotation)) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "discriminantValue" GH.Slot Field Std_.Word16) where
    fieldByLabel  = (GH.dataField 16 0 16 65535)
instance (GH.HasField "ordinal" GH.Group Field Field'ordinal) where
    fieldByLabel  = GH.groupField
data Field'slot 
type instance (R.ReprFor Field'slot) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Field'slot) where
    typeId  = 14133145859926553711
instance (C.TypedStruct Field'slot) where
    numStructWords  = 3
    numStructPtrs  = 4
instance (C.Allocate Field'slot) where
    type AllocHint Field'slot = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Field'slot (C.Parsed Field'slot))
instance (C.AllocateList Field'slot) where
    type ListAllocHint Field'slot = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Field'slot (C.Parsed Field'slot))
data instance C.Parsed Field'slot
    = Field'slot' 
        {offset :: (RP.Parsed Std_.Word32)
        ,type_ :: (RP.Parsed Type)
        ,defaultValue :: (RP.Parsed Value)
        ,hadExplicitDefault :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Field'slot))
deriving instance (Std_.Eq (C.Parsed Field'slot))
instance (C.Parse Field'slot (C.Parsed Field'slot)) where
    parse raw_ = (Field'slot' <$> (GH.parseField #offset raw_)
                              <*> (GH.parseField #type_ raw_)
                              <*> (GH.parseField #defaultValue raw_)
                              <*> (GH.parseField #hadExplicitDefault raw_))
instance (C.Marshal Field'slot (C.Parsed Field'slot)) where
    marshalInto raw_ Field'slot'{..} = (do
        (GH.encodeField #offset offset raw_)
        (GH.encodeField #type_ type_ raw_)
        (GH.encodeField #defaultValue defaultValue raw_)
        (GH.encodeField #hadExplicitDefault hadExplicitDefault raw_)
        (Std_.pure ())
        )
instance (GH.HasField "offset" GH.Slot Field'slot Std_.Word32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
instance (GH.HasField "type_" GH.Slot Field'slot Type) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "defaultValue" GH.Slot Field'slot Value) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "hadExplicitDefault" GH.Slot Field'slot Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 2 1 0)
data Field'group 
type instance (R.ReprFor Field'group) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Field'group) where
    typeId  = 14626792032033250577
instance (C.TypedStruct Field'group) where
    numStructWords  = 3
    numStructPtrs  = 4
instance (C.Allocate Field'group) where
    type AllocHint Field'group = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Field'group (C.Parsed Field'group))
instance (C.AllocateList Field'group) where
    type ListAllocHint Field'group = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Field'group (C.Parsed Field'group))
data instance C.Parsed Field'group
    = Field'group' 
        {typeId :: (RP.Parsed Std_.Word64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Field'group))
deriving instance (Std_.Eq (C.Parsed Field'group))
instance (C.Parse Field'group (C.Parsed Field'group)) where
    parse raw_ = (Field'group' <$> (GH.parseField #typeId raw_))
instance (C.Marshal Field'group (C.Parsed Field'group)) where
    marshalInto raw_ Field'group'{..} = (do
        (GH.encodeField #typeId typeId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "typeId" GH.Slot Field'group Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
data Field'ordinal 
type instance (R.ReprFor Field'ordinal) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Field'ordinal) where
    typeId  = 13515537513213004774
instance (C.TypedStruct Field'ordinal) where
    numStructWords  = 3
    numStructPtrs  = 4
instance (C.Allocate Field'ordinal) where
    type AllocHint Field'ordinal = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Field'ordinal (C.Parsed Field'ordinal))
instance (C.AllocateList Field'ordinal) where
    type ListAllocHint Field'ordinal = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Field'ordinal (C.Parsed Field'ordinal))
data instance C.Parsed Field'ordinal
    = Field'ordinal' 
        {union' :: (C.Parsed (GH.Which Field'ordinal))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Field'ordinal))
deriving instance (Std_.Eq (C.Parsed Field'ordinal))
instance (C.Parse Field'ordinal (C.Parsed Field'ordinal)) where
    parse raw_ = (Field'ordinal' <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Field'ordinal (C.Parsed Field'ordinal)) where
    marshalInto raw_ Field'ordinal'{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Field'ordinal) where
    unionField  = (GH.dataField 16 1 16 0)
    data RawWhich Field'ordinal mut_
        = RW_Field'ordinal'implicit (R.Raw () mut_)
        | RW_Field'ordinal'explicit (R.Raw Std_.Word16 mut_)
        | RW_Field'ordinal'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Field'ordinal'implicit <$> (GH.readVariant #implicit struct_))
        1 ->
            (RW_Field'ordinal'explicit <$> (GH.readVariant #explicit struct_))
        _ ->
            (Std_.pure (RW_Field'ordinal'unknown' tag_))
    data Which Field'ordinal
instance (GH.HasVariant "implicit" GH.Slot Field'ordinal ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance (GH.HasVariant "explicit" GH.Slot Field'ordinal Std_.Word16) where
    variantByLabel  = (GH.Variant (GH.dataField 32 1 16 0) 1)
data instance C.Parsed (GH.Which Field'ordinal)
    = Field'ordinal'implicit 
    | Field'ordinal'explicit (RP.Parsed Std_.Word16)
    | Field'ordinal'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Field'ordinal)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Field'ordinal)))
instance (C.Parse (GH.Which Field'ordinal) (C.Parsed (GH.Which Field'ordinal))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Field'ordinal'implicit _) ->
                (Std_.pure Field'ordinal'implicit)
            (RW_Field'ordinal'explicit rawArg_) ->
                (Field'ordinal'explicit <$> (C.parse rawArg_))
            (RW_Field'ordinal'unknown' tag_) ->
                (Std_.pure (Field'ordinal'unknown' tag_))
        )
instance (C.Marshal (GH.Which Field'ordinal) (C.Parsed (GH.Which Field'ordinal))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Field'ordinal'implicit) ->
            (GH.encodeVariant #implicit () (GH.unionStruct raw_))
        (Field'ordinal'explicit arg_) ->
            (GH.encodeVariant #explicit arg_ (GH.unionStruct raw_))
        (Field'ordinal'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
field'noDiscriminant :: Std_.Word16
field'noDiscriminant  = (C.fromWord 65535)
data Enumerant 
type instance (R.ReprFor Enumerant) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Enumerant) where
    typeId  = 10919677598968879693
instance (C.TypedStruct Enumerant) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Enumerant) where
    type AllocHint Enumerant = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Enumerant (C.Parsed Enumerant))
instance (C.AllocateList Enumerant) where
    type ListAllocHint Enumerant = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Enumerant (C.Parsed Enumerant))
data instance C.Parsed Enumerant
    = Enumerant 
        {name :: (RP.Parsed Basics.Text)
        ,codeOrder :: (RP.Parsed Std_.Word16)
        ,annotations :: (RP.Parsed (R.List Annotation))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Enumerant))
deriving instance (Std_.Eq (C.Parsed Enumerant))
instance (C.Parse Enumerant (C.Parsed Enumerant)) where
    parse raw_ = (Enumerant <$> (GH.parseField #name raw_)
                            <*> (GH.parseField #codeOrder raw_)
                            <*> (GH.parseField #annotations raw_))
instance (C.Marshal Enumerant (C.Parsed Enumerant)) where
    marshalInto raw_ Enumerant{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #codeOrder codeOrder raw_)
        (GH.encodeField #annotations annotations raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot Enumerant Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "codeOrder" GH.Slot Enumerant Std_.Word16) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "annotations" GH.Slot Enumerant (R.List Annotation)) where
    fieldByLabel  = (GH.ptrField 1)
data Superclass 
type instance (R.ReprFor Superclass) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Superclass) where
    typeId  = 12220001500510083064
instance (C.TypedStruct Superclass) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Superclass) where
    type AllocHint Superclass = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Superclass (C.Parsed Superclass))
instance (C.AllocateList Superclass) where
    type ListAllocHint Superclass = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Superclass (C.Parsed Superclass))
data instance C.Parsed Superclass
    = Superclass 
        {id :: (RP.Parsed Std_.Word64)
        ,brand :: (RP.Parsed Brand)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Superclass))
deriving instance (Std_.Eq (C.Parsed Superclass))
instance (C.Parse Superclass (C.Parsed Superclass)) where
    parse raw_ = (Superclass <$> (GH.parseField #id raw_)
                             <*> (GH.parseField #brand raw_))
instance (C.Marshal Superclass (C.Parsed Superclass)) where
    marshalInto raw_ Superclass{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #brand brand raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot Superclass Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "brand" GH.Slot Superclass Brand) where
    fieldByLabel  = (GH.ptrField 0)
data Method 
type instance (R.ReprFor Method) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Method) where
    typeId  = 10736806783679155584
instance (C.TypedStruct Method) where
    numStructWords  = 3
    numStructPtrs  = 5
instance (C.Allocate Method) where
    type AllocHint Method = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Method (C.Parsed Method))
instance (C.AllocateList Method) where
    type ListAllocHint Method = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Method (C.Parsed Method))
data instance C.Parsed Method
    = Method 
        {name :: (RP.Parsed Basics.Text)
        ,codeOrder :: (RP.Parsed Std_.Word16)
        ,paramStructType :: (RP.Parsed Std_.Word64)
        ,resultStructType :: (RP.Parsed Std_.Word64)
        ,annotations :: (RP.Parsed (R.List Annotation))
        ,paramBrand :: (RP.Parsed Brand)
        ,resultBrand :: (RP.Parsed Brand)
        ,implicitParameters :: (RP.Parsed (R.List Node'Parameter))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Method))
deriving instance (Std_.Eq (C.Parsed Method))
instance (C.Parse Method (C.Parsed Method)) where
    parse raw_ = (Method <$> (GH.parseField #name raw_)
                         <*> (GH.parseField #codeOrder raw_)
                         <*> (GH.parseField #paramStructType raw_)
                         <*> (GH.parseField #resultStructType raw_)
                         <*> (GH.parseField #annotations raw_)
                         <*> (GH.parseField #paramBrand raw_)
                         <*> (GH.parseField #resultBrand raw_)
                         <*> (GH.parseField #implicitParameters raw_))
instance (C.Marshal Method (C.Parsed Method)) where
    marshalInto raw_ Method{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #codeOrder codeOrder raw_)
        (GH.encodeField #paramStructType paramStructType raw_)
        (GH.encodeField #resultStructType resultStructType raw_)
        (GH.encodeField #annotations annotations raw_)
        (GH.encodeField #paramBrand paramBrand raw_)
        (GH.encodeField #resultBrand resultBrand raw_)
        (GH.encodeField #implicitParameters implicitParameters raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot Method Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "codeOrder" GH.Slot Method Std_.Word16) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "paramStructType" GH.Slot Method Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "resultStructType" GH.Slot Method Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "annotations" GH.Slot Method (R.List Annotation)) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "paramBrand" GH.Slot Method Brand) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "resultBrand" GH.Slot Method Brand) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "implicitParameters" GH.Slot Method (R.List Node'Parameter)) where
    fieldByLabel  = (GH.ptrField 4)
data Type 
type instance (R.ReprFor Type) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Type) where
    typeId  = 15020482145304562784
instance (C.TypedStruct Type) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type) where
    type AllocHint Type = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Type (C.Parsed Type))
instance (C.AllocateList Type) where
    type ListAllocHint Type = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Type (C.Parsed Type))
data instance C.Parsed Type
    = Type 
        {union' :: (C.Parsed (GH.Which Type))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Type))
deriving instance (Std_.Eq (C.Parsed Type))
instance (C.Parse Type (C.Parsed Type)) where
    parse raw_ = (Type <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Type (C.Parsed Type)) where
    marshalInto raw_ Type{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Type) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich Type mut_
        = RW_Type'void (R.Raw () mut_)
        | RW_Type'bool (R.Raw () mut_)
        | RW_Type'int8 (R.Raw () mut_)
        | RW_Type'int16 (R.Raw () mut_)
        | RW_Type'int32 (R.Raw () mut_)
        | RW_Type'int64 (R.Raw () mut_)
        | RW_Type'uint8 (R.Raw () mut_)
        | RW_Type'uint16 (R.Raw () mut_)
        | RW_Type'uint32 (R.Raw () mut_)
        | RW_Type'uint64 (R.Raw () mut_)
        | RW_Type'float32 (R.Raw () mut_)
        | RW_Type'float64 (R.Raw () mut_)
        | RW_Type'text (R.Raw () mut_)
        | RW_Type'data_ (R.Raw () mut_)
        | RW_Type'list (R.Raw Type'list mut_)
        | RW_Type'enum (R.Raw Type'enum mut_)
        | RW_Type'struct (R.Raw Type'struct mut_)
        | RW_Type'interface (R.Raw Type'interface mut_)
        | RW_Type'anyPointer (R.Raw Type'anyPointer mut_)
        | RW_Type'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Type'void <$> (GH.readVariant #void struct_))
        1 ->
            (RW_Type'bool <$> (GH.readVariant #bool struct_))
        2 ->
            (RW_Type'int8 <$> (GH.readVariant #int8 struct_))
        3 ->
            (RW_Type'int16 <$> (GH.readVariant #int16 struct_))
        4 ->
            (RW_Type'int32 <$> (GH.readVariant #int32 struct_))
        5 ->
            (RW_Type'int64 <$> (GH.readVariant #int64 struct_))
        6 ->
            (RW_Type'uint8 <$> (GH.readVariant #uint8 struct_))
        7 ->
            (RW_Type'uint16 <$> (GH.readVariant #uint16 struct_))
        8 ->
            (RW_Type'uint32 <$> (GH.readVariant #uint32 struct_))
        9 ->
            (RW_Type'uint64 <$> (GH.readVariant #uint64 struct_))
        10 ->
            (RW_Type'float32 <$> (GH.readVariant #float32 struct_))
        11 ->
            (RW_Type'float64 <$> (GH.readVariant #float64 struct_))
        12 ->
            (RW_Type'text <$> (GH.readVariant #text struct_))
        13 ->
            (RW_Type'data_ <$> (GH.readVariant #data_ struct_))
        14 ->
            (RW_Type'list <$> (GH.readVariant #list struct_))
        15 ->
            (RW_Type'enum <$> (GH.readVariant #enum struct_))
        16 ->
            (RW_Type'struct <$> (GH.readVariant #struct struct_))
        17 ->
            (RW_Type'interface <$> (GH.readVariant #interface struct_))
        18 ->
            (RW_Type'anyPointer <$> (GH.readVariant #anyPointer struct_))
        _ ->
            (Std_.pure (RW_Type'unknown' tag_))
    data Which Type
instance (GH.HasVariant "void" GH.Slot Type ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance (GH.HasVariant "bool" GH.Slot Type ()) where
    variantByLabel  = (GH.Variant GH.voidField 1)
instance (GH.HasVariant "int8" GH.Slot Type ()) where
    variantByLabel  = (GH.Variant GH.voidField 2)
instance (GH.HasVariant "int16" GH.Slot Type ()) where
    variantByLabel  = (GH.Variant GH.voidField 3)
instance (GH.HasVariant "int32" GH.Slot Type ()) where
    variantByLabel  = (GH.Variant GH.voidField 4)
instance (GH.HasVariant "int64" GH.Slot Type ()) where
    variantByLabel  = (GH.Variant GH.voidField 5)
instance (GH.HasVariant "uint8" GH.Slot Type ()) where
    variantByLabel  = (GH.Variant GH.voidField 6)
instance (GH.HasVariant "uint16" GH.Slot Type ()) where
    variantByLabel  = (GH.Variant GH.voidField 7)
instance (GH.HasVariant "uint32" GH.Slot Type ()) where
    variantByLabel  = (GH.Variant GH.voidField 8)
instance (GH.HasVariant "uint64" GH.Slot Type ()) where
    variantByLabel  = (GH.Variant GH.voidField 9)
instance (GH.HasVariant "float32" GH.Slot Type ()) where
    variantByLabel  = (GH.Variant GH.voidField 10)
instance (GH.HasVariant "float64" GH.Slot Type ()) where
    variantByLabel  = (GH.Variant GH.voidField 11)
instance (GH.HasVariant "text" GH.Slot Type ()) where
    variantByLabel  = (GH.Variant GH.voidField 12)
instance (GH.HasVariant "data_" GH.Slot Type ()) where
    variantByLabel  = (GH.Variant GH.voidField 13)
instance (GH.HasVariant "list" GH.Group Type Type'list) where
    variantByLabel  = (GH.Variant GH.groupField 14)
instance (GH.HasVariant "enum" GH.Group Type Type'enum) where
    variantByLabel  = (GH.Variant GH.groupField 15)
instance (GH.HasVariant "struct" GH.Group Type Type'struct) where
    variantByLabel  = (GH.Variant GH.groupField 16)
instance (GH.HasVariant "interface" GH.Group Type Type'interface) where
    variantByLabel  = (GH.Variant GH.groupField 17)
instance (GH.HasVariant "anyPointer" GH.Group Type Type'anyPointer) where
    variantByLabel  = (GH.Variant GH.groupField 18)
data instance C.Parsed (GH.Which Type)
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
    | Type'list (RP.Parsed Type'list)
    | Type'enum (RP.Parsed Type'enum)
    | Type'struct (RP.Parsed Type'struct)
    | Type'interface (RP.Parsed Type'interface)
    | Type'anyPointer (RP.Parsed Type'anyPointer)
    | Type'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Type)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Type)))
instance (C.Parse (GH.Which Type) (C.Parsed (GH.Which Type))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Type'void _) ->
                (Std_.pure Type'void)
            (RW_Type'bool _) ->
                (Std_.pure Type'bool)
            (RW_Type'int8 _) ->
                (Std_.pure Type'int8)
            (RW_Type'int16 _) ->
                (Std_.pure Type'int16)
            (RW_Type'int32 _) ->
                (Std_.pure Type'int32)
            (RW_Type'int64 _) ->
                (Std_.pure Type'int64)
            (RW_Type'uint8 _) ->
                (Std_.pure Type'uint8)
            (RW_Type'uint16 _) ->
                (Std_.pure Type'uint16)
            (RW_Type'uint32 _) ->
                (Std_.pure Type'uint32)
            (RW_Type'uint64 _) ->
                (Std_.pure Type'uint64)
            (RW_Type'float32 _) ->
                (Std_.pure Type'float32)
            (RW_Type'float64 _) ->
                (Std_.pure Type'float64)
            (RW_Type'text _) ->
                (Std_.pure Type'text)
            (RW_Type'data_ _) ->
                (Std_.pure Type'data_)
            (RW_Type'list rawArg_) ->
                (Type'list <$> (C.parse rawArg_))
            (RW_Type'enum rawArg_) ->
                (Type'enum <$> (C.parse rawArg_))
            (RW_Type'struct rawArg_) ->
                (Type'struct <$> (C.parse rawArg_))
            (RW_Type'interface rawArg_) ->
                (Type'interface <$> (C.parse rawArg_))
            (RW_Type'anyPointer rawArg_) ->
                (Type'anyPointer <$> (C.parse rawArg_))
            (RW_Type'unknown' tag_) ->
                (Std_.pure (Type'unknown' tag_))
        )
instance (C.Marshal (GH.Which Type) (C.Parsed (GH.Which Type))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Type'void) ->
            (GH.encodeVariant #void () (GH.unionStruct raw_))
        (Type'bool) ->
            (GH.encodeVariant #bool () (GH.unionStruct raw_))
        (Type'int8) ->
            (GH.encodeVariant #int8 () (GH.unionStruct raw_))
        (Type'int16) ->
            (GH.encodeVariant #int16 () (GH.unionStruct raw_))
        (Type'int32) ->
            (GH.encodeVariant #int32 () (GH.unionStruct raw_))
        (Type'int64) ->
            (GH.encodeVariant #int64 () (GH.unionStruct raw_))
        (Type'uint8) ->
            (GH.encodeVariant #uint8 () (GH.unionStruct raw_))
        (Type'uint16) ->
            (GH.encodeVariant #uint16 () (GH.unionStruct raw_))
        (Type'uint32) ->
            (GH.encodeVariant #uint32 () (GH.unionStruct raw_))
        (Type'uint64) ->
            (GH.encodeVariant #uint64 () (GH.unionStruct raw_))
        (Type'float32) ->
            (GH.encodeVariant #float32 () (GH.unionStruct raw_))
        (Type'float64) ->
            (GH.encodeVariant #float64 () (GH.unionStruct raw_))
        (Type'text) ->
            (GH.encodeVariant #text () (GH.unionStruct raw_))
        (Type'data_) ->
            (GH.encodeVariant #data_ () (GH.unionStruct raw_))
        (Type'list arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #list (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Type'enum arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #enum (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Type'struct arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #struct (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Type'interface arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #interface (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Type'anyPointer arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #anyPointer (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Type'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Type'list 
type instance (R.ReprFor Type'list) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Type'list) where
    typeId  = 9792858745991129751
instance (C.TypedStruct Type'list) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'list) where
    type AllocHint Type'list = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Type'list (C.Parsed Type'list))
instance (C.AllocateList Type'list) where
    type ListAllocHint Type'list = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Type'list (C.Parsed Type'list))
data instance C.Parsed Type'list
    = Type'list' 
        {elementType :: (RP.Parsed Type)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Type'list))
deriving instance (Std_.Eq (C.Parsed Type'list))
instance (C.Parse Type'list (C.Parsed Type'list)) where
    parse raw_ = (Type'list' <$> (GH.parseField #elementType raw_))
instance (C.Marshal Type'list (C.Parsed Type'list)) where
    marshalInto raw_ Type'list'{..} = (do
        (GH.encodeField #elementType elementType raw_)
        (Std_.pure ())
        )
instance (GH.HasField "elementType" GH.Slot Type'list Type) where
    fieldByLabel  = (GH.ptrField 0)
data Type'enum 
type instance (R.ReprFor Type'enum) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Type'enum) where
    typeId  = 11389172934837766057
instance (C.TypedStruct Type'enum) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'enum) where
    type AllocHint Type'enum = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Type'enum (C.Parsed Type'enum))
instance (C.AllocateList Type'enum) where
    type ListAllocHint Type'enum = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Type'enum (C.Parsed Type'enum))
data instance C.Parsed Type'enum
    = Type'enum' 
        {typeId :: (RP.Parsed Std_.Word64)
        ,brand :: (RP.Parsed Brand)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Type'enum))
deriving instance (Std_.Eq (C.Parsed Type'enum))
instance (C.Parse Type'enum (C.Parsed Type'enum)) where
    parse raw_ = (Type'enum' <$> (GH.parseField #typeId raw_)
                             <*> (GH.parseField #brand raw_))
instance (C.Marshal Type'enum (C.Parsed Type'enum)) where
    marshalInto raw_ Type'enum'{..} = (do
        (GH.encodeField #typeId typeId raw_)
        (GH.encodeField #brand brand raw_)
        (Std_.pure ())
        )
instance (GH.HasField "typeId" GH.Slot Type'enum Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "brand" GH.Slot Type'enum Brand) where
    fieldByLabel  = (GH.ptrField 0)
data Type'struct 
type instance (R.ReprFor Type'struct) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Type'struct) where
    typeId  = 12410354185295152851
instance (C.TypedStruct Type'struct) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'struct) where
    type AllocHint Type'struct = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Type'struct (C.Parsed Type'struct))
instance (C.AllocateList Type'struct) where
    type ListAllocHint Type'struct = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Type'struct (C.Parsed Type'struct))
data instance C.Parsed Type'struct
    = Type'struct' 
        {typeId :: (RP.Parsed Std_.Word64)
        ,brand :: (RP.Parsed Brand)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Type'struct))
deriving instance (Std_.Eq (C.Parsed Type'struct))
instance (C.Parse Type'struct (C.Parsed Type'struct)) where
    parse raw_ = (Type'struct' <$> (GH.parseField #typeId raw_)
                               <*> (GH.parseField #brand raw_))
instance (C.Marshal Type'struct (C.Parsed Type'struct)) where
    marshalInto raw_ Type'struct'{..} = (do
        (GH.encodeField #typeId typeId raw_)
        (GH.encodeField #brand brand raw_)
        (Std_.pure ())
        )
instance (GH.HasField "typeId" GH.Slot Type'struct Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "brand" GH.Slot Type'struct Brand) where
    fieldByLabel  = (GH.ptrField 0)
data Type'interface 
type instance (R.ReprFor Type'interface) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Type'interface) where
    typeId  = 17116997365232503999
instance (C.TypedStruct Type'interface) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'interface) where
    type AllocHint Type'interface = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Type'interface (C.Parsed Type'interface))
instance (C.AllocateList Type'interface) where
    type ListAllocHint Type'interface = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Type'interface (C.Parsed Type'interface))
data instance C.Parsed Type'interface
    = Type'interface' 
        {typeId :: (RP.Parsed Std_.Word64)
        ,brand :: (RP.Parsed Brand)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Type'interface))
deriving instance (Std_.Eq (C.Parsed Type'interface))
instance (C.Parse Type'interface (C.Parsed Type'interface)) where
    parse raw_ = (Type'interface' <$> (GH.parseField #typeId raw_)
                                  <*> (GH.parseField #brand raw_))
instance (C.Marshal Type'interface (C.Parsed Type'interface)) where
    marshalInto raw_ Type'interface'{..} = (do
        (GH.encodeField #typeId typeId raw_)
        (GH.encodeField #brand brand raw_)
        (Std_.pure ())
        )
instance (GH.HasField "typeId" GH.Slot Type'interface Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "brand" GH.Slot Type'interface Brand) where
    fieldByLabel  = (GH.ptrField 0)
data Type'anyPointer 
type instance (R.ReprFor Type'anyPointer) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Type'anyPointer) where
    typeId  = 14003731834718800369
instance (C.TypedStruct Type'anyPointer) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'anyPointer) where
    type AllocHint Type'anyPointer = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Type'anyPointer (C.Parsed Type'anyPointer))
instance (C.AllocateList Type'anyPointer) where
    type ListAllocHint Type'anyPointer = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Type'anyPointer (C.Parsed Type'anyPointer))
data instance C.Parsed Type'anyPointer
    = Type'anyPointer' 
        {union' :: (C.Parsed (GH.Which Type'anyPointer))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Type'anyPointer))
deriving instance (Std_.Eq (C.Parsed Type'anyPointer))
instance (C.Parse Type'anyPointer (C.Parsed Type'anyPointer)) where
    parse raw_ = (Type'anyPointer' <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Type'anyPointer (C.Parsed Type'anyPointer)) where
    marshalInto raw_ Type'anyPointer'{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Type'anyPointer) where
    unionField  = (GH.dataField 0 1 16 0)
    data RawWhich Type'anyPointer mut_
        = RW_Type'anyPointer'unconstrained (R.Raw Type'anyPointer'unconstrained mut_)
        | RW_Type'anyPointer'parameter (R.Raw Type'anyPointer'parameter mut_)
        | RW_Type'anyPointer'implicitMethodParameter (R.Raw Type'anyPointer'implicitMethodParameter mut_)
        | RW_Type'anyPointer'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Type'anyPointer'unconstrained <$> (GH.readVariant #unconstrained struct_))
        1 ->
            (RW_Type'anyPointer'parameter <$> (GH.readVariant #parameter struct_))
        2 ->
            (RW_Type'anyPointer'implicitMethodParameter <$> (GH.readVariant #implicitMethodParameter struct_))
        _ ->
            (Std_.pure (RW_Type'anyPointer'unknown' tag_))
    data Which Type'anyPointer
instance (GH.HasVariant "unconstrained" GH.Group Type'anyPointer Type'anyPointer'unconstrained) where
    variantByLabel  = (GH.Variant GH.groupField 0)
instance (GH.HasVariant "parameter" GH.Group Type'anyPointer Type'anyPointer'parameter) where
    variantByLabel  = (GH.Variant GH.groupField 1)
instance (GH.HasVariant "implicitMethodParameter" GH.Group Type'anyPointer Type'anyPointer'implicitMethodParameter) where
    variantByLabel  = (GH.Variant GH.groupField 2)
data instance C.Parsed (GH.Which Type'anyPointer)
    = Type'anyPointer'unconstrained (RP.Parsed Type'anyPointer'unconstrained)
    | Type'anyPointer'parameter (RP.Parsed Type'anyPointer'parameter)
    | Type'anyPointer'implicitMethodParameter (RP.Parsed Type'anyPointer'implicitMethodParameter)
    | Type'anyPointer'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Type'anyPointer)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Type'anyPointer)))
instance (C.Parse (GH.Which Type'anyPointer) (C.Parsed (GH.Which Type'anyPointer))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Type'anyPointer'unconstrained rawArg_) ->
                (Type'anyPointer'unconstrained <$> (C.parse rawArg_))
            (RW_Type'anyPointer'parameter rawArg_) ->
                (Type'anyPointer'parameter <$> (C.parse rawArg_))
            (RW_Type'anyPointer'implicitMethodParameter rawArg_) ->
                (Type'anyPointer'implicitMethodParameter <$> (C.parse rawArg_))
            (RW_Type'anyPointer'unknown' tag_) ->
                (Std_.pure (Type'anyPointer'unknown' tag_))
        )
instance (C.Marshal (GH.Which Type'anyPointer) (C.Parsed (GH.Which Type'anyPointer))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Type'anyPointer'unconstrained arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #unconstrained (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Type'anyPointer'parameter arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #parameter (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Type'anyPointer'implicitMethodParameter arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #implicitMethodParameter (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Type'anyPointer'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Type'anyPointer'unconstrained 
type instance (R.ReprFor Type'anyPointer'unconstrained) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Type'anyPointer'unconstrained) where
    typeId  = 10248890354574636630
instance (C.TypedStruct Type'anyPointer'unconstrained) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'anyPointer'unconstrained) where
    type AllocHint Type'anyPointer'unconstrained = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Type'anyPointer'unconstrained (C.Parsed Type'anyPointer'unconstrained))
instance (C.AllocateList Type'anyPointer'unconstrained) where
    type ListAllocHint Type'anyPointer'unconstrained = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Type'anyPointer'unconstrained (C.Parsed Type'anyPointer'unconstrained))
data instance C.Parsed Type'anyPointer'unconstrained
    = Type'anyPointer'unconstrained' 
        {union' :: (C.Parsed (GH.Which Type'anyPointer'unconstrained))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Type'anyPointer'unconstrained))
deriving instance (Std_.Eq (C.Parsed Type'anyPointer'unconstrained))
instance (C.Parse Type'anyPointer'unconstrained (C.Parsed Type'anyPointer'unconstrained)) where
    parse raw_ = (Type'anyPointer'unconstrained' <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Type'anyPointer'unconstrained (C.Parsed Type'anyPointer'unconstrained)) where
    marshalInto raw_ Type'anyPointer'unconstrained'{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Type'anyPointer'unconstrained) where
    unionField  = (GH.dataField 16 1 16 0)
    data RawWhich Type'anyPointer'unconstrained mut_
        = RW_Type'anyPointer'unconstrained'anyKind (R.Raw () mut_)
        | RW_Type'anyPointer'unconstrained'struct (R.Raw () mut_)
        | RW_Type'anyPointer'unconstrained'list (R.Raw () mut_)
        | RW_Type'anyPointer'unconstrained'capability (R.Raw () mut_)
        | RW_Type'anyPointer'unconstrained'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Type'anyPointer'unconstrained'anyKind <$> (GH.readVariant #anyKind struct_))
        1 ->
            (RW_Type'anyPointer'unconstrained'struct <$> (GH.readVariant #struct struct_))
        2 ->
            (RW_Type'anyPointer'unconstrained'list <$> (GH.readVariant #list struct_))
        3 ->
            (RW_Type'anyPointer'unconstrained'capability <$> (GH.readVariant #capability struct_))
        _ ->
            (Std_.pure (RW_Type'anyPointer'unconstrained'unknown' tag_))
    data Which Type'anyPointer'unconstrained
instance (GH.HasVariant "anyKind" GH.Slot Type'anyPointer'unconstrained ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance (GH.HasVariant "struct" GH.Slot Type'anyPointer'unconstrained ()) where
    variantByLabel  = (GH.Variant GH.voidField 1)
instance (GH.HasVariant "list" GH.Slot Type'anyPointer'unconstrained ()) where
    variantByLabel  = (GH.Variant GH.voidField 2)
instance (GH.HasVariant "capability" GH.Slot Type'anyPointer'unconstrained ()) where
    variantByLabel  = (GH.Variant GH.voidField 3)
data instance C.Parsed (GH.Which Type'anyPointer'unconstrained)
    = Type'anyPointer'unconstrained'anyKind 
    | Type'anyPointer'unconstrained'struct 
    | Type'anyPointer'unconstrained'list 
    | Type'anyPointer'unconstrained'capability 
    | Type'anyPointer'unconstrained'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Type'anyPointer'unconstrained)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Type'anyPointer'unconstrained)))
instance (C.Parse (GH.Which Type'anyPointer'unconstrained) (C.Parsed (GH.Which Type'anyPointer'unconstrained))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Type'anyPointer'unconstrained'anyKind _) ->
                (Std_.pure Type'anyPointer'unconstrained'anyKind)
            (RW_Type'anyPointer'unconstrained'struct _) ->
                (Std_.pure Type'anyPointer'unconstrained'struct)
            (RW_Type'anyPointer'unconstrained'list _) ->
                (Std_.pure Type'anyPointer'unconstrained'list)
            (RW_Type'anyPointer'unconstrained'capability _) ->
                (Std_.pure Type'anyPointer'unconstrained'capability)
            (RW_Type'anyPointer'unconstrained'unknown' tag_) ->
                (Std_.pure (Type'anyPointer'unconstrained'unknown' tag_))
        )
instance (C.Marshal (GH.Which Type'anyPointer'unconstrained) (C.Parsed (GH.Which Type'anyPointer'unconstrained))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Type'anyPointer'unconstrained'anyKind) ->
            (GH.encodeVariant #anyKind () (GH.unionStruct raw_))
        (Type'anyPointer'unconstrained'struct) ->
            (GH.encodeVariant #struct () (GH.unionStruct raw_))
        (Type'anyPointer'unconstrained'list) ->
            (GH.encodeVariant #list () (GH.unionStruct raw_))
        (Type'anyPointer'unconstrained'capability) ->
            (GH.encodeVariant #capability () (GH.unionStruct raw_))
        (Type'anyPointer'unconstrained'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Type'anyPointer'parameter 
type instance (R.ReprFor Type'anyPointer'parameter) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Type'anyPointer'parameter) where
    typeId  = 11372142272178113157
instance (C.TypedStruct Type'anyPointer'parameter) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'anyPointer'parameter) where
    type AllocHint Type'anyPointer'parameter = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Type'anyPointer'parameter (C.Parsed Type'anyPointer'parameter))
instance (C.AllocateList Type'anyPointer'parameter) where
    type ListAllocHint Type'anyPointer'parameter = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Type'anyPointer'parameter (C.Parsed Type'anyPointer'parameter))
data instance C.Parsed Type'anyPointer'parameter
    = Type'anyPointer'parameter' 
        {scopeId :: (RP.Parsed Std_.Word64)
        ,parameterIndex :: (RP.Parsed Std_.Word16)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Type'anyPointer'parameter))
deriving instance (Std_.Eq (C.Parsed Type'anyPointer'parameter))
instance (C.Parse Type'anyPointer'parameter (C.Parsed Type'anyPointer'parameter)) where
    parse raw_ = (Type'anyPointer'parameter' <$> (GH.parseField #scopeId raw_)
                                             <*> (GH.parseField #parameterIndex raw_))
instance (C.Marshal Type'anyPointer'parameter (C.Parsed Type'anyPointer'parameter)) where
    marshalInto raw_ Type'anyPointer'parameter'{..} = (do
        (GH.encodeField #scopeId scopeId raw_)
        (GH.encodeField #parameterIndex parameterIndex raw_)
        (Std_.pure ())
        )
instance (GH.HasField "scopeId" GH.Slot Type'anyPointer'parameter Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "parameterIndex" GH.Slot Type'anyPointer'parameter Std_.Word16) where
    fieldByLabel  = (GH.dataField 16 1 16 0)
data Type'anyPointer'implicitMethodParameter 
type instance (R.ReprFor Type'anyPointer'implicitMethodParameter) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Type'anyPointer'implicitMethodParameter) where
    typeId  = 13470206089842057844
instance (C.TypedStruct Type'anyPointer'implicitMethodParameter) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'anyPointer'implicitMethodParameter) where
    type AllocHint Type'anyPointer'implicitMethodParameter = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Type'anyPointer'implicitMethodParameter (C.Parsed Type'anyPointer'implicitMethodParameter))
instance (C.AllocateList Type'anyPointer'implicitMethodParameter) where
    type ListAllocHint Type'anyPointer'implicitMethodParameter = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Type'anyPointer'implicitMethodParameter (C.Parsed Type'anyPointer'implicitMethodParameter))
data instance C.Parsed Type'anyPointer'implicitMethodParameter
    = Type'anyPointer'implicitMethodParameter' 
        {parameterIndex :: (RP.Parsed Std_.Word16)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Type'anyPointer'implicitMethodParameter))
deriving instance (Std_.Eq (C.Parsed Type'anyPointer'implicitMethodParameter))
instance (C.Parse Type'anyPointer'implicitMethodParameter (C.Parsed Type'anyPointer'implicitMethodParameter)) where
    parse raw_ = (Type'anyPointer'implicitMethodParameter' <$> (GH.parseField #parameterIndex raw_))
instance (C.Marshal Type'anyPointer'implicitMethodParameter (C.Parsed Type'anyPointer'implicitMethodParameter)) where
    marshalInto raw_ Type'anyPointer'implicitMethodParameter'{..} = (do
        (GH.encodeField #parameterIndex parameterIndex raw_)
        (Std_.pure ())
        )
instance (GH.HasField "parameterIndex" GH.Slot Type'anyPointer'implicitMethodParameter Std_.Word16) where
    fieldByLabel  = (GH.dataField 16 1 16 0)
data Brand 
type instance (R.ReprFor Brand) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Brand) where
    typeId  = 10391024731148337707
instance (C.TypedStruct Brand) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Brand) where
    type AllocHint Brand = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Brand (C.Parsed Brand))
instance (C.AllocateList Brand) where
    type ListAllocHint Brand = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Brand (C.Parsed Brand))
data instance C.Parsed Brand
    = Brand 
        {scopes :: (RP.Parsed (R.List Brand'Scope))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Brand))
deriving instance (Std_.Eq (C.Parsed Brand))
instance (C.Parse Brand (C.Parsed Brand)) where
    parse raw_ = (Brand <$> (GH.parseField #scopes raw_))
instance (C.Marshal Brand (C.Parsed Brand)) where
    marshalInto raw_ Brand{..} = (do
        (GH.encodeField #scopes scopes raw_)
        (Std_.pure ())
        )
instance (GH.HasField "scopes" GH.Slot Brand (R.List Brand'Scope)) where
    fieldByLabel  = (GH.ptrField 0)
data Brand'Scope 
type instance (R.ReprFor Brand'Scope) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Brand'Scope) where
    typeId  = 12382423449155627977
instance (C.TypedStruct Brand'Scope) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate Brand'Scope) where
    type AllocHint Brand'Scope = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Brand'Scope (C.Parsed Brand'Scope))
instance (C.AllocateList Brand'Scope) where
    type ListAllocHint Brand'Scope = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Brand'Scope (C.Parsed Brand'Scope))
data instance C.Parsed Brand'Scope
    = Brand'Scope 
        {scopeId :: (RP.Parsed Std_.Word64)
        ,union' :: (C.Parsed (GH.Which Brand'Scope))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Brand'Scope))
deriving instance (Std_.Eq (C.Parsed Brand'Scope))
instance (C.Parse Brand'Scope (C.Parsed Brand'Scope)) where
    parse raw_ = (Brand'Scope <$> (GH.parseField #scopeId raw_)
                              <*> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Brand'Scope (C.Parsed Brand'Scope)) where
    marshalInto raw_ Brand'Scope{..} = (do
        (GH.encodeField #scopeId scopeId raw_)
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Brand'Scope) where
    unionField  = (GH.dataField 0 1 16 0)
    data RawWhich Brand'Scope mut_
        = RW_Brand'Scope'bind (R.Raw (R.List Brand'Binding) mut_)
        | RW_Brand'Scope'inherit (R.Raw () mut_)
        | RW_Brand'Scope'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Brand'Scope'bind <$> (GH.readVariant #bind struct_))
        1 ->
            (RW_Brand'Scope'inherit <$> (GH.readVariant #inherit struct_))
        _ ->
            (Std_.pure (RW_Brand'Scope'unknown' tag_))
    data Which Brand'Scope
instance (GH.HasVariant "bind" GH.Slot Brand'Scope (R.List Brand'Binding)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 0)
instance (GH.HasVariant "inherit" GH.Slot Brand'Scope ()) where
    variantByLabel  = (GH.Variant GH.voidField 1)
data instance C.Parsed (GH.Which Brand'Scope)
    = Brand'Scope'bind (RP.Parsed (R.List Brand'Binding))
    | Brand'Scope'inherit 
    | Brand'Scope'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Brand'Scope)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Brand'Scope)))
instance (C.Parse (GH.Which Brand'Scope) (C.Parsed (GH.Which Brand'Scope))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Brand'Scope'bind rawArg_) ->
                (Brand'Scope'bind <$> (C.parse rawArg_))
            (RW_Brand'Scope'inherit _) ->
                (Std_.pure Brand'Scope'inherit)
            (RW_Brand'Scope'unknown' tag_) ->
                (Std_.pure (Brand'Scope'unknown' tag_))
        )
instance (C.Marshal (GH.Which Brand'Scope) (C.Parsed (GH.Which Brand'Scope))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Brand'Scope'bind arg_) ->
            (GH.encodeVariant #bind arg_ (GH.unionStruct raw_))
        (Brand'Scope'inherit) ->
            (GH.encodeVariant #inherit () (GH.unionStruct raw_))
        (Brand'Scope'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
instance (GH.HasField "scopeId" GH.Slot Brand'Scope Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Brand'Binding 
type instance (R.ReprFor Brand'Binding) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Brand'Binding) where
    typeId  = 14439610327179913212
instance (C.TypedStruct Brand'Binding) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Brand'Binding) where
    type AllocHint Brand'Binding = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Brand'Binding (C.Parsed Brand'Binding))
instance (C.AllocateList Brand'Binding) where
    type ListAllocHint Brand'Binding = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Brand'Binding (C.Parsed Brand'Binding))
data instance C.Parsed Brand'Binding
    = Brand'Binding 
        {union' :: (C.Parsed (GH.Which Brand'Binding))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Brand'Binding))
deriving instance (Std_.Eq (C.Parsed Brand'Binding))
instance (C.Parse Brand'Binding (C.Parsed Brand'Binding)) where
    parse raw_ = (Brand'Binding <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Brand'Binding (C.Parsed Brand'Binding)) where
    marshalInto raw_ Brand'Binding{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Brand'Binding) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich Brand'Binding mut_
        = RW_Brand'Binding'unbound (R.Raw () mut_)
        | RW_Brand'Binding'type_ (R.Raw Type mut_)
        | RW_Brand'Binding'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Brand'Binding'unbound <$> (GH.readVariant #unbound struct_))
        1 ->
            (RW_Brand'Binding'type_ <$> (GH.readVariant #type_ struct_))
        _ ->
            (Std_.pure (RW_Brand'Binding'unknown' tag_))
    data Which Brand'Binding
instance (GH.HasVariant "unbound" GH.Slot Brand'Binding ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance (GH.HasVariant "type_" GH.Slot Brand'Binding Type) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 1)
data instance C.Parsed (GH.Which Brand'Binding)
    = Brand'Binding'unbound 
    | Brand'Binding'type_ (RP.Parsed Type)
    | Brand'Binding'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Brand'Binding)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Brand'Binding)))
instance (C.Parse (GH.Which Brand'Binding) (C.Parsed (GH.Which Brand'Binding))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Brand'Binding'unbound _) ->
                (Std_.pure Brand'Binding'unbound)
            (RW_Brand'Binding'type_ rawArg_) ->
                (Brand'Binding'type_ <$> (C.parse rawArg_))
            (RW_Brand'Binding'unknown' tag_) ->
                (Std_.pure (Brand'Binding'unknown' tag_))
        )
instance (C.Marshal (GH.Which Brand'Binding) (C.Parsed (GH.Which Brand'Binding))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Brand'Binding'unbound) ->
            (GH.encodeVariant #unbound () (GH.unionStruct raw_))
        (Brand'Binding'type_ arg_) ->
            (GH.encodeVariant #type_ arg_ (GH.unionStruct raw_))
        (Brand'Binding'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Value 
type instance (R.ReprFor Value) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Value) where
    typeId  = 14853958794117909659
instance (C.TypedStruct Value) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate Value) where
    type AllocHint Value = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Value (C.Parsed Value))
instance (C.AllocateList Value) where
    type ListAllocHint Value = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Value (C.Parsed Value))
data instance C.Parsed Value
    = Value 
        {union' :: (C.Parsed (GH.Which Value))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Value))
deriving instance (Std_.Eq (C.Parsed Value))
instance (C.Parse Value (C.Parsed Value)) where
    parse raw_ = (Value <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Value (C.Parsed Value)) where
    marshalInto raw_ Value{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Value) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich Value mut_
        = RW_Value'void (R.Raw () mut_)
        | RW_Value'bool (R.Raw Std_.Bool mut_)
        | RW_Value'int8 (R.Raw Std_.Int8 mut_)
        | RW_Value'int16 (R.Raw Std_.Int16 mut_)
        | RW_Value'int32 (R.Raw Std_.Int32 mut_)
        | RW_Value'int64 (R.Raw Std_.Int64 mut_)
        | RW_Value'uint8 (R.Raw Std_.Word8 mut_)
        | RW_Value'uint16 (R.Raw Std_.Word16 mut_)
        | RW_Value'uint32 (R.Raw Std_.Word32 mut_)
        | RW_Value'uint64 (R.Raw Std_.Word64 mut_)
        | RW_Value'float32 (R.Raw Std_.Float mut_)
        | RW_Value'float64 (R.Raw Std_.Double mut_)
        | RW_Value'text (R.Raw Basics.Text mut_)
        | RW_Value'data_ (R.Raw Basics.Data mut_)
        | RW_Value'list (R.Raw (Std_.Maybe Basics.AnyPointer) mut_)
        | RW_Value'enum (R.Raw Std_.Word16 mut_)
        | RW_Value'struct (R.Raw (Std_.Maybe Basics.AnyPointer) mut_)
        | RW_Value'interface (R.Raw () mut_)
        | RW_Value'anyPointer (R.Raw (Std_.Maybe Basics.AnyPointer) mut_)
        | RW_Value'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Value'void <$> (GH.readVariant #void struct_))
        1 ->
            (RW_Value'bool <$> (GH.readVariant #bool struct_))
        2 ->
            (RW_Value'int8 <$> (GH.readVariant #int8 struct_))
        3 ->
            (RW_Value'int16 <$> (GH.readVariant #int16 struct_))
        4 ->
            (RW_Value'int32 <$> (GH.readVariant #int32 struct_))
        5 ->
            (RW_Value'int64 <$> (GH.readVariant #int64 struct_))
        6 ->
            (RW_Value'uint8 <$> (GH.readVariant #uint8 struct_))
        7 ->
            (RW_Value'uint16 <$> (GH.readVariant #uint16 struct_))
        8 ->
            (RW_Value'uint32 <$> (GH.readVariant #uint32 struct_))
        9 ->
            (RW_Value'uint64 <$> (GH.readVariant #uint64 struct_))
        10 ->
            (RW_Value'float32 <$> (GH.readVariant #float32 struct_))
        11 ->
            (RW_Value'float64 <$> (GH.readVariant #float64 struct_))
        12 ->
            (RW_Value'text <$> (GH.readVariant #text struct_))
        13 ->
            (RW_Value'data_ <$> (GH.readVariant #data_ struct_))
        14 ->
            (RW_Value'list <$> (GH.readVariant #list struct_))
        15 ->
            (RW_Value'enum <$> (GH.readVariant #enum struct_))
        16 ->
            (RW_Value'struct <$> (GH.readVariant #struct struct_))
        17 ->
            (RW_Value'interface <$> (GH.readVariant #interface struct_))
        18 ->
            (RW_Value'anyPointer <$> (GH.readVariant #anyPointer struct_))
        _ ->
            (Std_.pure (RW_Value'unknown' tag_))
    data Which Value
instance (GH.HasVariant "void" GH.Slot Value ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance (GH.HasVariant "bool" GH.Slot Value Std_.Bool) where
    variantByLabel  = (GH.Variant (GH.dataField 16 0 1 0) 1)
instance (GH.HasVariant "int8" GH.Slot Value Std_.Int8) where
    variantByLabel  = (GH.Variant (GH.dataField 16 0 8 0) 2)
instance (GH.HasVariant "int16" GH.Slot Value Std_.Int16) where
    variantByLabel  = (GH.Variant (GH.dataField 16 0 16 0) 3)
instance (GH.HasVariant "int32" GH.Slot Value Std_.Int32) where
    variantByLabel  = (GH.Variant (GH.dataField 32 0 32 0) 4)
instance (GH.HasVariant "int64" GH.Slot Value Std_.Int64) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 64 0) 5)
instance (GH.HasVariant "uint8" GH.Slot Value Std_.Word8) where
    variantByLabel  = (GH.Variant (GH.dataField 16 0 8 0) 6)
instance (GH.HasVariant "uint16" GH.Slot Value Std_.Word16) where
    variantByLabel  = (GH.Variant (GH.dataField 16 0 16 0) 7)
instance (GH.HasVariant "uint32" GH.Slot Value Std_.Word32) where
    variantByLabel  = (GH.Variant (GH.dataField 32 0 32 0) 8)
instance (GH.HasVariant "uint64" GH.Slot Value Std_.Word64) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 64 0) 9)
instance (GH.HasVariant "float32" GH.Slot Value Std_.Float) where
    variantByLabel  = (GH.Variant (GH.dataField 32 0 32 0) 10)
instance (GH.HasVariant "float64" GH.Slot Value Std_.Double) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 64 0) 11)
instance (GH.HasVariant "text" GH.Slot Value Basics.Text) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 12)
instance (GH.HasVariant "data_" GH.Slot Value Basics.Data) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 13)
instance (GH.HasVariant "list" GH.Slot Value (Std_.Maybe Basics.AnyPointer)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 14)
instance (GH.HasVariant "enum" GH.Slot Value Std_.Word16) where
    variantByLabel  = (GH.Variant (GH.dataField 16 0 16 0) 15)
instance (GH.HasVariant "struct" GH.Slot Value (Std_.Maybe Basics.AnyPointer)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 16)
instance (GH.HasVariant "interface" GH.Slot Value ()) where
    variantByLabel  = (GH.Variant GH.voidField 17)
instance (GH.HasVariant "anyPointer" GH.Slot Value (Std_.Maybe Basics.AnyPointer)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 18)
data instance C.Parsed (GH.Which Value)
    = Value'void 
    | Value'bool (RP.Parsed Std_.Bool)
    | Value'int8 (RP.Parsed Std_.Int8)
    | Value'int16 (RP.Parsed Std_.Int16)
    | Value'int32 (RP.Parsed Std_.Int32)
    | Value'int64 (RP.Parsed Std_.Int64)
    | Value'uint8 (RP.Parsed Std_.Word8)
    | Value'uint16 (RP.Parsed Std_.Word16)
    | Value'uint32 (RP.Parsed Std_.Word32)
    | Value'uint64 (RP.Parsed Std_.Word64)
    | Value'float32 (RP.Parsed Std_.Float)
    | Value'float64 (RP.Parsed Std_.Double)
    | Value'text (RP.Parsed Basics.Text)
    | Value'data_ (RP.Parsed Basics.Data)
    | Value'list (RP.Parsed (Std_.Maybe Basics.AnyPointer))
    | Value'enum (RP.Parsed Std_.Word16)
    | Value'struct (RP.Parsed (Std_.Maybe Basics.AnyPointer))
    | Value'interface 
    | Value'anyPointer (RP.Parsed (Std_.Maybe Basics.AnyPointer))
    | Value'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Value)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Value)))
instance (C.Parse (GH.Which Value) (C.Parsed (GH.Which Value))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Value'void _) ->
                (Std_.pure Value'void)
            (RW_Value'bool rawArg_) ->
                (Value'bool <$> (C.parse rawArg_))
            (RW_Value'int8 rawArg_) ->
                (Value'int8 <$> (C.parse rawArg_))
            (RW_Value'int16 rawArg_) ->
                (Value'int16 <$> (C.parse rawArg_))
            (RW_Value'int32 rawArg_) ->
                (Value'int32 <$> (C.parse rawArg_))
            (RW_Value'int64 rawArg_) ->
                (Value'int64 <$> (C.parse rawArg_))
            (RW_Value'uint8 rawArg_) ->
                (Value'uint8 <$> (C.parse rawArg_))
            (RW_Value'uint16 rawArg_) ->
                (Value'uint16 <$> (C.parse rawArg_))
            (RW_Value'uint32 rawArg_) ->
                (Value'uint32 <$> (C.parse rawArg_))
            (RW_Value'uint64 rawArg_) ->
                (Value'uint64 <$> (C.parse rawArg_))
            (RW_Value'float32 rawArg_) ->
                (Value'float32 <$> (C.parse rawArg_))
            (RW_Value'float64 rawArg_) ->
                (Value'float64 <$> (C.parse rawArg_))
            (RW_Value'text rawArg_) ->
                (Value'text <$> (C.parse rawArg_))
            (RW_Value'data_ rawArg_) ->
                (Value'data_ <$> (C.parse rawArg_))
            (RW_Value'list rawArg_) ->
                (Value'list <$> (C.parse rawArg_))
            (RW_Value'enum rawArg_) ->
                (Value'enum <$> (C.parse rawArg_))
            (RW_Value'struct rawArg_) ->
                (Value'struct <$> (C.parse rawArg_))
            (RW_Value'interface _) ->
                (Std_.pure Value'interface)
            (RW_Value'anyPointer rawArg_) ->
                (Value'anyPointer <$> (C.parse rawArg_))
            (RW_Value'unknown' tag_) ->
                (Std_.pure (Value'unknown' tag_))
        )
instance (C.Marshal (GH.Which Value) (C.Parsed (GH.Which Value))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Value'void) ->
            (GH.encodeVariant #void () (GH.unionStruct raw_))
        (Value'bool arg_) ->
            (GH.encodeVariant #bool arg_ (GH.unionStruct raw_))
        (Value'int8 arg_) ->
            (GH.encodeVariant #int8 arg_ (GH.unionStruct raw_))
        (Value'int16 arg_) ->
            (GH.encodeVariant #int16 arg_ (GH.unionStruct raw_))
        (Value'int32 arg_) ->
            (GH.encodeVariant #int32 arg_ (GH.unionStruct raw_))
        (Value'int64 arg_) ->
            (GH.encodeVariant #int64 arg_ (GH.unionStruct raw_))
        (Value'uint8 arg_) ->
            (GH.encodeVariant #uint8 arg_ (GH.unionStruct raw_))
        (Value'uint16 arg_) ->
            (GH.encodeVariant #uint16 arg_ (GH.unionStruct raw_))
        (Value'uint32 arg_) ->
            (GH.encodeVariant #uint32 arg_ (GH.unionStruct raw_))
        (Value'uint64 arg_) ->
            (GH.encodeVariant #uint64 arg_ (GH.unionStruct raw_))
        (Value'float32 arg_) ->
            (GH.encodeVariant #float32 arg_ (GH.unionStruct raw_))
        (Value'float64 arg_) ->
            (GH.encodeVariant #float64 arg_ (GH.unionStruct raw_))
        (Value'text arg_) ->
            (GH.encodeVariant #text arg_ (GH.unionStruct raw_))
        (Value'data_ arg_) ->
            (GH.encodeVariant #data_ arg_ (GH.unionStruct raw_))
        (Value'list arg_) ->
            (GH.encodeVariant #list arg_ (GH.unionStruct raw_))
        (Value'enum arg_) ->
            (GH.encodeVariant #enum arg_ (GH.unionStruct raw_))
        (Value'struct arg_) ->
            (GH.encodeVariant #struct arg_ (GH.unionStruct raw_))
        (Value'interface) ->
            (GH.encodeVariant #interface () (GH.unionStruct raw_))
        (Value'anyPointer arg_) ->
            (GH.encodeVariant #anyPointer arg_ (GH.unionStruct raw_))
        (Value'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Annotation 
type instance (R.ReprFor Annotation) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Annotation) where
    typeId  = 17422339044421236034
instance (C.TypedStruct Annotation) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Annotation) where
    type AllocHint Annotation = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Annotation (C.Parsed Annotation))
instance (C.AllocateList Annotation) where
    type ListAllocHint Annotation = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Annotation (C.Parsed Annotation))
data instance C.Parsed Annotation
    = Annotation 
        {id :: (RP.Parsed Std_.Word64)
        ,value :: (RP.Parsed Value)
        ,brand :: (RP.Parsed Brand)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Annotation))
deriving instance (Std_.Eq (C.Parsed Annotation))
instance (C.Parse Annotation (C.Parsed Annotation)) where
    parse raw_ = (Annotation <$> (GH.parseField #id raw_)
                             <*> (GH.parseField #value raw_)
                             <*> (GH.parseField #brand raw_))
instance (C.Marshal Annotation (C.Parsed Annotation)) where
    marshalInto raw_ Annotation{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #value value raw_)
        (GH.encodeField #brand brand raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot Annotation Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "value" GH.Slot Annotation Value) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "brand" GH.Slot Annotation Brand) where
    fieldByLabel  = (GH.ptrField 1)
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
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor ElementSize) = (R.Data R.Sz16)
instance (C.HasTypeId ElementSize) where
    typeId  = 15102134695616452902
instance (Std_.Enum ElementSize) where
    toEnum n_ = case n_ of
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
        tag_ ->
            (ElementSize'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (ElementSize'empty) ->
            0
        (ElementSize'bit) ->
            1
        (ElementSize'byte) ->
            2
        (ElementSize'twoBytes) ->
            3
        (ElementSize'fourBytes) ->
            4
        (ElementSize'eightBytes) ->
            5
        (ElementSize'pointer) ->
            6
        (ElementSize'inlineComposite) ->
            7
        (ElementSize'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord ElementSize) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse ElementSize ElementSize) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList ElementSize) where
    type ListAllocHint ElementSize = Std_.Int
instance (C.EstimateListAlloc ElementSize ElementSize)
data CapnpVersion 
type instance (R.ReprFor CapnpVersion) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CapnpVersion) where
    typeId  = 15590670654532458851
instance (C.TypedStruct CapnpVersion) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate CapnpVersion) where
    type AllocHint CapnpVersion = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CapnpVersion (C.Parsed CapnpVersion))
instance (C.AllocateList CapnpVersion) where
    type ListAllocHint CapnpVersion = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CapnpVersion (C.Parsed CapnpVersion))
data instance C.Parsed CapnpVersion
    = CapnpVersion 
        {major :: (RP.Parsed Std_.Word16)
        ,minor :: (RP.Parsed Std_.Word8)
        ,micro :: (RP.Parsed Std_.Word8)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CapnpVersion))
deriving instance (Std_.Eq (C.Parsed CapnpVersion))
instance (C.Parse CapnpVersion (C.Parsed CapnpVersion)) where
    parse raw_ = (CapnpVersion <$> (GH.parseField #major raw_)
                               <*> (GH.parseField #minor raw_)
                               <*> (GH.parseField #micro raw_))
instance (C.Marshal CapnpVersion (C.Parsed CapnpVersion)) where
    marshalInto raw_ CapnpVersion{..} = (do
        (GH.encodeField #major major raw_)
        (GH.encodeField #minor minor raw_)
        (GH.encodeField #micro micro raw_)
        (Std_.pure ())
        )
instance (GH.HasField "major" GH.Slot CapnpVersion Std_.Word16) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "minor" GH.Slot CapnpVersion Std_.Word8) where
    fieldByLabel  = (GH.dataField 16 0 8 0)
instance (GH.HasField "micro" GH.Slot CapnpVersion Std_.Word8) where
    fieldByLabel  = (GH.dataField 24 0 8 0)
data CodeGeneratorRequest 
type instance (R.ReprFor CodeGeneratorRequest) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CodeGeneratorRequest) where
    typeId  = 13818529054586492878
instance (C.TypedStruct CodeGeneratorRequest) where
    numStructWords  = 0
    numStructPtrs  = 4
instance (C.Allocate CodeGeneratorRequest) where
    type AllocHint CodeGeneratorRequest = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CodeGeneratorRequest (C.Parsed CodeGeneratorRequest))
instance (C.AllocateList CodeGeneratorRequest) where
    type ListAllocHint CodeGeneratorRequest = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CodeGeneratorRequest (C.Parsed CodeGeneratorRequest))
data instance C.Parsed CodeGeneratorRequest
    = CodeGeneratorRequest 
        {nodes :: (RP.Parsed (R.List Node))
        ,requestedFiles :: (RP.Parsed (R.List CodeGeneratorRequest'RequestedFile))
        ,capnpVersion :: (RP.Parsed CapnpVersion)
        ,sourceInfo :: (RP.Parsed (R.List Node'SourceInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CodeGeneratorRequest))
deriving instance (Std_.Eq (C.Parsed CodeGeneratorRequest))
instance (C.Parse CodeGeneratorRequest (C.Parsed CodeGeneratorRequest)) where
    parse raw_ = (CodeGeneratorRequest <$> (GH.parseField #nodes raw_)
                                       <*> (GH.parseField #requestedFiles raw_)
                                       <*> (GH.parseField #capnpVersion raw_)
                                       <*> (GH.parseField #sourceInfo raw_))
instance (C.Marshal CodeGeneratorRequest (C.Parsed CodeGeneratorRequest)) where
    marshalInto raw_ CodeGeneratorRequest{..} = (do
        (GH.encodeField #nodes nodes raw_)
        (GH.encodeField #requestedFiles requestedFiles raw_)
        (GH.encodeField #capnpVersion capnpVersion raw_)
        (GH.encodeField #sourceInfo sourceInfo raw_)
        (Std_.pure ())
        )
instance (GH.HasField "nodes" GH.Slot CodeGeneratorRequest (R.List Node)) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "requestedFiles" GH.Slot CodeGeneratorRequest (R.List CodeGeneratorRequest'RequestedFile)) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "capnpVersion" GH.Slot CodeGeneratorRequest CapnpVersion) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "sourceInfo" GH.Slot CodeGeneratorRequest (R.List Node'SourceInfo)) where
    fieldByLabel  = (GH.ptrField 3)
data CodeGeneratorRequest'RequestedFile 
type instance (R.ReprFor CodeGeneratorRequest'RequestedFile) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CodeGeneratorRequest'RequestedFile) where
    typeId  = 14981803260258615394
instance (C.TypedStruct CodeGeneratorRequest'RequestedFile) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate CodeGeneratorRequest'RequestedFile) where
    type AllocHint CodeGeneratorRequest'RequestedFile = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CodeGeneratorRequest'RequestedFile (C.Parsed CodeGeneratorRequest'RequestedFile))
instance (C.AllocateList CodeGeneratorRequest'RequestedFile) where
    type ListAllocHint CodeGeneratorRequest'RequestedFile = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CodeGeneratorRequest'RequestedFile (C.Parsed CodeGeneratorRequest'RequestedFile))
data instance C.Parsed CodeGeneratorRequest'RequestedFile
    = CodeGeneratorRequest'RequestedFile 
        {id :: (RP.Parsed Std_.Word64)
        ,filename :: (RP.Parsed Basics.Text)
        ,imports :: (RP.Parsed (R.List CodeGeneratorRequest'RequestedFile'Import))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CodeGeneratorRequest'RequestedFile))
deriving instance (Std_.Eq (C.Parsed CodeGeneratorRequest'RequestedFile))
instance (C.Parse CodeGeneratorRequest'RequestedFile (C.Parsed CodeGeneratorRequest'RequestedFile)) where
    parse raw_ = (CodeGeneratorRequest'RequestedFile <$> (GH.parseField #id raw_)
                                                     <*> (GH.parseField #filename raw_)
                                                     <*> (GH.parseField #imports raw_))
instance (C.Marshal CodeGeneratorRequest'RequestedFile (C.Parsed CodeGeneratorRequest'RequestedFile)) where
    marshalInto raw_ CodeGeneratorRequest'RequestedFile{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #filename filename raw_)
        (GH.encodeField #imports imports raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot CodeGeneratorRequest'RequestedFile Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "filename" GH.Slot CodeGeneratorRequest'RequestedFile Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "imports" GH.Slot CodeGeneratorRequest'RequestedFile (R.List CodeGeneratorRequest'RequestedFile'Import)) where
    fieldByLabel  = (GH.ptrField 1)
data CodeGeneratorRequest'RequestedFile'Import 
type instance (R.ReprFor CodeGeneratorRequest'RequestedFile'Import) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CodeGeneratorRequest'RequestedFile'Import) where
    typeId  = 12560611460656617445
instance (C.TypedStruct CodeGeneratorRequest'RequestedFile'Import) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate CodeGeneratorRequest'RequestedFile'Import) where
    type AllocHint CodeGeneratorRequest'RequestedFile'Import = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CodeGeneratorRequest'RequestedFile'Import (C.Parsed CodeGeneratorRequest'RequestedFile'Import))
instance (C.AllocateList CodeGeneratorRequest'RequestedFile'Import) where
    type ListAllocHint CodeGeneratorRequest'RequestedFile'Import = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CodeGeneratorRequest'RequestedFile'Import (C.Parsed CodeGeneratorRequest'RequestedFile'Import))
data instance C.Parsed CodeGeneratorRequest'RequestedFile'Import
    = CodeGeneratorRequest'RequestedFile'Import 
        {id :: (RP.Parsed Std_.Word64)
        ,name :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CodeGeneratorRequest'RequestedFile'Import))
deriving instance (Std_.Eq (C.Parsed CodeGeneratorRequest'RequestedFile'Import))
instance (C.Parse CodeGeneratorRequest'RequestedFile'Import (C.Parsed CodeGeneratorRequest'RequestedFile'Import)) where
    parse raw_ = (CodeGeneratorRequest'RequestedFile'Import <$> (GH.parseField #id raw_)
                                                            <*> (GH.parseField #name raw_))
instance (C.Marshal CodeGeneratorRequest'RequestedFile'Import (C.Parsed CodeGeneratorRequest'RequestedFile'Import)) where
    marshalInto raw_ CodeGeneratorRequest'RequestedFile'Import{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #name name raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot CodeGeneratorRequest'RequestedFile'Import Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot CodeGeneratorRequest'RequestedFile'Import Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)