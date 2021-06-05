{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.Schema.New where
import qualified Capnp.Repr as R
import qualified Capnp.New.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers.New as GH
import qualified Capnp.New.Classes as C
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Node 
type instance (R.ReprFor Node) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node) where
    type AllocHint Node = ()
    new  = GH.newStruct
instance (GH.HasUnion Node) where
    unionField  = (GH.dataField 32 1 16 0)
    data RawWhich mut_ Node
        = RW_Node'file (R.Raw mut_ ())
        | RW_Node'struct (R.Raw mut_ Node'struct)
        | RW_Node'enum (R.Raw mut_ Node'enum)
        | RW_Node'interface (R.Raw mut_ Node'interface)
        | RW_Node'const (R.Raw mut_ Node'const)
        | RW_Node'annotation (R.Raw mut_ Node'annotation)
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
instance (C.TypedStruct Node'struct) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'struct) where
    type AllocHint Node'struct = ()
    new  = GH.newStruct
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
instance (C.TypedStruct Node'enum) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'enum) where
    type AllocHint Node'enum = ()
    new  = GH.newStruct
instance (GH.HasField "enumerants" GH.Slot Node'enum (R.List Enumerant)) where
    fieldByLabel  = (GH.ptrField 3)
data Node'interface 
type instance (R.ReprFor Node'interface) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'interface) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'interface) where
    type AllocHint Node'interface = ()
    new  = GH.newStruct
instance (GH.HasField "methods" GH.Slot Node'interface (R.List Method)) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "superclasses" GH.Slot Node'interface (R.List Superclass)) where
    fieldByLabel  = (GH.ptrField 4)
data Node'const 
type instance (R.ReprFor Node'const) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'const) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'const) where
    type AllocHint Node'const = ()
    new  = GH.newStruct
instance (GH.HasField "type_" GH.Slot Node'const Type) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "value" GH.Slot Node'const Value) where
    fieldByLabel  = (GH.ptrField 4)
data Node'annotation 
type instance (R.ReprFor Node'annotation) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'annotation) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'annotation) where
    type AllocHint Node'annotation = ()
    new  = GH.newStruct
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
instance (C.TypedStruct Node'Parameter) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Node'Parameter) where
    type AllocHint Node'Parameter = ()
    new  = GH.newStruct
instance (GH.HasField "name" GH.Slot Node'Parameter Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Node'NestedNode 
type instance (R.ReprFor Node'NestedNode) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'NestedNode) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Node'NestedNode) where
    type AllocHint Node'NestedNode = ()
    new  = GH.newStruct
instance (GH.HasField "name" GH.Slot Node'NestedNode Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "id" GH.Slot Node'NestedNode Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Node'SourceInfo 
type instance (R.ReprFor Node'SourceInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'SourceInfo) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Node'SourceInfo) where
    type AllocHint Node'SourceInfo = ()
    new  = GH.newStruct
instance (GH.HasField "id" GH.Slot Node'SourceInfo Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "docComment" GH.Slot Node'SourceInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "members" GH.Slot Node'SourceInfo (R.List Node'SourceInfo'Member)) where
    fieldByLabel  = (GH.ptrField 1)
data Node'SourceInfo'Member 
type instance (R.ReprFor Node'SourceInfo'Member) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'SourceInfo'Member) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Node'SourceInfo'Member) where
    type AllocHint Node'SourceInfo'Member = ()
    new  = GH.newStruct
instance (GH.HasField "docComment" GH.Slot Node'SourceInfo'Member Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Field 
type instance (R.ReprFor Field) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Field) where
    numStructWords  = 3
    numStructPtrs  = 4
instance (C.Allocate Field) where
    type AllocHint Field = ()
    new  = GH.newStruct
instance (GH.HasUnion Field) where
    unionField  = (GH.dataField 0 1 16 0)
    data RawWhich mut_ Field
        = RW_Field'slot (R.Raw mut_ Field'slot)
        | RW_Field'group (R.Raw mut_ Field'group)
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
instance (C.TypedStruct Field'slot) where
    numStructWords  = 3
    numStructPtrs  = 4
instance (C.Allocate Field'slot) where
    type AllocHint Field'slot = ()
    new  = GH.newStruct
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
instance (C.TypedStruct Field'group) where
    numStructWords  = 3
    numStructPtrs  = 4
instance (C.Allocate Field'group) where
    type AllocHint Field'group = ()
    new  = GH.newStruct
instance (GH.HasField "typeId" GH.Slot Field'group Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
data Field'ordinal 
type instance (R.ReprFor Field'ordinal) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Field'ordinal) where
    numStructWords  = 3
    numStructPtrs  = 4
instance (C.Allocate Field'ordinal) where
    type AllocHint Field'ordinal = ()
    new  = GH.newStruct
instance (GH.HasUnion Field'ordinal) where
    unionField  = (GH.dataField 16 1 16 0)
    data RawWhich mut_ Field'ordinal
        = RW_Field'ordinal'implicit (R.Raw mut_ ())
        | RW_Field'ordinal'explicit (R.Raw mut_ Std_.Word16)
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
data Enumerant 
type instance (R.ReprFor Enumerant) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Enumerant) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Enumerant) where
    type AllocHint Enumerant = ()
    new  = GH.newStruct
instance (GH.HasField "name" GH.Slot Enumerant Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "codeOrder" GH.Slot Enumerant Std_.Word16) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "annotations" GH.Slot Enumerant (R.List Annotation)) where
    fieldByLabel  = (GH.ptrField 1)
data Superclass 
type instance (R.ReprFor Superclass) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Superclass) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Superclass) where
    type AllocHint Superclass = ()
    new  = GH.newStruct
instance (GH.HasField "id" GH.Slot Superclass Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "brand" GH.Slot Superclass Brand) where
    fieldByLabel  = (GH.ptrField 0)
data Method 
type instance (R.ReprFor Method) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Method) where
    numStructWords  = 3
    numStructPtrs  = 5
instance (C.Allocate Method) where
    type AllocHint Method = ()
    new  = GH.newStruct
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
instance (C.TypedStruct Type) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type) where
    type AllocHint Type = ()
    new  = GH.newStruct
instance (GH.HasUnion Type) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ Type
        = RW_Type'void (R.Raw mut_ ())
        | RW_Type'bool (R.Raw mut_ ())
        | RW_Type'int8 (R.Raw mut_ ())
        | RW_Type'int16 (R.Raw mut_ ())
        | RW_Type'int32 (R.Raw mut_ ())
        | RW_Type'int64 (R.Raw mut_ ())
        | RW_Type'uint8 (R.Raw mut_ ())
        | RW_Type'uint16 (R.Raw mut_ ())
        | RW_Type'uint32 (R.Raw mut_ ())
        | RW_Type'uint64 (R.Raw mut_ ())
        | RW_Type'float32 (R.Raw mut_ ())
        | RW_Type'float64 (R.Raw mut_ ())
        | RW_Type'text (R.Raw mut_ ())
        | RW_Type'data_ (R.Raw mut_ ())
        | RW_Type'list (R.Raw mut_ Type'list)
        | RW_Type'enum (R.Raw mut_ Type'enum)
        | RW_Type'struct (R.Raw mut_ Type'struct)
        | RW_Type'interface (R.Raw mut_ Type'interface)
        | RW_Type'anyPointer (R.Raw mut_ Type'anyPointer)
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
data Type'list 
type instance (R.ReprFor Type'list) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'list) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'list) where
    type AllocHint Type'list = ()
    new  = GH.newStruct
instance (GH.HasField "elementType" GH.Slot Type'list Type) where
    fieldByLabel  = (GH.ptrField 0)
data Type'enum 
type instance (R.ReprFor Type'enum) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'enum) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'enum) where
    type AllocHint Type'enum = ()
    new  = GH.newStruct
instance (GH.HasField "typeId" GH.Slot Type'enum Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "brand" GH.Slot Type'enum Brand) where
    fieldByLabel  = (GH.ptrField 0)
data Type'struct 
type instance (R.ReprFor Type'struct) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'struct) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'struct) where
    type AllocHint Type'struct = ()
    new  = GH.newStruct
instance (GH.HasField "typeId" GH.Slot Type'struct Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "brand" GH.Slot Type'struct Brand) where
    fieldByLabel  = (GH.ptrField 0)
data Type'interface 
type instance (R.ReprFor Type'interface) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'interface) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'interface) where
    type AllocHint Type'interface = ()
    new  = GH.newStruct
instance (GH.HasField "typeId" GH.Slot Type'interface Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "brand" GH.Slot Type'interface Brand) where
    fieldByLabel  = (GH.ptrField 0)
data Type'anyPointer 
type instance (R.ReprFor Type'anyPointer) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'anyPointer) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'anyPointer) where
    type AllocHint Type'anyPointer = ()
    new  = GH.newStruct
instance (GH.HasUnion Type'anyPointer) where
    unionField  = (GH.dataField 0 1 16 0)
    data RawWhich mut_ Type'anyPointer
        = RW_Type'anyPointer'unconstrained (R.Raw mut_ Type'anyPointer'unconstrained)
        | RW_Type'anyPointer'parameter (R.Raw mut_ Type'anyPointer'parameter)
        | RW_Type'anyPointer'implicitMethodParameter (R.Raw mut_ Type'anyPointer'implicitMethodParameter)
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
data Type'anyPointer'unconstrained 
type instance (R.ReprFor Type'anyPointer'unconstrained) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'anyPointer'unconstrained) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'anyPointer'unconstrained) where
    type AllocHint Type'anyPointer'unconstrained = ()
    new  = GH.newStruct
instance (GH.HasUnion Type'anyPointer'unconstrained) where
    unionField  = (GH.dataField 16 1 16 0)
    data RawWhich mut_ Type'anyPointer'unconstrained
        = RW_Type'anyPointer'unconstrained'anyKind (R.Raw mut_ ())
        | RW_Type'anyPointer'unconstrained'struct (R.Raw mut_ ())
        | RW_Type'anyPointer'unconstrained'list (R.Raw mut_ ())
        | RW_Type'anyPointer'unconstrained'capability (R.Raw mut_ ())
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
data Type'anyPointer'parameter 
type instance (R.ReprFor Type'anyPointer'parameter) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'anyPointer'parameter) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'anyPointer'parameter) where
    type AllocHint Type'anyPointer'parameter = ()
    new  = GH.newStruct
instance (GH.HasField "scopeId" GH.Slot Type'anyPointer'parameter Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "parameterIndex" GH.Slot Type'anyPointer'parameter Std_.Word16) where
    fieldByLabel  = (GH.dataField 16 1 16 0)
data Type'anyPointer'implicitMethodParameter 
type instance (R.ReprFor Type'anyPointer'implicitMethodParameter) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'anyPointer'implicitMethodParameter) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'anyPointer'implicitMethodParameter) where
    type AllocHint Type'anyPointer'implicitMethodParameter = ()
    new  = GH.newStruct
instance (GH.HasField "parameterIndex" GH.Slot Type'anyPointer'implicitMethodParameter Std_.Word16) where
    fieldByLabel  = (GH.dataField 16 1 16 0)
data Brand 
type instance (R.ReprFor Brand) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Brand) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Brand) where
    type AllocHint Brand = ()
    new  = GH.newStruct
instance (GH.HasField "scopes" GH.Slot Brand (R.List Brand'Scope)) where
    fieldByLabel  = (GH.ptrField 0)
data Brand'Scope 
type instance (R.ReprFor Brand'Scope) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Brand'Scope) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate Brand'Scope) where
    type AllocHint Brand'Scope = ()
    new  = GH.newStruct
instance (GH.HasUnion Brand'Scope) where
    unionField  = (GH.dataField 0 1 16 0)
    data RawWhich mut_ Brand'Scope
        = RW_Brand'Scope'bind (R.Raw mut_ (R.List Brand'Binding))
        | RW_Brand'Scope'inherit (R.Raw mut_ ())
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
instance (GH.HasField "scopeId" GH.Slot Brand'Scope Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Brand'Binding 
type instance (R.ReprFor Brand'Binding) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Brand'Binding) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Brand'Binding) where
    type AllocHint Brand'Binding = ()
    new  = GH.newStruct
instance (GH.HasUnion Brand'Binding) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ Brand'Binding
        = RW_Brand'Binding'unbound (R.Raw mut_ ())
        | RW_Brand'Binding'type_ (R.Raw mut_ Type)
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
data Value 
type instance (R.ReprFor Value) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Value) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate Value) where
    type AllocHint Value = ()
    new  = GH.newStruct
instance (GH.HasUnion Value) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ Value
        = RW_Value'void (R.Raw mut_ ())
        | RW_Value'bool (R.Raw mut_ Std_.Bool)
        | RW_Value'int8 (R.Raw mut_ Std_.Int8)
        | RW_Value'int16 (R.Raw mut_ Std_.Int16)
        | RW_Value'int32 (R.Raw mut_ Std_.Int32)
        | RW_Value'int64 (R.Raw mut_ Std_.Int64)
        | RW_Value'uint8 (R.Raw mut_ Std_.Word8)
        | RW_Value'uint16 (R.Raw mut_ Std_.Word16)
        | RW_Value'uint32 (R.Raw mut_ Std_.Word32)
        | RW_Value'uint64 (R.Raw mut_ Std_.Word64)
        | RW_Value'float32 (R.Raw mut_ Std_.Float)
        | RW_Value'float64 (R.Raw mut_ Std_.Double)
        | RW_Value'text (R.Raw mut_ Basics.Text)
        | RW_Value'data_ (R.Raw mut_ Basics.Data)
        | RW_Value'list (R.Raw mut_ Basics.AnyPointer)
        | RW_Value'enum (R.Raw mut_ Std_.Word16)
        | RW_Value'struct (R.Raw mut_ Basics.AnyPointer)
        | RW_Value'interface (R.Raw mut_ ())
        | RW_Value'anyPointer (R.Raw mut_ Basics.AnyPointer)
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
instance (GH.HasVariant "list" GH.Slot Value Basics.AnyPointer) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 14)
instance (GH.HasVariant "enum" GH.Slot Value Std_.Word16) where
    variantByLabel  = (GH.Variant (GH.dataField 16 0 16 0) 15)
instance (GH.HasVariant "struct" GH.Slot Value Basics.AnyPointer) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 16)
instance (GH.HasVariant "interface" GH.Slot Value ()) where
    variantByLabel  = (GH.Variant GH.voidField 17)
instance (GH.HasVariant "anyPointer" GH.Slot Value Basics.AnyPointer) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 18)
data Annotation 
type instance (R.ReprFor Annotation) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Annotation) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Annotation) where
    type AllocHint Annotation = ()
    new  = GH.newStruct
instance (GH.HasField "id" GH.Slot Annotation Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "value" GH.Slot Annotation Value) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "brand" GH.Slot Annotation Brand) where
    fieldByLabel  = (GH.ptrField 1)
data ElementSize 
type instance (R.ReprFor ElementSize) = (R.Data R.Sz16)
data CapnpVersion 
type instance (R.ReprFor CapnpVersion) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct CapnpVersion) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate CapnpVersion) where
    type AllocHint CapnpVersion = ()
    new  = GH.newStruct
instance (GH.HasField "major" GH.Slot CapnpVersion Std_.Word16) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "minor" GH.Slot CapnpVersion Std_.Word8) where
    fieldByLabel  = (GH.dataField 16 0 8 0)
instance (GH.HasField "micro" GH.Slot CapnpVersion Std_.Word8) where
    fieldByLabel  = (GH.dataField 24 0 8 0)
data CodeGeneratorRequest 
type instance (R.ReprFor CodeGeneratorRequest) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct CodeGeneratorRequest) where
    numStructWords  = 0
    numStructPtrs  = 4
instance (C.Allocate CodeGeneratorRequest) where
    type AllocHint CodeGeneratorRequest = ()
    new  = GH.newStruct
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
instance (C.TypedStruct CodeGeneratorRequest'RequestedFile) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate CodeGeneratorRequest'RequestedFile) where
    type AllocHint CodeGeneratorRequest'RequestedFile = ()
    new  = GH.newStruct
instance (GH.HasField "id" GH.Slot CodeGeneratorRequest'RequestedFile Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "filename" GH.Slot CodeGeneratorRequest'RequestedFile Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "imports" GH.Slot CodeGeneratorRequest'RequestedFile (R.List CodeGeneratorRequest'RequestedFile'Import)) where
    fieldByLabel  = (GH.ptrField 1)
data CodeGeneratorRequest'RequestedFile'Import 
type instance (R.ReprFor CodeGeneratorRequest'RequestedFile'Import) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct CodeGeneratorRequest'RequestedFile'Import) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate CodeGeneratorRequest'RequestedFile'Import) where
    type AllocHint CodeGeneratorRequest'RequestedFile'Import = ()
    new  = GH.newStruct
instance (GH.HasField "id" GH.Slot CodeGeneratorRequest'RequestedFile'Import Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot CodeGeneratorRequest'RequestedFile'Import Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)