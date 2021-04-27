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
import qualified Capnp.Fields as F
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
instance (F.HasUnion Node) where
    unionField  = (GH.dataField 2 1 16 0)
    data RawWhich mut_ Node
        = Node'file (R.Raw mut_ ())
        | Node'struct (R.Raw mut_ Node'struct)
        | Node'enum (R.Raw mut_ Node'enum)
        | Node'interface (R.Raw mut_ Node'interface)
        | Node'const (R.Raw mut_ Node'const)
        | Node'annotation (R.Raw mut_ Node'annotation)
        | Node'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Node'file <$> (GH.readVariant #file struct_))
        1 ->
            (Node'struct <$> (GH.readVariant #struct struct_))
        2 ->
            (Node'enum <$> (GH.readVariant #enum struct_))
        3 ->
            (Node'interface <$> (GH.readVariant #interface struct_))
        4 ->
            (Node'const <$> (GH.readVariant #const struct_))
        5 ->
            (Node'annotation <$> (GH.readVariant #annotation struct_))
        _ ->
            (Std_.pure (Node'unknown' tag_))
instance (F.HasVariant "file" F.Slot Node ()) where
    theVariant  = (F.Variant GH.voidField 0)
instance (F.HasVariant "struct" F.Group Node Node'struct) where
    theVariant  = (F.Variant GH.groupField 1)
instance (F.HasVariant "enum" F.Group Node Node'enum) where
    theVariant  = (F.Variant GH.groupField 2)
instance (F.HasVariant "interface" F.Group Node Node'interface) where
    theVariant  = (F.Variant GH.groupField 3)
instance (F.HasVariant "const" F.Group Node Node'const) where
    theVariant  = (F.Variant GH.groupField 4)
instance (F.HasVariant "annotation" F.Group Node Node'annotation) where
    theVariant  = (F.Variant GH.groupField 5)
instance (F.HasField "id" F.Slot Node Std_.Word64) where
    theField  = (GH.dataField 0 0 64 0)
instance (F.HasField "displayName" F.Slot Node Basics.Text) where
    theField  = (GH.ptrField 0)
instance (F.HasField "displayNamePrefixLength" F.Slot Node Std_.Word32) where
    theField  = (GH.dataField 0 1 32 0)
instance (F.HasField "scopeId" F.Slot Node Std_.Word64) where
    theField  = (GH.dataField 0 2 64 0)
instance (F.HasField "nestedNodes" F.Slot Node (R.List Node'NestedNode)) where
    theField  = (GH.ptrField 1)
instance (F.HasField "annotations" F.Slot Node (R.List Annotation)) where
    theField  = (GH.ptrField 2)
instance (F.HasField "parameters" F.Slot Node (R.List Node'Parameter)) where
    theField  = (GH.ptrField 5)
instance (F.HasField "isGeneric" F.Slot Node Std_.Bool) where
    theField  = (GH.dataField 32 4 1 0)
data Node'struct 
type instance (R.ReprFor Node'struct) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'struct) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'struct) where
    type AllocHint Node'struct = ()
    new  = GH.newStruct
instance (F.HasField "dataWordCount" F.Slot Node'struct Std_.Word16) where
    theField  = (GH.dataField 48 1 16 0)
instance (F.HasField "pointerCount" F.Slot Node'struct Std_.Word16) where
    theField  = (GH.dataField 0 3 16 0)
instance (F.HasField "preferredListEncoding" F.Slot Node'struct ElementSize) where
    theField  = (GH.dataField 16 3 16 0)
instance (F.HasField "isGroup" F.Slot Node'struct Std_.Bool) where
    theField  = (GH.dataField 32 3 1 0)
instance (F.HasField "discriminantCount" F.Slot Node'struct Std_.Word16) where
    theField  = (GH.dataField 48 3 16 0)
instance (F.HasField "discriminantOffset" F.Slot Node'struct Std_.Word32) where
    theField  = (GH.dataField 0 4 32 0)
instance (F.HasField "fields" F.Slot Node'struct (R.List Field)) where
    theField  = (GH.ptrField 3)
data Node'enum 
type instance (R.ReprFor Node'enum) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'enum) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'enum) where
    type AllocHint Node'enum = ()
    new  = GH.newStruct
instance (F.HasField "enumerants" F.Slot Node'enum (R.List Enumerant)) where
    theField  = (GH.ptrField 3)
data Node'interface 
type instance (R.ReprFor Node'interface) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'interface) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'interface) where
    type AllocHint Node'interface = ()
    new  = GH.newStruct
instance (F.HasField "methods" F.Slot Node'interface (R.List Method)) where
    theField  = (GH.ptrField 3)
instance (F.HasField "superclasses" F.Slot Node'interface (R.List Superclass)) where
    theField  = (GH.ptrField 4)
data Node'const 
type instance (R.ReprFor Node'const) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'const) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'const) where
    type AllocHint Node'const = ()
    new  = GH.newStruct
instance (F.HasField "type_" F.Slot Node'const Type) where
    theField  = (GH.ptrField 3)
instance (F.HasField "value" F.Slot Node'const Value) where
    theField  = (GH.ptrField 4)
data Node'annotation 
type instance (R.ReprFor Node'annotation) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'annotation) where
    numStructWords  = 5
    numStructPtrs  = 6
instance (C.Allocate Node'annotation) where
    type AllocHint Node'annotation = ()
    new  = GH.newStruct
instance (F.HasField "type_" F.Slot Node'annotation Type) where
    theField  = (GH.ptrField 3)
instance (F.HasField "targetsFile" F.Slot Node'annotation Std_.Bool) where
    theField  = (GH.dataField 48 1 1 0)
instance (F.HasField "targetsConst" F.Slot Node'annotation Std_.Bool) where
    theField  = (GH.dataField 49 1 1 0)
instance (F.HasField "targetsEnum" F.Slot Node'annotation Std_.Bool) where
    theField  = (GH.dataField 50 1 1 0)
instance (F.HasField "targetsEnumerant" F.Slot Node'annotation Std_.Bool) where
    theField  = (GH.dataField 51 1 1 0)
instance (F.HasField "targetsStruct" F.Slot Node'annotation Std_.Bool) where
    theField  = (GH.dataField 52 1 1 0)
instance (F.HasField "targetsField" F.Slot Node'annotation Std_.Bool) where
    theField  = (GH.dataField 53 1 1 0)
instance (F.HasField "targetsUnion" F.Slot Node'annotation Std_.Bool) where
    theField  = (GH.dataField 54 1 1 0)
instance (F.HasField "targetsGroup" F.Slot Node'annotation Std_.Bool) where
    theField  = (GH.dataField 55 1 1 0)
instance (F.HasField "targetsInterface" F.Slot Node'annotation Std_.Bool) where
    theField  = (GH.dataField 56 1 1 0)
instance (F.HasField "targetsMethod" F.Slot Node'annotation Std_.Bool) where
    theField  = (GH.dataField 57 1 1 0)
instance (F.HasField "targetsParam" F.Slot Node'annotation Std_.Bool) where
    theField  = (GH.dataField 58 1 1 0)
instance (F.HasField "targetsAnnotation" F.Slot Node'annotation Std_.Bool) where
    theField  = (GH.dataField 59 1 1 0)
data Node'Parameter 
type instance (R.ReprFor Node'Parameter) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'Parameter) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Node'Parameter) where
    type AllocHint Node'Parameter = ()
    new  = GH.newStruct
instance (F.HasField "name" F.Slot Node'Parameter Basics.Text) where
    theField  = (GH.ptrField 0)
data Node'NestedNode 
type instance (R.ReprFor Node'NestedNode) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'NestedNode) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Node'NestedNode) where
    type AllocHint Node'NestedNode = ()
    new  = GH.newStruct
instance (F.HasField "name" F.Slot Node'NestedNode Basics.Text) where
    theField  = (GH.ptrField 0)
instance (F.HasField "id" F.Slot Node'NestedNode Std_.Word64) where
    theField  = (GH.dataField 0 0 64 0)
data Node'SourceInfo 
type instance (R.ReprFor Node'SourceInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'SourceInfo) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Node'SourceInfo) where
    type AllocHint Node'SourceInfo = ()
    new  = GH.newStruct
instance (F.HasField "id" F.Slot Node'SourceInfo Std_.Word64) where
    theField  = (GH.dataField 0 0 64 0)
instance (F.HasField "docComment" F.Slot Node'SourceInfo Basics.Text) where
    theField  = (GH.ptrField 0)
instance (F.HasField "members" F.Slot Node'SourceInfo (R.List Node'SourceInfo'Member)) where
    theField  = (GH.ptrField 1)
data Node'SourceInfo'Member 
type instance (R.ReprFor Node'SourceInfo'Member) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Node'SourceInfo'Member) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Node'SourceInfo'Member) where
    type AllocHint Node'SourceInfo'Member = ()
    new  = GH.newStruct
instance (F.HasField "docComment" F.Slot Node'SourceInfo'Member Basics.Text) where
    theField  = (GH.ptrField 0)
data Field 
type instance (R.ReprFor Field) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Field) where
    numStructWords  = 3
    numStructPtrs  = 4
instance (C.Allocate Field) where
    type AllocHint Field = ()
    new  = GH.newStruct
instance (F.HasUnion Field) where
    unionField  = (GH.dataField 0 1 16 0)
    data RawWhich mut_ Field
        = Field'slot (R.Raw mut_ Field'slot)
        | Field'group (R.Raw mut_ Field'group)
        | Field'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Field'slot <$> (GH.readVariant #slot struct_))
        1 ->
            (Field'group <$> (GH.readVariant #group struct_))
        _ ->
            (Std_.pure (Field'unknown' tag_))
instance (F.HasVariant "slot" F.Group Field Field'slot) where
    theVariant  = (F.Variant GH.groupField 0)
instance (F.HasVariant "group" F.Group Field Field'group) where
    theVariant  = (F.Variant GH.groupField 1)
instance (F.HasField "name" F.Slot Field Basics.Text) where
    theField  = (GH.ptrField 0)
instance (F.HasField "codeOrder" F.Slot Field Std_.Word16) where
    theField  = (GH.dataField 0 0 16 0)
instance (F.HasField "annotations" F.Slot Field (R.List Annotation)) where
    theField  = (GH.ptrField 1)
instance (F.HasField "discriminantValue" F.Slot Field Std_.Word16) where
    theField  = (GH.dataField 16 0 16 65535)
instance (F.HasField "ordinal" F.Group Field Field'ordinal) where
    theField  = GH.groupField
data Field'slot 
type instance (R.ReprFor Field'slot) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Field'slot) where
    numStructWords  = 3
    numStructPtrs  = 4
instance (C.Allocate Field'slot) where
    type AllocHint Field'slot = ()
    new  = GH.newStruct
instance (F.HasField "offset" F.Slot Field'slot Std_.Word32) where
    theField  = (GH.dataField 32 0 32 0)
instance (F.HasField "type_" F.Slot Field'slot Type) where
    theField  = (GH.ptrField 2)
instance (F.HasField "defaultValue" F.Slot Field'slot Value) where
    theField  = (GH.ptrField 3)
instance (F.HasField "hadExplicitDefault" F.Slot Field'slot Std_.Bool) where
    theField  = (GH.dataField 0 2 1 0)
data Field'group 
type instance (R.ReprFor Field'group) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Field'group) where
    numStructWords  = 3
    numStructPtrs  = 4
instance (C.Allocate Field'group) where
    type AllocHint Field'group = ()
    new  = GH.newStruct
instance (F.HasField "typeId" F.Slot Field'group Std_.Word64) where
    theField  = (GH.dataField 0 2 64 0)
data Field'ordinal 
type instance (R.ReprFor Field'ordinal) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Field'ordinal) where
    numStructWords  = 3
    numStructPtrs  = 4
instance (C.Allocate Field'ordinal) where
    type AllocHint Field'ordinal = ()
    new  = GH.newStruct
instance (F.HasUnion Field'ordinal) where
    unionField  = (GH.dataField 1 1 16 0)
    data RawWhich mut_ Field'ordinal
        = Field'ordinal'implicit (R.Raw mut_ ())
        | Field'ordinal'explicit (R.Raw mut_ Std_.Word16)
        | Field'ordinal'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Field'ordinal'implicit <$> (GH.readVariant #implicit struct_))
        1 ->
            (Field'ordinal'explicit <$> (GH.readVariant #explicit struct_))
        _ ->
            (Std_.pure (Field'ordinal'unknown' tag_))
instance (F.HasVariant "implicit" F.Slot Field'ordinal ()) where
    theVariant  = (F.Variant GH.voidField 0)
instance (F.HasVariant "explicit" F.Slot Field'ordinal Std_.Word16) where
    theVariant  = (F.Variant (GH.dataField 32 1 16 0) 1)
data Enumerant 
type instance (R.ReprFor Enumerant) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Enumerant) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Enumerant) where
    type AllocHint Enumerant = ()
    new  = GH.newStruct
instance (F.HasField "name" F.Slot Enumerant Basics.Text) where
    theField  = (GH.ptrField 0)
instance (F.HasField "codeOrder" F.Slot Enumerant Std_.Word16) where
    theField  = (GH.dataField 0 0 16 0)
instance (F.HasField "annotations" F.Slot Enumerant (R.List Annotation)) where
    theField  = (GH.ptrField 1)
data Superclass 
type instance (R.ReprFor Superclass) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Superclass) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Superclass) where
    type AllocHint Superclass = ()
    new  = GH.newStruct
instance (F.HasField "id" F.Slot Superclass Std_.Word64) where
    theField  = (GH.dataField 0 0 64 0)
instance (F.HasField "brand" F.Slot Superclass Brand) where
    theField  = (GH.ptrField 0)
data Method 
type instance (R.ReprFor Method) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Method) where
    numStructWords  = 3
    numStructPtrs  = 5
instance (C.Allocate Method) where
    type AllocHint Method = ()
    new  = GH.newStruct
instance (F.HasField "name" F.Slot Method Basics.Text) where
    theField  = (GH.ptrField 0)
instance (F.HasField "codeOrder" F.Slot Method Std_.Word16) where
    theField  = (GH.dataField 0 0 16 0)
instance (F.HasField "paramStructType" F.Slot Method Std_.Word64) where
    theField  = (GH.dataField 0 1 64 0)
instance (F.HasField "resultStructType" F.Slot Method Std_.Word64) where
    theField  = (GH.dataField 0 2 64 0)
instance (F.HasField "annotations" F.Slot Method (R.List Annotation)) where
    theField  = (GH.ptrField 1)
instance (F.HasField "paramBrand" F.Slot Method Brand) where
    theField  = (GH.ptrField 2)
instance (F.HasField "resultBrand" F.Slot Method Brand) where
    theField  = (GH.ptrField 3)
instance (F.HasField "implicitParameters" F.Slot Method (R.List Node'Parameter)) where
    theField  = (GH.ptrField 4)
data Type 
type instance (R.ReprFor Type) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type) where
    type AllocHint Type = ()
    new  = GH.newStruct
instance (F.HasUnion Type) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ Type
        = Type'void (R.Raw mut_ ())
        | Type'bool (R.Raw mut_ ())
        | Type'int8 (R.Raw mut_ ())
        | Type'int16 (R.Raw mut_ ())
        | Type'int32 (R.Raw mut_ ())
        | Type'int64 (R.Raw mut_ ())
        | Type'uint8 (R.Raw mut_ ())
        | Type'uint16 (R.Raw mut_ ())
        | Type'uint32 (R.Raw mut_ ())
        | Type'uint64 (R.Raw mut_ ())
        | Type'float32 (R.Raw mut_ ())
        | Type'float64 (R.Raw mut_ ())
        | Type'text (R.Raw mut_ ())
        | Type'data_ (R.Raw mut_ ())
        | Type'list (R.Raw mut_ Type'list)
        | Type'enum (R.Raw mut_ Type'enum)
        | Type'struct (R.Raw mut_ Type'struct)
        | Type'interface (R.Raw mut_ Type'interface)
        | Type'anyPointer (R.Raw mut_ Type'anyPointer)
        | Type'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Type'void <$> (GH.readVariant #void struct_))
        1 ->
            (Type'bool <$> (GH.readVariant #bool struct_))
        2 ->
            (Type'int8 <$> (GH.readVariant #int8 struct_))
        3 ->
            (Type'int16 <$> (GH.readVariant #int16 struct_))
        4 ->
            (Type'int32 <$> (GH.readVariant #int32 struct_))
        5 ->
            (Type'int64 <$> (GH.readVariant #int64 struct_))
        6 ->
            (Type'uint8 <$> (GH.readVariant #uint8 struct_))
        7 ->
            (Type'uint16 <$> (GH.readVariant #uint16 struct_))
        8 ->
            (Type'uint32 <$> (GH.readVariant #uint32 struct_))
        9 ->
            (Type'uint64 <$> (GH.readVariant #uint64 struct_))
        10 ->
            (Type'float32 <$> (GH.readVariant #float32 struct_))
        11 ->
            (Type'float64 <$> (GH.readVariant #float64 struct_))
        12 ->
            (Type'text <$> (GH.readVariant #text struct_))
        13 ->
            (Type'data_ <$> (GH.readVariant #data_ struct_))
        14 ->
            (Type'list <$> (GH.readVariant #list struct_))
        15 ->
            (Type'enum <$> (GH.readVariant #enum struct_))
        16 ->
            (Type'struct <$> (GH.readVariant #struct struct_))
        17 ->
            (Type'interface <$> (GH.readVariant #interface struct_))
        18 ->
            (Type'anyPointer <$> (GH.readVariant #anyPointer struct_))
        _ ->
            (Std_.pure (Type'unknown' tag_))
instance (F.HasVariant "void" F.Slot Type ()) where
    theVariant  = (F.Variant GH.voidField 0)
instance (F.HasVariant "bool" F.Slot Type ()) where
    theVariant  = (F.Variant GH.voidField 1)
instance (F.HasVariant "int8" F.Slot Type ()) where
    theVariant  = (F.Variant GH.voidField 2)
instance (F.HasVariant "int16" F.Slot Type ()) where
    theVariant  = (F.Variant GH.voidField 3)
instance (F.HasVariant "int32" F.Slot Type ()) where
    theVariant  = (F.Variant GH.voidField 4)
instance (F.HasVariant "int64" F.Slot Type ()) where
    theVariant  = (F.Variant GH.voidField 5)
instance (F.HasVariant "uint8" F.Slot Type ()) where
    theVariant  = (F.Variant GH.voidField 6)
instance (F.HasVariant "uint16" F.Slot Type ()) where
    theVariant  = (F.Variant GH.voidField 7)
instance (F.HasVariant "uint32" F.Slot Type ()) where
    theVariant  = (F.Variant GH.voidField 8)
instance (F.HasVariant "uint64" F.Slot Type ()) where
    theVariant  = (F.Variant GH.voidField 9)
instance (F.HasVariant "float32" F.Slot Type ()) where
    theVariant  = (F.Variant GH.voidField 10)
instance (F.HasVariant "float64" F.Slot Type ()) where
    theVariant  = (F.Variant GH.voidField 11)
instance (F.HasVariant "text" F.Slot Type ()) where
    theVariant  = (F.Variant GH.voidField 12)
instance (F.HasVariant "data_" F.Slot Type ()) where
    theVariant  = (F.Variant GH.voidField 13)
instance (F.HasVariant "list" F.Group Type Type'list) where
    theVariant  = (F.Variant GH.groupField 14)
instance (F.HasVariant "enum" F.Group Type Type'enum) where
    theVariant  = (F.Variant GH.groupField 15)
instance (F.HasVariant "struct" F.Group Type Type'struct) where
    theVariant  = (F.Variant GH.groupField 16)
instance (F.HasVariant "interface" F.Group Type Type'interface) where
    theVariant  = (F.Variant GH.groupField 17)
instance (F.HasVariant "anyPointer" F.Group Type Type'anyPointer) where
    theVariant  = (F.Variant GH.groupField 18)
data Type'list 
type instance (R.ReprFor Type'list) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'list) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'list) where
    type AllocHint Type'list = ()
    new  = GH.newStruct
instance (F.HasField "elementType" F.Slot Type'list Type) where
    theField  = (GH.ptrField 0)
data Type'enum 
type instance (R.ReprFor Type'enum) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'enum) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'enum) where
    type AllocHint Type'enum = ()
    new  = GH.newStruct
instance (F.HasField "typeId" F.Slot Type'enum Std_.Word64) where
    theField  = (GH.dataField 0 1 64 0)
instance (F.HasField "brand" F.Slot Type'enum Brand) where
    theField  = (GH.ptrField 0)
data Type'struct 
type instance (R.ReprFor Type'struct) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'struct) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'struct) where
    type AllocHint Type'struct = ()
    new  = GH.newStruct
instance (F.HasField "typeId" F.Slot Type'struct Std_.Word64) where
    theField  = (GH.dataField 0 1 64 0)
instance (F.HasField "brand" F.Slot Type'struct Brand) where
    theField  = (GH.ptrField 0)
data Type'interface 
type instance (R.ReprFor Type'interface) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'interface) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'interface) where
    type AllocHint Type'interface = ()
    new  = GH.newStruct
instance (F.HasField "typeId" F.Slot Type'interface Std_.Word64) where
    theField  = (GH.dataField 0 1 64 0)
instance (F.HasField "brand" F.Slot Type'interface Brand) where
    theField  = (GH.ptrField 0)
data Type'anyPointer 
type instance (R.ReprFor Type'anyPointer) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'anyPointer) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'anyPointer) where
    type AllocHint Type'anyPointer = ()
    new  = GH.newStruct
instance (F.HasUnion Type'anyPointer) where
    unionField  = (GH.dataField 0 1 16 0)
    data RawWhich mut_ Type'anyPointer
        = Type'anyPointer'unconstrained (R.Raw mut_ Type'anyPointer'unconstrained)
        | Type'anyPointer'parameter (R.Raw mut_ Type'anyPointer'parameter)
        | Type'anyPointer'implicitMethodParameter (R.Raw mut_ Type'anyPointer'implicitMethodParameter)
        | Type'anyPointer'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Type'anyPointer'unconstrained <$> (GH.readVariant #unconstrained struct_))
        1 ->
            (Type'anyPointer'parameter <$> (GH.readVariant #parameter struct_))
        2 ->
            (Type'anyPointer'implicitMethodParameter <$> (GH.readVariant #implicitMethodParameter struct_))
        _ ->
            (Std_.pure (Type'anyPointer'unknown' tag_))
instance (F.HasVariant "unconstrained" F.Group Type'anyPointer Type'anyPointer'unconstrained) where
    theVariant  = (F.Variant GH.groupField 0)
instance (F.HasVariant "parameter" F.Group Type'anyPointer Type'anyPointer'parameter) where
    theVariant  = (F.Variant GH.groupField 1)
instance (F.HasVariant "implicitMethodParameter" F.Group Type'anyPointer Type'anyPointer'implicitMethodParameter) where
    theVariant  = (F.Variant GH.groupField 2)
data Type'anyPointer'unconstrained 
type instance (R.ReprFor Type'anyPointer'unconstrained) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'anyPointer'unconstrained) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'anyPointer'unconstrained) where
    type AllocHint Type'anyPointer'unconstrained = ()
    new  = GH.newStruct
instance (F.HasUnion Type'anyPointer'unconstrained) where
    unionField  = (GH.dataField 1 1 16 0)
    data RawWhich mut_ Type'anyPointer'unconstrained
        = Type'anyPointer'unconstrained'anyKind (R.Raw mut_ ())
        | Type'anyPointer'unconstrained'struct (R.Raw mut_ ())
        | Type'anyPointer'unconstrained'list (R.Raw mut_ ())
        | Type'anyPointer'unconstrained'capability (R.Raw mut_ ())
        | Type'anyPointer'unconstrained'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Type'anyPointer'unconstrained'anyKind <$> (GH.readVariant #anyKind struct_))
        1 ->
            (Type'anyPointer'unconstrained'struct <$> (GH.readVariant #struct struct_))
        2 ->
            (Type'anyPointer'unconstrained'list <$> (GH.readVariant #list struct_))
        3 ->
            (Type'anyPointer'unconstrained'capability <$> (GH.readVariant #capability struct_))
        _ ->
            (Std_.pure (Type'anyPointer'unconstrained'unknown' tag_))
instance (F.HasVariant "anyKind" F.Slot Type'anyPointer'unconstrained ()) where
    theVariant  = (F.Variant GH.voidField 0)
instance (F.HasVariant "struct" F.Slot Type'anyPointer'unconstrained ()) where
    theVariant  = (F.Variant GH.voidField 1)
instance (F.HasVariant "list" F.Slot Type'anyPointer'unconstrained ()) where
    theVariant  = (F.Variant GH.voidField 2)
instance (F.HasVariant "capability" F.Slot Type'anyPointer'unconstrained ()) where
    theVariant  = (F.Variant GH.voidField 3)
data Type'anyPointer'parameter 
type instance (R.ReprFor Type'anyPointer'parameter) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'anyPointer'parameter) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'anyPointer'parameter) where
    type AllocHint Type'anyPointer'parameter = ()
    new  = GH.newStruct
instance (F.HasField "scopeId" F.Slot Type'anyPointer'parameter Std_.Word64) where
    theField  = (GH.dataField 0 2 64 0)
instance (F.HasField "parameterIndex" F.Slot Type'anyPointer'parameter Std_.Word16) where
    theField  = (GH.dataField 16 1 16 0)
data Type'anyPointer'implicitMethodParameter 
type instance (R.ReprFor Type'anyPointer'implicitMethodParameter) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Type'anyPointer'implicitMethodParameter) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Type'anyPointer'implicitMethodParameter) where
    type AllocHint Type'anyPointer'implicitMethodParameter = ()
    new  = GH.newStruct
instance (F.HasField "parameterIndex" F.Slot Type'anyPointer'implicitMethodParameter Std_.Word16) where
    theField  = (GH.dataField 16 1 16 0)
data Brand 
type instance (R.ReprFor Brand) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Brand) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Brand) where
    type AllocHint Brand = ()
    new  = GH.newStruct
instance (F.HasField "scopes" F.Slot Brand (R.List Brand'Scope)) where
    theField  = (GH.ptrField 0)
data Brand'Scope 
type instance (R.ReprFor Brand'Scope) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Brand'Scope) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate Brand'Scope) where
    type AllocHint Brand'Scope = ()
    new  = GH.newStruct
instance (F.HasUnion Brand'Scope) where
    unionField  = (GH.dataField 0 1 16 0)
    data RawWhich mut_ Brand'Scope
        = Brand'Scope'bind (R.Raw mut_ (R.List Brand'Binding))
        | Brand'Scope'inherit (R.Raw mut_ ())
        | Brand'Scope'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Brand'Scope'bind <$> (GH.readVariant #bind struct_))
        1 ->
            (Brand'Scope'inherit <$> (GH.readVariant #inherit struct_))
        _ ->
            (Std_.pure (Brand'Scope'unknown' tag_))
instance (F.HasVariant "bind" F.Slot Brand'Scope (R.List Brand'Binding)) where
    theVariant  = (F.Variant (GH.ptrField 0) 0)
instance (F.HasVariant "inherit" F.Slot Brand'Scope ()) where
    theVariant  = (F.Variant GH.voidField 1)
instance (F.HasField "scopeId" F.Slot Brand'Scope Std_.Word64) where
    theField  = (GH.dataField 0 0 64 0)
data Brand'Binding 
type instance (R.ReprFor Brand'Binding) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Brand'Binding) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Brand'Binding) where
    type AllocHint Brand'Binding = ()
    new  = GH.newStruct
instance (F.HasUnion Brand'Binding) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ Brand'Binding
        = Brand'Binding'unbound (R.Raw mut_ ())
        | Brand'Binding'type_ (R.Raw mut_ Type)
        | Brand'Binding'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Brand'Binding'unbound <$> (GH.readVariant #unbound struct_))
        1 ->
            (Brand'Binding'type_ <$> (GH.readVariant #type_ struct_))
        _ ->
            (Std_.pure (Brand'Binding'unknown' tag_))
instance (F.HasVariant "unbound" F.Slot Brand'Binding ()) where
    theVariant  = (F.Variant GH.voidField 0)
instance (F.HasVariant "type_" F.Slot Brand'Binding Type) where
    theVariant  = (F.Variant (GH.ptrField 0) 1)
data Value 
type instance (R.ReprFor Value) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Value) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate Value) where
    type AllocHint Value = ()
    new  = GH.newStruct
instance (F.HasUnion Value) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ Value
        = Value'void (R.Raw mut_ ())
        | Value'bool (R.Raw mut_ Std_.Bool)
        | Value'int8 (R.Raw mut_ Std_.Int8)
        | Value'int16 (R.Raw mut_ Std_.Int16)
        | Value'int32 (R.Raw mut_ Std_.Int32)
        | Value'int64 (R.Raw mut_ Std_.Int64)
        | Value'uint8 (R.Raw mut_ Std_.Word8)
        | Value'uint16 (R.Raw mut_ Std_.Word16)
        | Value'uint32 (R.Raw mut_ Std_.Word32)
        | Value'uint64 (R.Raw mut_ Std_.Word64)
        | Value'float32 (R.Raw mut_ Std_.Float)
        | Value'float64 (R.Raw mut_ Std_.Double)
        | Value'text (R.Raw mut_ Basics.Text)
        | Value'data_ (R.Raw mut_ Basics.Data)
        | Value'list (R.Raw mut_ Basics.AnyPointer)
        | Value'enum (R.Raw mut_ Std_.Word16)
        | Value'struct (R.Raw mut_ Basics.AnyPointer)
        | Value'interface (R.Raw mut_ ())
        | Value'anyPointer (R.Raw mut_ Basics.AnyPointer)
        | Value'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Value'void <$> (GH.readVariant #void struct_))
        1 ->
            (Value'bool <$> (GH.readVariant #bool struct_))
        2 ->
            (Value'int8 <$> (GH.readVariant #int8 struct_))
        3 ->
            (Value'int16 <$> (GH.readVariant #int16 struct_))
        4 ->
            (Value'int32 <$> (GH.readVariant #int32 struct_))
        5 ->
            (Value'int64 <$> (GH.readVariant #int64 struct_))
        6 ->
            (Value'uint8 <$> (GH.readVariant #uint8 struct_))
        7 ->
            (Value'uint16 <$> (GH.readVariant #uint16 struct_))
        8 ->
            (Value'uint32 <$> (GH.readVariant #uint32 struct_))
        9 ->
            (Value'uint64 <$> (GH.readVariant #uint64 struct_))
        10 ->
            (Value'float32 <$> (GH.readVariant #float32 struct_))
        11 ->
            (Value'float64 <$> (GH.readVariant #float64 struct_))
        12 ->
            (Value'text <$> (GH.readVariant #text struct_))
        13 ->
            (Value'data_ <$> (GH.readVariant #data_ struct_))
        14 ->
            (Value'list <$> (GH.readVariant #list struct_))
        15 ->
            (Value'enum <$> (GH.readVariant #enum struct_))
        16 ->
            (Value'struct <$> (GH.readVariant #struct struct_))
        17 ->
            (Value'interface <$> (GH.readVariant #interface struct_))
        18 ->
            (Value'anyPointer <$> (GH.readVariant #anyPointer struct_))
        _ ->
            (Std_.pure (Value'unknown' tag_))
instance (F.HasVariant "void" F.Slot Value ()) where
    theVariant  = (F.Variant GH.voidField 0)
instance (F.HasVariant "bool" F.Slot Value Std_.Bool) where
    theVariant  = (F.Variant (GH.dataField 16 0 1 0) 1)
instance (F.HasVariant "int8" F.Slot Value Std_.Int8) where
    theVariant  = (F.Variant (GH.dataField 16 0 8 0) 2)
instance (F.HasVariant "int16" F.Slot Value Std_.Int16) where
    theVariant  = (F.Variant (GH.dataField 16 0 16 0) 3)
instance (F.HasVariant "int32" F.Slot Value Std_.Int32) where
    theVariant  = (F.Variant (GH.dataField 32 0 32 0) 4)
instance (F.HasVariant "int64" F.Slot Value Std_.Int64) where
    theVariant  = (F.Variant (GH.dataField 0 1 64 0) 5)
instance (F.HasVariant "uint8" F.Slot Value Std_.Word8) where
    theVariant  = (F.Variant (GH.dataField 16 0 8 0) 6)
instance (F.HasVariant "uint16" F.Slot Value Std_.Word16) where
    theVariant  = (F.Variant (GH.dataField 16 0 16 0) 7)
instance (F.HasVariant "uint32" F.Slot Value Std_.Word32) where
    theVariant  = (F.Variant (GH.dataField 32 0 32 0) 8)
instance (F.HasVariant "uint64" F.Slot Value Std_.Word64) where
    theVariant  = (F.Variant (GH.dataField 0 1 64 0) 9)
instance (F.HasVariant "float32" F.Slot Value Std_.Float) where
    theVariant  = (F.Variant (GH.dataField 32 0 32 0) 10)
instance (F.HasVariant "float64" F.Slot Value Std_.Double) where
    theVariant  = (F.Variant (GH.dataField 0 1 64 0) 11)
instance (F.HasVariant "text" F.Slot Value Basics.Text) where
    theVariant  = (F.Variant (GH.ptrField 0) 12)
instance (F.HasVariant "data_" F.Slot Value Basics.Data) where
    theVariant  = (F.Variant (GH.ptrField 0) 13)
instance (F.HasVariant "list" F.Slot Value Basics.AnyPointer) where
    theVariant  = (F.Variant (GH.ptrField 0) 14)
instance (F.HasVariant "enum" F.Slot Value Std_.Word16) where
    theVariant  = (F.Variant (GH.dataField 16 0 16 0) 15)
instance (F.HasVariant "struct" F.Slot Value Basics.AnyPointer) where
    theVariant  = (F.Variant (GH.ptrField 0) 16)
instance (F.HasVariant "interface" F.Slot Value ()) where
    theVariant  = (F.Variant GH.voidField 17)
instance (F.HasVariant "anyPointer" F.Slot Value Basics.AnyPointer) where
    theVariant  = (F.Variant (GH.ptrField 0) 18)
data Annotation 
type instance (R.ReprFor Annotation) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Annotation) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Annotation) where
    type AllocHint Annotation = ()
    new  = GH.newStruct
instance (F.HasField "id" F.Slot Annotation Std_.Word64) where
    theField  = (GH.dataField 0 0 64 0)
instance (F.HasField "value" F.Slot Annotation Value) where
    theField  = (GH.ptrField 0)
instance (F.HasField "brand" F.Slot Annotation Brand) where
    theField  = (GH.ptrField 1)
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
instance (F.HasField "major" F.Slot CapnpVersion Std_.Word16) where
    theField  = (GH.dataField 0 0 16 0)
instance (F.HasField "minor" F.Slot CapnpVersion Std_.Word8) where
    theField  = (GH.dataField 16 0 8 0)
instance (F.HasField "micro" F.Slot CapnpVersion Std_.Word8) where
    theField  = (GH.dataField 24 0 8 0)
data CodeGeneratorRequest 
type instance (R.ReprFor CodeGeneratorRequest) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct CodeGeneratorRequest) where
    numStructWords  = 0
    numStructPtrs  = 4
instance (C.Allocate CodeGeneratorRequest) where
    type AllocHint CodeGeneratorRequest = ()
    new  = GH.newStruct
instance (F.HasField "nodes" F.Slot CodeGeneratorRequest (R.List Node)) where
    theField  = (GH.ptrField 0)
instance (F.HasField "requestedFiles" F.Slot CodeGeneratorRequest (R.List CodeGeneratorRequest'RequestedFile)) where
    theField  = (GH.ptrField 1)
instance (F.HasField "capnpVersion" F.Slot CodeGeneratorRequest CapnpVersion) where
    theField  = (GH.ptrField 2)
instance (F.HasField "sourceInfo" F.Slot CodeGeneratorRequest (R.List Node'SourceInfo)) where
    theField  = (GH.ptrField 3)
data CodeGeneratorRequest'RequestedFile 
type instance (R.ReprFor CodeGeneratorRequest'RequestedFile) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct CodeGeneratorRequest'RequestedFile) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate CodeGeneratorRequest'RequestedFile) where
    type AllocHint CodeGeneratorRequest'RequestedFile = ()
    new  = GH.newStruct
instance (F.HasField "id" F.Slot CodeGeneratorRequest'RequestedFile Std_.Word64) where
    theField  = (GH.dataField 0 0 64 0)
instance (F.HasField "filename" F.Slot CodeGeneratorRequest'RequestedFile Basics.Text) where
    theField  = (GH.ptrField 0)
instance (F.HasField "imports" F.Slot CodeGeneratorRequest'RequestedFile (R.List CodeGeneratorRequest'RequestedFile'Import)) where
    theField  = (GH.ptrField 1)
data CodeGeneratorRequest'RequestedFile'Import 
type instance (R.ReprFor CodeGeneratorRequest'RequestedFile'Import) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct CodeGeneratorRequest'RequestedFile'Import) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate CodeGeneratorRequest'RequestedFile'Import) where
    type AllocHint CodeGeneratorRequest'RequestedFile'Import = ()
    new  = GH.newStruct
instance (F.HasField "id" F.Slot CodeGeneratorRequest'RequestedFile'Import Std_.Word64) where
    theField  = (GH.dataField 0 0 64 0)
instance (F.HasField "name" F.Slot CodeGeneratorRequest'RequestedFile'Import Basics.Text) where
    theField  = (GH.ptrField 0)