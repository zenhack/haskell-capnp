{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
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
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Node 
type instance (R.ReprFor Node) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field F.Slot (Node) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "id" F.Slot (Node) Std_.Word64)
instance (OL.IsLabel "displayName" (F.Field F.Slot (Node) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "displayName" F.Slot (Node) Basics.Text)
instance (OL.IsLabel "displayNamePrefixLength" (F.Field F.Slot (Node) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 1 32 0)
instance (F.HasField "displayNamePrefixLength" F.Slot (Node) Std_.Word32)
instance (OL.IsLabel "scopeId" (F.Field F.Slot (Node) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 2 64 0)
instance (F.HasField "scopeId" F.Slot (Node) Std_.Word64)
instance (OL.IsLabel "nestedNodes" (F.Field F.Slot (Node) (R.List Node'NestedNode))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "nestedNodes" F.Slot (Node) (R.List Node'NestedNode))
instance (OL.IsLabel "annotations" (F.Field F.Slot (Node) (R.List Annotation))) where
    fromLabel  = (GH.ptrField 2)
instance (F.HasField "annotations" F.Slot (Node) (R.List Annotation))
instance (OL.IsLabel "parameters" (F.Field F.Slot (Node) (R.List Node'Parameter))) where
    fromLabel  = (GH.ptrField 5)
instance (F.HasField "parameters" F.Slot (Node) (R.List Node'Parameter))
instance (OL.IsLabel "isGeneric" (F.Field F.Slot (Node) Std_.Bool)) where
    fromLabel  = (GH.dataField 32 4 1 0)
instance (F.HasField "isGeneric" F.Slot (Node) Std_.Bool)
data Node'struct 
type instance (R.ReprFor Node'struct) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "dataWordCount" (F.Field F.Slot (Node'struct) Std_.Word16)) where
    fromLabel  = (GH.dataField 48 1 16 0)
instance (F.HasField "dataWordCount" F.Slot (Node'struct) Std_.Word16)
instance (OL.IsLabel "pointerCount" (F.Field F.Slot (Node'struct) Std_.Word16)) where
    fromLabel  = (GH.dataField 0 3 16 0)
instance (F.HasField "pointerCount" F.Slot (Node'struct) Std_.Word16)
instance (OL.IsLabel "preferredListEncoding" (F.Field F.Slot (Node'struct) ElementSize)) where
    fromLabel  = (GH.dataField 16 3 16 0)
instance (F.HasField "preferredListEncoding" F.Slot (Node'struct) ElementSize)
instance (OL.IsLabel "isGroup" (F.Field F.Slot (Node'struct) Std_.Bool)) where
    fromLabel  = (GH.dataField 32 3 1 0)
instance (F.HasField "isGroup" F.Slot (Node'struct) Std_.Bool)
instance (OL.IsLabel "discriminantCount" (F.Field F.Slot (Node'struct) Std_.Word16)) where
    fromLabel  = (GH.dataField 48 3 16 0)
instance (F.HasField "discriminantCount" F.Slot (Node'struct) Std_.Word16)
instance (OL.IsLabel "discriminantOffset" (F.Field F.Slot (Node'struct) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 4 32 0)
instance (F.HasField "discriminantOffset" F.Slot (Node'struct) Std_.Word32)
instance (OL.IsLabel "fields" (F.Field F.Slot (Node'struct) (R.List Field))) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "fields" F.Slot (Node'struct) (R.List Field))
data Node'enum 
type instance (R.ReprFor Node'enum) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "enumerants" (F.Field F.Slot (Node'enum) (R.List Enumerant))) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "enumerants" F.Slot (Node'enum) (R.List Enumerant))
data Node'interface 
type instance (R.ReprFor Node'interface) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "methods" (F.Field F.Slot (Node'interface) (R.List Method))) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "methods" F.Slot (Node'interface) (R.List Method))
instance (OL.IsLabel "superclasses" (F.Field F.Slot (Node'interface) (R.List Superclass))) where
    fromLabel  = (GH.ptrField 4)
instance (F.HasField "superclasses" F.Slot (Node'interface) (R.List Superclass))
data Node'const 
type instance (R.ReprFor Node'const) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "type_" (F.Field F.Slot (Node'const) Type)) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "type_" F.Slot (Node'const) Type)
instance (OL.IsLabel "value" (F.Field F.Slot (Node'const) Value)) where
    fromLabel  = (GH.ptrField 4)
instance (F.HasField "value" F.Slot (Node'const) Value)
data Node'annotation 
type instance (R.ReprFor Node'annotation) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "type_" (F.Field F.Slot (Node'annotation) Type)) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "type_" F.Slot (Node'annotation) Type)
instance (OL.IsLabel "targetsFile" (F.Field F.Slot (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 48 1 1 0)
instance (F.HasField "targetsFile" F.Slot (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsConst" (F.Field F.Slot (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 49 1 1 0)
instance (F.HasField "targetsConst" F.Slot (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsEnum" (F.Field F.Slot (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 50 1 1 0)
instance (F.HasField "targetsEnum" F.Slot (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsEnumerant" (F.Field F.Slot (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 51 1 1 0)
instance (F.HasField "targetsEnumerant" F.Slot (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsStruct" (F.Field F.Slot (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 52 1 1 0)
instance (F.HasField "targetsStruct" F.Slot (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsField" (F.Field F.Slot (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 53 1 1 0)
instance (F.HasField "targetsField" F.Slot (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsUnion" (F.Field F.Slot (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 54 1 1 0)
instance (F.HasField "targetsUnion" F.Slot (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsGroup" (F.Field F.Slot (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 55 1 1 0)
instance (F.HasField "targetsGroup" F.Slot (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsInterface" (F.Field F.Slot (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 56 1 1 0)
instance (F.HasField "targetsInterface" F.Slot (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsMethod" (F.Field F.Slot (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 57 1 1 0)
instance (F.HasField "targetsMethod" F.Slot (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsParam" (F.Field F.Slot (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 58 1 1 0)
instance (F.HasField "targetsParam" F.Slot (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsAnnotation" (F.Field F.Slot (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 59 1 1 0)
instance (F.HasField "targetsAnnotation" F.Slot (Node'annotation) Std_.Bool)
data Node'Parameter 
type instance (R.ReprFor Node'Parameter) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "name" (F.Field F.Slot (Node'Parameter) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" F.Slot (Node'Parameter) Basics.Text)
data Node'NestedNode 
type instance (R.ReprFor Node'NestedNode) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "name" (F.Field F.Slot (Node'NestedNode) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" F.Slot (Node'NestedNode) Basics.Text)
instance (OL.IsLabel "id" (F.Field F.Slot (Node'NestedNode) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "id" F.Slot (Node'NestedNode) Std_.Word64)
data Node'SourceInfo 
type instance (R.ReprFor Node'SourceInfo) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field F.Slot (Node'SourceInfo) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "id" F.Slot (Node'SourceInfo) Std_.Word64)
instance (OL.IsLabel "docComment" (F.Field F.Slot (Node'SourceInfo) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "docComment" F.Slot (Node'SourceInfo) Basics.Text)
instance (OL.IsLabel "members" (F.Field F.Slot (Node'SourceInfo) (R.List Node'SourceInfo'Member))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "members" F.Slot (Node'SourceInfo) (R.List Node'SourceInfo'Member))
data Node'SourceInfo'Member 
type instance (R.ReprFor Node'SourceInfo'Member) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "docComment" (F.Field F.Slot (Node'SourceInfo'Member) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "docComment" F.Slot (Node'SourceInfo'Member) Basics.Text)
data Field 
type instance (R.ReprFor Field) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "name" (F.Field F.Slot (Field) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" F.Slot (Field) Basics.Text)
instance (OL.IsLabel "codeOrder" (F.Field F.Slot (Field) Std_.Word16)) where
    fromLabel  = (GH.dataField 0 0 16 0)
instance (F.HasField "codeOrder" F.Slot (Field) Std_.Word16)
instance (OL.IsLabel "annotations" (F.Field F.Slot (Field) (R.List Annotation))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "annotations" F.Slot (Field) (R.List Annotation))
instance (OL.IsLabel "discriminantValue" (F.Field F.Slot (Field) Std_.Word16)) where
    fromLabel  = (GH.dataField 16 0 16 65535)
instance (F.HasField "discriminantValue" F.Slot (Field) Std_.Word16)
instance (OL.IsLabel "ordinal" (F.Field F.Group (Field) Field'ordinal)) where
    fromLabel  = GH.groupField
instance (F.HasField "ordinal" F.Group (Field) Field'ordinal)
data Field'slot 
type instance (R.ReprFor Field'slot) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "offset" (F.Field F.Slot (Field'slot) Std_.Word32)) where
    fromLabel  = (GH.dataField 32 0 32 0)
instance (F.HasField "offset" F.Slot (Field'slot) Std_.Word32)
instance (OL.IsLabel "type_" (F.Field F.Slot (Field'slot) Type)) where
    fromLabel  = (GH.ptrField 2)
instance (F.HasField "type_" F.Slot (Field'slot) Type)
instance (OL.IsLabel "defaultValue" (F.Field F.Slot (Field'slot) Value)) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "defaultValue" F.Slot (Field'slot) Value)
instance (OL.IsLabel "hadExplicitDefault" (F.Field F.Slot (Field'slot) Std_.Bool)) where
    fromLabel  = (GH.dataField 0 2 1 0)
instance (F.HasField "hadExplicitDefault" F.Slot (Field'slot) Std_.Bool)
data Field'group 
type instance (R.ReprFor Field'group) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "typeId" (F.Field F.Slot (Field'group) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 2 64 0)
instance (F.HasField "typeId" F.Slot (Field'group) Std_.Word64)
data Field'ordinal 
type instance (R.ReprFor Field'ordinal) = (R.Ptr (Std_.Just R.Struct))
data Enumerant 
type instance (R.ReprFor Enumerant) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "name" (F.Field F.Slot (Enumerant) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" F.Slot (Enumerant) Basics.Text)
instance (OL.IsLabel "codeOrder" (F.Field F.Slot (Enumerant) Std_.Word16)) where
    fromLabel  = (GH.dataField 0 0 16 0)
instance (F.HasField "codeOrder" F.Slot (Enumerant) Std_.Word16)
instance (OL.IsLabel "annotations" (F.Field F.Slot (Enumerant) (R.List Annotation))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "annotations" F.Slot (Enumerant) (R.List Annotation))
data Superclass 
type instance (R.ReprFor Superclass) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field F.Slot (Superclass) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "id" F.Slot (Superclass) Std_.Word64)
instance (OL.IsLabel "brand" (F.Field F.Slot (Superclass) Brand)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "brand" F.Slot (Superclass) Brand)
data Method 
type instance (R.ReprFor Method) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "name" (F.Field F.Slot (Method) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" F.Slot (Method) Basics.Text)
instance (OL.IsLabel "codeOrder" (F.Field F.Slot (Method) Std_.Word16)) where
    fromLabel  = (GH.dataField 0 0 16 0)
instance (F.HasField "codeOrder" F.Slot (Method) Std_.Word16)
instance (OL.IsLabel "paramStructType" (F.Field F.Slot (Method) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 1 64 0)
instance (F.HasField "paramStructType" F.Slot (Method) Std_.Word64)
instance (OL.IsLabel "resultStructType" (F.Field F.Slot (Method) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 2 64 0)
instance (F.HasField "resultStructType" F.Slot (Method) Std_.Word64)
instance (OL.IsLabel "annotations" (F.Field F.Slot (Method) (R.List Annotation))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "annotations" F.Slot (Method) (R.List Annotation))
instance (OL.IsLabel "paramBrand" (F.Field F.Slot (Method) Brand)) where
    fromLabel  = (GH.ptrField 2)
instance (F.HasField "paramBrand" F.Slot (Method) Brand)
instance (OL.IsLabel "resultBrand" (F.Field F.Slot (Method) Brand)) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "resultBrand" F.Slot (Method) Brand)
instance (OL.IsLabel "implicitParameters" (F.Field F.Slot (Method) (R.List Node'Parameter))) where
    fromLabel  = (GH.ptrField 4)
instance (F.HasField "implicitParameters" F.Slot (Method) (R.List Node'Parameter))
data Type 
type instance (R.ReprFor Type) = (R.Ptr (Std_.Just R.Struct))
data Type'list 
type instance (R.ReprFor Type'list) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "elementType" (F.Field F.Slot (Type'list) Type)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "elementType" F.Slot (Type'list) Type)
data Type'enum 
type instance (R.ReprFor Type'enum) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "typeId" (F.Field F.Slot (Type'enum) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 1 64 0)
instance (F.HasField "typeId" F.Slot (Type'enum) Std_.Word64)
instance (OL.IsLabel "brand" (F.Field F.Slot (Type'enum) Brand)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "brand" F.Slot (Type'enum) Brand)
data Type'struct 
type instance (R.ReprFor Type'struct) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "typeId" (F.Field F.Slot (Type'struct) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 1 64 0)
instance (F.HasField "typeId" F.Slot (Type'struct) Std_.Word64)
instance (OL.IsLabel "brand" (F.Field F.Slot (Type'struct) Brand)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "brand" F.Slot (Type'struct) Brand)
data Type'interface 
type instance (R.ReprFor Type'interface) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "typeId" (F.Field F.Slot (Type'interface) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 1 64 0)
instance (F.HasField "typeId" F.Slot (Type'interface) Std_.Word64)
instance (OL.IsLabel "brand" (F.Field F.Slot (Type'interface) Brand)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "brand" F.Slot (Type'interface) Brand)
data Type'anyPointer 
type instance (R.ReprFor Type'anyPointer) = (R.Ptr (Std_.Just R.Struct))
data Type'anyPointer'unconstrained 
type instance (R.ReprFor Type'anyPointer'unconstrained) = (R.Ptr (Std_.Just R.Struct))
data Type'anyPointer'parameter 
type instance (R.ReprFor Type'anyPointer'parameter) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "scopeId" (F.Field F.Slot (Type'anyPointer'parameter) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 2 64 0)
instance (F.HasField "scopeId" F.Slot (Type'anyPointer'parameter) Std_.Word64)
instance (OL.IsLabel "parameterIndex" (F.Field F.Slot (Type'anyPointer'parameter) Std_.Word16)) where
    fromLabel  = (GH.dataField 16 1 16 0)
instance (F.HasField "parameterIndex" F.Slot (Type'anyPointer'parameter) Std_.Word16)
data Type'anyPointer'implicitMethodParameter 
type instance (R.ReprFor Type'anyPointer'implicitMethodParameter) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "parameterIndex" (F.Field F.Slot (Type'anyPointer'implicitMethodParameter) Std_.Word16)) where
    fromLabel  = (GH.dataField 16 1 16 0)
instance (F.HasField "parameterIndex" F.Slot (Type'anyPointer'implicitMethodParameter) Std_.Word16)
data Brand 
type instance (R.ReprFor Brand) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "scopes" (F.Field F.Slot (Brand) (R.List Brand'Scope))) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "scopes" F.Slot (Brand) (R.List Brand'Scope))
data Brand'Scope 
type instance (R.ReprFor Brand'Scope) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "scopeId" (F.Field F.Slot (Brand'Scope) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "scopeId" F.Slot (Brand'Scope) Std_.Word64)
data Brand'Binding 
type instance (R.ReprFor Brand'Binding) = (R.Ptr (Std_.Just R.Struct))
data Value 
type instance (R.ReprFor Value) = (R.Ptr (Std_.Just R.Struct))
data Annotation 
type instance (R.ReprFor Annotation) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field F.Slot (Annotation) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "id" F.Slot (Annotation) Std_.Word64)
instance (OL.IsLabel "value" (F.Field F.Slot (Annotation) Value)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "value" F.Slot (Annotation) Value)
instance (OL.IsLabel "brand" (F.Field F.Slot (Annotation) Brand)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "brand" F.Slot (Annotation) Brand)
data ElementSize 
type instance (R.ReprFor ElementSize) = (R.Data R.Sz16)
data CapnpVersion 
type instance (R.ReprFor CapnpVersion) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "major" (F.Field F.Slot (CapnpVersion) Std_.Word16)) where
    fromLabel  = (GH.dataField 0 0 16 0)
instance (F.HasField "major" F.Slot (CapnpVersion) Std_.Word16)
instance (OL.IsLabel "minor" (F.Field F.Slot (CapnpVersion) Std_.Word8)) where
    fromLabel  = (GH.dataField 16 0 8 0)
instance (F.HasField "minor" F.Slot (CapnpVersion) Std_.Word8)
instance (OL.IsLabel "micro" (F.Field F.Slot (CapnpVersion) Std_.Word8)) where
    fromLabel  = (GH.dataField 24 0 8 0)
instance (F.HasField "micro" F.Slot (CapnpVersion) Std_.Word8)
data CodeGeneratorRequest 
type instance (R.ReprFor CodeGeneratorRequest) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "nodes" (F.Field F.Slot (CodeGeneratorRequest) (R.List Node))) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "nodes" F.Slot (CodeGeneratorRequest) (R.List Node))
instance (OL.IsLabel "requestedFiles" (F.Field F.Slot (CodeGeneratorRequest) (R.List CodeGeneratorRequest'RequestedFile))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "requestedFiles" F.Slot (CodeGeneratorRequest) (R.List CodeGeneratorRequest'RequestedFile))
instance (OL.IsLabel "capnpVersion" (F.Field F.Slot (CodeGeneratorRequest) CapnpVersion)) where
    fromLabel  = (GH.ptrField 2)
instance (F.HasField "capnpVersion" F.Slot (CodeGeneratorRequest) CapnpVersion)
instance (OL.IsLabel "sourceInfo" (F.Field F.Slot (CodeGeneratorRequest) (R.List Node'SourceInfo))) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "sourceInfo" F.Slot (CodeGeneratorRequest) (R.List Node'SourceInfo))
data CodeGeneratorRequest'RequestedFile 
type instance (R.ReprFor CodeGeneratorRequest'RequestedFile) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field F.Slot (CodeGeneratorRequest'RequestedFile) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "id" F.Slot (CodeGeneratorRequest'RequestedFile) Std_.Word64)
instance (OL.IsLabel "filename" (F.Field F.Slot (CodeGeneratorRequest'RequestedFile) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "filename" F.Slot (CodeGeneratorRequest'RequestedFile) Basics.Text)
instance (OL.IsLabel "imports" (F.Field F.Slot (CodeGeneratorRequest'RequestedFile) (R.List CodeGeneratorRequest'RequestedFile'Import))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "imports" F.Slot (CodeGeneratorRequest'RequestedFile) (R.List CodeGeneratorRequest'RequestedFile'Import))
data CodeGeneratorRequest'RequestedFile'Import 
type instance (R.ReprFor CodeGeneratorRequest'RequestedFile'Import) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field F.Slot (CodeGeneratorRequest'RequestedFile'Import) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "id" F.Slot (CodeGeneratorRequest'RequestedFile'Import) Std_.Word64)
instance (OL.IsLabel "name" (F.Field F.Slot (CodeGeneratorRequest'RequestedFile'Import) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" F.Slot (CodeGeneratorRequest'RequestedFile'Import) Basics.Text)