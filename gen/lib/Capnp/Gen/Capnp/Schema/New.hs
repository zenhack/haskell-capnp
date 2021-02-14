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
instance (OL.IsLabel "id" (F.Field (Node) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "id" (Node) Std_.Word64)
instance (OL.IsLabel "displayName" (F.Field (Node) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "displayName" (Node) Basics.Text)
instance (OL.IsLabel "displayNamePrefixLength" (F.Field (Node) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 1 32 0)
instance (F.HasField "displayNamePrefixLength" (Node) Std_.Word32)
instance (OL.IsLabel "scopeId" (F.Field (Node) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 2 64 0)
instance (F.HasField "scopeId" (Node) Std_.Word64)
instance (OL.IsLabel "nestedNodes" (F.Field (Node) (R.List Node'NestedNode))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "nestedNodes" (Node) (R.List Node'NestedNode))
instance (OL.IsLabel "annotations" (F.Field (Node) (R.List Annotation))) where
    fromLabel  = (GH.ptrField 2)
instance (F.HasField "annotations" (Node) (R.List Annotation))
instance (OL.IsLabel "parameters" (F.Field (Node) (R.List Node'Parameter))) where
    fromLabel  = (GH.ptrField 5)
instance (F.HasField "parameters" (Node) (R.List Node'Parameter))
instance (OL.IsLabel "isGeneric" (F.Field (Node) Std_.Bool)) where
    fromLabel  = (GH.dataField 32 4 1 0)
instance (F.HasField "isGeneric" (Node) Std_.Bool)
data Node'struct 
type instance (R.ReprFor Node'struct) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "dataWordCount" (F.Field (Node'struct) Std_.Word16)) where
    fromLabel  = (GH.dataField 48 1 16 0)
instance (F.HasField "dataWordCount" (Node'struct) Std_.Word16)
instance (OL.IsLabel "pointerCount" (F.Field (Node'struct) Std_.Word16)) where
    fromLabel  = (GH.dataField 0 3 16 0)
instance (F.HasField "pointerCount" (Node'struct) Std_.Word16)
instance (OL.IsLabel "preferredListEncoding" (F.Field (Node'struct) ElementSize)) where
    fromLabel  = (GH.dataField 16 3 16 0)
instance (F.HasField "preferredListEncoding" (Node'struct) ElementSize)
instance (OL.IsLabel "isGroup" (F.Field (Node'struct) Std_.Bool)) where
    fromLabel  = (GH.dataField 32 3 1 0)
instance (F.HasField "isGroup" (Node'struct) Std_.Bool)
instance (OL.IsLabel "discriminantCount" (F.Field (Node'struct) Std_.Word16)) where
    fromLabel  = (GH.dataField 48 3 16 0)
instance (F.HasField "discriminantCount" (Node'struct) Std_.Word16)
instance (OL.IsLabel "discriminantOffset" (F.Field (Node'struct) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 4 32 0)
instance (F.HasField "discriminantOffset" (Node'struct) Std_.Word32)
instance (OL.IsLabel "fields" (F.Field (Node'struct) (R.List Field))) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "fields" (Node'struct) (R.List Field))
data Node'enum 
type instance (R.ReprFor Node'enum) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "enumerants" (F.Field (Node'enum) (R.List Enumerant))) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "enumerants" (Node'enum) (R.List Enumerant))
data Node'interface 
type instance (R.ReprFor Node'interface) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "methods" (F.Field (Node'interface) (R.List Method))) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "methods" (Node'interface) (R.List Method))
instance (OL.IsLabel "superclasses" (F.Field (Node'interface) (R.List Superclass))) where
    fromLabel  = (GH.ptrField 4)
instance (F.HasField "superclasses" (Node'interface) (R.List Superclass))
data Node'const 
type instance (R.ReprFor Node'const) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "type_" (F.Field (Node'const) Type)) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "type_" (Node'const) Type)
instance (OL.IsLabel "value" (F.Field (Node'const) Value)) where
    fromLabel  = (GH.ptrField 4)
instance (F.HasField "value" (Node'const) Value)
data Node'annotation 
type instance (R.ReprFor Node'annotation) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "type_" (F.Field (Node'annotation) Type)) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "type_" (Node'annotation) Type)
instance (OL.IsLabel "targetsFile" (F.Field (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 48 1 1 0)
instance (F.HasField "targetsFile" (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsConst" (F.Field (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 49 1 1 0)
instance (F.HasField "targetsConst" (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsEnum" (F.Field (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 50 1 1 0)
instance (F.HasField "targetsEnum" (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsEnumerant" (F.Field (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 51 1 1 0)
instance (F.HasField "targetsEnumerant" (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsStruct" (F.Field (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 52 1 1 0)
instance (F.HasField "targetsStruct" (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsField" (F.Field (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 53 1 1 0)
instance (F.HasField "targetsField" (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsUnion" (F.Field (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 54 1 1 0)
instance (F.HasField "targetsUnion" (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsGroup" (F.Field (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 55 1 1 0)
instance (F.HasField "targetsGroup" (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsInterface" (F.Field (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 56 1 1 0)
instance (F.HasField "targetsInterface" (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsMethod" (F.Field (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 57 1 1 0)
instance (F.HasField "targetsMethod" (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsParam" (F.Field (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 58 1 1 0)
instance (F.HasField "targetsParam" (Node'annotation) Std_.Bool)
instance (OL.IsLabel "targetsAnnotation" (F.Field (Node'annotation) Std_.Bool)) where
    fromLabel  = (GH.dataField 59 1 1 0)
instance (F.HasField "targetsAnnotation" (Node'annotation) Std_.Bool)
data Node'Parameter 
type instance (R.ReprFor Node'Parameter) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "name" (F.Field (Node'Parameter) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" (Node'Parameter) Basics.Text)
data Node'NestedNode 
type instance (R.ReprFor Node'NestedNode) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "name" (F.Field (Node'NestedNode) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" (Node'NestedNode) Basics.Text)
instance (OL.IsLabel "id" (F.Field (Node'NestedNode) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "id" (Node'NestedNode) Std_.Word64)
data Node'SourceInfo 
type instance (R.ReprFor Node'SourceInfo) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field (Node'SourceInfo) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "id" (Node'SourceInfo) Std_.Word64)
instance (OL.IsLabel "docComment" (F.Field (Node'SourceInfo) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "docComment" (Node'SourceInfo) Basics.Text)
instance (OL.IsLabel "members" (F.Field (Node'SourceInfo) (R.List Node'SourceInfo'Member))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "members" (Node'SourceInfo) (R.List Node'SourceInfo'Member))
data Node'SourceInfo'Member 
type instance (R.ReprFor Node'SourceInfo'Member) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "docComment" (F.Field (Node'SourceInfo'Member) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "docComment" (Node'SourceInfo'Member) Basics.Text)
data Field 
type instance (R.ReprFor Field) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "name" (F.Field (Field) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" (Field) Basics.Text)
instance (OL.IsLabel "codeOrder" (F.Field (Field) Std_.Word16)) where
    fromLabel  = (GH.dataField 0 0 16 0)
instance (F.HasField "codeOrder" (Field) Std_.Word16)
instance (OL.IsLabel "annotations" (F.Field (Field) (R.List Annotation))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "annotations" (Field) (R.List Annotation))
instance (OL.IsLabel "discriminantValue" (F.Field (Field) Std_.Word16)) where
    fromLabel  = (GH.dataField 16 0 16 65535)
instance (F.HasField "discriminantValue" (Field) Std_.Word16)
instance (OL.IsLabel "ordinal" (F.Field (Field) Field'ordinal)) where
    fromLabel  = Std_.undefined
instance (F.HasField "ordinal" (Field) Field'ordinal)
data Field'slot 
type instance (R.ReprFor Field'slot) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "offset" (F.Field (Field'slot) Std_.Word32)) where
    fromLabel  = (GH.dataField 32 0 32 0)
instance (F.HasField "offset" (Field'slot) Std_.Word32)
instance (OL.IsLabel "type_" (F.Field (Field'slot) Type)) where
    fromLabel  = (GH.ptrField 2)
instance (F.HasField "type_" (Field'slot) Type)
instance (OL.IsLabel "defaultValue" (F.Field (Field'slot) Value)) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "defaultValue" (Field'slot) Value)
instance (OL.IsLabel "hadExplicitDefault" (F.Field (Field'slot) Std_.Bool)) where
    fromLabel  = (GH.dataField 0 2 1 0)
instance (F.HasField "hadExplicitDefault" (Field'slot) Std_.Bool)
data Field'group 
type instance (R.ReprFor Field'group) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "typeId" (F.Field (Field'group) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 2 64 0)
instance (F.HasField "typeId" (Field'group) Std_.Word64)
data Field'ordinal 
type instance (R.ReprFor Field'ordinal) = (R.Ptr (Std_.Just R.Struct))
data Enumerant 
type instance (R.ReprFor Enumerant) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "name" (F.Field (Enumerant) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" (Enumerant) Basics.Text)
instance (OL.IsLabel "codeOrder" (F.Field (Enumerant) Std_.Word16)) where
    fromLabel  = (GH.dataField 0 0 16 0)
instance (F.HasField "codeOrder" (Enumerant) Std_.Word16)
instance (OL.IsLabel "annotations" (F.Field (Enumerant) (R.List Annotation))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "annotations" (Enumerant) (R.List Annotation))
data Superclass 
type instance (R.ReprFor Superclass) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field (Superclass) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "id" (Superclass) Std_.Word64)
instance (OL.IsLabel "brand" (F.Field (Superclass) Brand)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "brand" (Superclass) Brand)
data Method 
type instance (R.ReprFor Method) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "name" (F.Field (Method) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" (Method) Basics.Text)
instance (OL.IsLabel "codeOrder" (F.Field (Method) Std_.Word16)) where
    fromLabel  = (GH.dataField 0 0 16 0)
instance (F.HasField "codeOrder" (Method) Std_.Word16)
instance (OL.IsLabel "paramStructType" (F.Field (Method) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 1 64 0)
instance (F.HasField "paramStructType" (Method) Std_.Word64)
instance (OL.IsLabel "resultStructType" (F.Field (Method) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 2 64 0)
instance (F.HasField "resultStructType" (Method) Std_.Word64)
instance (OL.IsLabel "annotations" (F.Field (Method) (R.List Annotation))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "annotations" (Method) (R.List Annotation))
instance (OL.IsLabel "paramBrand" (F.Field (Method) Brand)) where
    fromLabel  = (GH.ptrField 2)
instance (F.HasField "paramBrand" (Method) Brand)
instance (OL.IsLabel "resultBrand" (F.Field (Method) Brand)) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "resultBrand" (Method) Brand)
instance (OL.IsLabel "implicitParameters" (F.Field (Method) (R.List Node'Parameter))) where
    fromLabel  = (GH.ptrField 4)
instance (F.HasField "implicitParameters" (Method) (R.List Node'Parameter))
data Type 
type instance (R.ReprFor Type) = (R.Ptr (Std_.Just R.Struct))
data Type'list 
type instance (R.ReprFor Type'list) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "elementType" (F.Field (Type'list) Type)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "elementType" (Type'list) Type)
data Type'enum 
type instance (R.ReprFor Type'enum) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "typeId" (F.Field (Type'enum) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 1 64 0)
instance (F.HasField "typeId" (Type'enum) Std_.Word64)
instance (OL.IsLabel "brand" (F.Field (Type'enum) Brand)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "brand" (Type'enum) Brand)
data Type'struct 
type instance (R.ReprFor Type'struct) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "typeId" (F.Field (Type'struct) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 1 64 0)
instance (F.HasField "typeId" (Type'struct) Std_.Word64)
instance (OL.IsLabel "brand" (F.Field (Type'struct) Brand)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "brand" (Type'struct) Brand)
data Type'interface 
type instance (R.ReprFor Type'interface) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "typeId" (F.Field (Type'interface) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 1 64 0)
instance (F.HasField "typeId" (Type'interface) Std_.Word64)
instance (OL.IsLabel "brand" (F.Field (Type'interface) Brand)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "brand" (Type'interface) Brand)
data Type'anyPointer 
type instance (R.ReprFor Type'anyPointer) = (R.Ptr (Std_.Just R.Struct))
data Type'anyPointer'unconstrained 
type instance (R.ReprFor Type'anyPointer'unconstrained) = (R.Ptr (Std_.Just R.Struct))
data Type'anyPointer'parameter 
type instance (R.ReprFor Type'anyPointer'parameter) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "scopeId" (F.Field (Type'anyPointer'parameter) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 2 64 0)
instance (F.HasField "scopeId" (Type'anyPointer'parameter) Std_.Word64)
instance (OL.IsLabel "parameterIndex" (F.Field (Type'anyPointer'parameter) Std_.Word16)) where
    fromLabel  = (GH.dataField 16 1 16 0)
instance (F.HasField "parameterIndex" (Type'anyPointer'parameter) Std_.Word16)
data Type'anyPointer'implicitMethodParameter 
type instance (R.ReprFor Type'anyPointer'implicitMethodParameter) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "parameterIndex" (F.Field (Type'anyPointer'implicitMethodParameter) Std_.Word16)) where
    fromLabel  = (GH.dataField 16 1 16 0)
instance (F.HasField "parameterIndex" (Type'anyPointer'implicitMethodParameter) Std_.Word16)
data Brand 
type instance (R.ReprFor Brand) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "scopes" (F.Field (Brand) (R.List Brand'Scope))) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "scopes" (Brand) (R.List Brand'Scope))
data Brand'Scope 
type instance (R.ReprFor Brand'Scope) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "scopeId" (F.Field (Brand'Scope) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "scopeId" (Brand'Scope) Std_.Word64)
data Brand'Binding 
type instance (R.ReprFor Brand'Binding) = (R.Ptr (Std_.Just R.Struct))
data Value 
type instance (R.ReprFor Value) = (R.Ptr (Std_.Just R.Struct))
data Annotation 
type instance (R.ReprFor Annotation) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field (Annotation) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "id" (Annotation) Std_.Word64)
instance (OL.IsLabel "value" (F.Field (Annotation) Value)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "value" (Annotation) Value)
instance (OL.IsLabel "brand" (F.Field (Annotation) Brand)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "brand" (Annotation) Brand)
data ElementSize 
type instance (R.ReprFor ElementSize) = (R.Data R.Sz16)
data CapnpVersion 
type instance (R.ReprFor CapnpVersion) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "major" (F.Field (CapnpVersion) Std_.Word16)) where
    fromLabel  = (GH.dataField 0 0 16 0)
instance (F.HasField "major" (CapnpVersion) Std_.Word16)
instance (OL.IsLabel "minor" (F.Field (CapnpVersion) Std_.Word8)) where
    fromLabel  = (GH.dataField 16 0 8 0)
instance (F.HasField "minor" (CapnpVersion) Std_.Word8)
instance (OL.IsLabel "micro" (F.Field (CapnpVersion) Std_.Word8)) where
    fromLabel  = (GH.dataField 24 0 8 0)
instance (F.HasField "micro" (CapnpVersion) Std_.Word8)
data CodeGeneratorRequest 
type instance (R.ReprFor CodeGeneratorRequest) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "nodes" (F.Field (CodeGeneratorRequest) (R.List Node))) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "nodes" (CodeGeneratorRequest) (R.List Node))
instance (OL.IsLabel "requestedFiles" (F.Field (CodeGeneratorRequest) (R.List CodeGeneratorRequest'RequestedFile))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "requestedFiles" (CodeGeneratorRequest) (R.List CodeGeneratorRequest'RequestedFile))
instance (OL.IsLabel "capnpVersion" (F.Field (CodeGeneratorRequest) CapnpVersion)) where
    fromLabel  = (GH.ptrField 2)
instance (F.HasField "capnpVersion" (CodeGeneratorRequest) CapnpVersion)
instance (OL.IsLabel "sourceInfo" (F.Field (CodeGeneratorRequest) (R.List Node'SourceInfo))) where
    fromLabel  = (GH.ptrField 3)
instance (F.HasField "sourceInfo" (CodeGeneratorRequest) (R.List Node'SourceInfo))
data CodeGeneratorRequest'RequestedFile 
type instance (R.ReprFor CodeGeneratorRequest'RequestedFile) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field (CodeGeneratorRequest'RequestedFile) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "id" (CodeGeneratorRequest'RequestedFile) Std_.Word64)
instance (OL.IsLabel "filename" (F.Field (CodeGeneratorRequest'RequestedFile) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "filename" (CodeGeneratorRequest'RequestedFile) Basics.Text)
instance (OL.IsLabel "imports" (F.Field (CodeGeneratorRequest'RequestedFile) (R.List CodeGeneratorRequest'RequestedFile'Import))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "imports" (CodeGeneratorRequest'RequestedFile) (R.List CodeGeneratorRequest'RequestedFile'Import))
data CodeGeneratorRequest'RequestedFile'Import 
type instance (R.ReprFor CodeGeneratorRequest'RequestedFile'Import) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "id" (F.Field (CodeGeneratorRequest'RequestedFile'Import) Std_.Word64)) where
    fromLabel  = (GH.dataField 0 0 64 0)
instance (F.HasField "id" (CodeGeneratorRequest'RequestedFile'Import) Std_.Word64)
instance (OL.IsLabel "name" (F.Field (CodeGeneratorRequest'RequestedFile'Import) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" (CodeGeneratorRequest'RequestedFile'Import) Basics.Text)