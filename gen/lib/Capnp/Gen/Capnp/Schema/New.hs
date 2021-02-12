{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.Schema.New where
import qualified Capnp.Repr as R
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Node 
type instance (R.ReprFor Node) = (R.Ptr (Std_.Just R.Struct))
data Node'struct 
type instance (R.ReprFor Node'struct) = (R.Ptr (Std_.Just R.Struct))
data Node'enum 
type instance (R.ReprFor Node'enum) = (R.Ptr (Std_.Just R.Struct))
data Node'interface 
type instance (R.ReprFor Node'interface) = (R.Ptr (Std_.Just R.Struct))
data Node'const 
type instance (R.ReprFor Node'const) = (R.Ptr (Std_.Just R.Struct))
data Node'annotation 
type instance (R.ReprFor Node'annotation) = (R.Ptr (Std_.Just R.Struct))
data Node'Parameter 
type instance (R.ReprFor Node'Parameter) = (R.Ptr (Std_.Just R.Struct))
data Node'NestedNode 
type instance (R.ReprFor Node'NestedNode) = (R.Ptr (Std_.Just R.Struct))
data Node'SourceInfo 
type instance (R.ReprFor Node'SourceInfo) = (R.Ptr (Std_.Just R.Struct))
data Node'SourceInfo'Member 
type instance (R.ReprFor Node'SourceInfo'Member) = (R.Ptr (Std_.Just R.Struct))
data Field 
type instance (R.ReprFor Field) = (R.Ptr (Std_.Just R.Struct))
data Field'slot 
type instance (R.ReprFor Field'slot) = (R.Ptr (Std_.Just R.Struct))
data Field'group 
type instance (R.ReprFor Field'group) = (R.Ptr (Std_.Just R.Struct))
data Field'ordinal 
type instance (R.ReprFor Field'ordinal) = (R.Ptr (Std_.Just R.Struct))
data Enumerant 
type instance (R.ReprFor Enumerant) = (R.Ptr (Std_.Just R.Struct))
data Superclass 
type instance (R.ReprFor Superclass) = (R.Ptr (Std_.Just R.Struct))
data Method 
type instance (R.ReprFor Method) = (R.Ptr (Std_.Just R.Struct))
data Type 
type instance (R.ReprFor Type) = (R.Ptr (Std_.Just R.Struct))
data Type'list 
type instance (R.ReprFor Type'list) = (R.Ptr (Std_.Just R.Struct))
data Type'enum 
type instance (R.ReprFor Type'enum) = (R.Ptr (Std_.Just R.Struct))
data Type'struct 
type instance (R.ReprFor Type'struct) = (R.Ptr (Std_.Just R.Struct))
data Type'interface 
type instance (R.ReprFor Type'interface) = (R.Ptr (Std_.Just R.Struct))
data Type'anyPointer 
type instance (R.ReprFor Type'anyPointer) = (R.Ptr (Std_.Just R.Struct))
data Type'anyPointer'unconstrained 
type instance (R.ReprFor Type'anyPointer'unconstrained) = (R.Ptr (Std_.Just R.Struct))
data Type'anyPointer'parameter 
type instance (R.ReprFor Type'anyPointer'parameter) = (R.Ptr (Std_.Just R.Struct))
data Type'anyPointer'implicitMethodParameter 
type instance (R.ReprFor Type'anyPointer'implicitMethodParameter) = (R.Ptr (Std_.Just R.Struct))
data Brand 
type instance (R.ReprFor Brand) = (R.Ptr (Std_.Just R.Struct))
data Brand'Scope 
type instance (R.ReprFor Brand'Scope) = (R.Ptr (Std_.Just R.Struct))
data Brand'Binding 
type instance (R.ReprFor Brand'Binding) = (R.Ptr (Std_.Just R.Struct))
data Value 
type instance (R.ReprFor Value) = (R.Ptr (Std_.Just R.Struct))
data Annotation 
type instance (R.ReprFor Annotation) = (R.Ptr (Std_.Just R.Struct))
data ElementSize 
type instance (R.ReprFor ElementSize) = (R.Data R.Sz16)
data CapnpVersion 
type instance (R.ReprFor CapnpVersion) = (R.Ptr (Std_.Just R.Struct))
data CodeGeneratorRequest 
type instance (R.ReprFor CodeGeneratorRequest) = (R.Ptr (Std_.Just R.Struct))
data CodeGeneratorRequest'RequestedFile 
type instance (R.ReprFor CodeGeneratorRequest'RequestedFile) = (R.Ptr (Std_.Just R.Struct))
data CodeGeneratorRequest'RequestedFile'Import 
type instance (R.ReprFor CodeGeneratorRequest'RequestedFile'Import) = (R.Ptr (Std_.Just R.Struct))