{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.Capnp.ById.Xa93fc509624c72d9 where

-- generated from /usr/include/capnp/schema.capnp

import Data.Int
import Data.Word

import qualified Data.Capnp.BuiltinTypes
import qualified Data.Capnp.Untyped

import qualified Data.Capnp.ById.Xbdf87d7bb8304e81

data Type'anyPointer'unconstrained b
    = Type'anyPointer'unconstrained'anyKind
    | Type'anyPointer'unconstrained'struct
    | Type'anyPointer'unconstrained'list
    | Type'anyPointer'unconstrained'capability
    | Type'anyPointer'unconstrained'unknown' Word16





newtype Brand b = Brand (Data.Capnp.Untyped.Struct b)

get_Brand'scopes :: Data.Capnp.Untyped.ReadCtx m b => Brand b -> m (Data.Capnp.Untyped.ListOf b (Brand'Scope b))
get_Brand'scopes = undefined -- TODO: generate accessor values.

newtype Method b = Method (Data.Capnp.Untyped.Struct b)

get_Method'name :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m (Data.Capnp.BuiltinTypes.Text b)
get_Method'name = undefined -- TODO: generate accessor values.

get_Method'codeOrder :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m Word16
get_Method'codeOrder = undefined -- TODO: generate accessor values.

get_Method'paramStructType :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m Word64
get_Method'paramStructType = undefined -- TODO: generate accessor values.

get_Method'resultStructType :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m Word64
get_Method'resultStructType = undefined -- TODO: generate accessor values.

get_Method'annotations :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m (Data.Capnp.Untyped.ListOf b (Annotation b))
get_Method'annotations = undefined -- TODO: generate accessor values.

get_Method'paramBrand :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m (Brand b)
get_Method'paramBrand = undefined -- TODO: generate accessor values.

get_Method'resultBrand :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m (Brand b)
get_Method'resultBrand = undefined -- TODO: generate accessor values.

get_Method'implicitParameters :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m (Data.Capnp.Untyped.ListOf b (Node'Parameter b))
get_Method'implicitParameters = undefined -- TODO: generate accessor values.

newtype Enumerant b = Enumerant (Data.Capnp.Untyped.Struct b)

get_Enumerant'name :: Data.Capnp.Untyped.ReadCtx m b => Enumerant b -> m (Data.Capnp.BuiltinTypes.Text b)
get_Enumerant'name = undefined -- TODO: generate accessor values.

get_Enumerant'codeOrder :: Data.Capnp.Untyped.ReadCtx m b => Enumerant b -> m Word16
get_Enumerant'codeOrder = undefined -- TODO: generate accessor values.

get_Enumerant'annotations :: Data.Capnp.Untyped.ReadCtx m b => Enumerant b -> m (Data.Capnp.Untyped.ListOf b (Annotation b))
get_Enumerant'annotations = undefined -- TODO: generate accessor values.

newtype Field b = Field (Data.Capnp.Untyped.Struct b)

get_Field''name :: Data.Capnp.Untyped.ReadCtx m b => Field b -> m (Data.Capnp.BuiltinTypes.Text b)
get_Field''name = undefined -- TODO: generate accessor values.

get_Field''codeOrder :: Data.Capnp.Untyped.ReadCtx m b => Field b -> m Word16
get_Field''codeOrder = undefined -- TODO: generate accessor values.

get_Field''annotations :: Data.Capnp.Untyped.ReadCtx m b => Field b -> m (Data.Capnp.Untyped.ListOf b (Annotation b))
get_Field''annotations = undefined -- TODO: generate accessor values.

get_Field''discriminantValue :: Data.Capnp.Untyped.ReadCtx m b => Field b -> m Word16
get_Field''discriminantValue = undefined -- TODO: generate accessor values.

get_Field''ordinal :: Data.Capnp.Untyped.ReadCtx m b => Field b -> m (Field'ordinal b)
get_Field''ordinal = undefined -- TODO: generate accessor values.

get_Field''union' :: Data.Capnp.Untyped.ReadCtx m b => Field b -> m (Field' b)
get_Field''union' = undefined -- TODO: generate accessor values.

data Field' b
    = Field'slot (Field'slot'group' b)
    | Field'group (Field'group'group' b)
    | Field'unknown' Word16
newtype Field'slot'group' b = Field'slot'group' (Data.Capnp.Untyped.Struct b)

get_Field'slot'offset :: Data.Capnp.Untyped.ReadCtx m b => Field'slot'group' b -> m Word32
get_Field'slot'offset = undefined -- TODO: generate accessor values.

get_Field'slot'type_ :: Data.Capnp.Untyped.ReadCtx m b => Field'slot'group' b -> m (Type b)
get_Field'slot'type_ = undefined -- TODO: generate accessor values.

get_Field'slot'defaultValue :: Data.Capnp.Untyped.ReadCtx m b => Field'slot'group' b -> m (Value b)
get_Field'slot'defaultValue = undefined -- TODO: generate accessor values.

get_Field'slot'hadExplicitDefault :: Data.Capnp.Untyped.ReadCtx m b => Field'slot'group' b -> m Bool
get_Field'slot'hadExplicitDefault = undefined -- TODO: generate accessor values.

newtype Field'group'group' b = Field'group'group' (Data.Capnp.Untyped.Struct b)

get_Field'group'typeId :: Data.Capnp.Untyped.ReadCtx m b => Field'group'group' b -> m Word64
get_Field'group'typeId = undefined -- TODO: generate accessor values.


newtype Superclass b = Superclass (Data.Capnp.Untyped.Struct b)

get_Superclass'id :: Data.Capnp.Untyped.ReadCtx m b => Superclass b -> m Word64
get_Superclass'id = undefined -- TODO: generate accessor values.

get_Superclass'brand :: Data.Capnp.Untyped.ReadCtx m b => Superclass b -> m (Brand b)
get_Superclass'brand = undefined -- TODO: generate accessor values.

newtype Brand'Scope b = Brand'Scope (Data.Capnp.Untyped.Struct b)

get_Brand'Scope''scopeId :: Data.Capnp.Untyped.ReadCtx m b => Brand'Scope b -> m Word64
get_Brand'Scope''scopeId = undefined -- TODO: generate accessor values.

get_Brand'Scope''union' :: Data.Capnp.Untyped.ReadCtx m b => Brand'Scope b -> m (Brand'Scope' b)
get_Brand'Scope''union' = undefined -- TODO: generate accessor values.

data Brand'Scope' b
    = Brand'Scope'bind (Data.Capnp.Untyped.ListOf b (Brand'Binding b))
    | Brand'Scope'inherit
    | Brand'Scope'unknown' Word16



newtype CodeGeneratorRequest'RequestedFile'Import b = CodeGeneratorRequest'RequestedFile'Import (Data.Capnp.Untyped.Struct b)

get_CodeGeneratorRequest'RequestedFile'Import'id :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest'RequestedFile'Import b -> m Word64
get_CodeGeneratorRequest'RequestedFile'Import'id = undefined -- TODO: generate accessor values.

get_CodeGeneratorRequest'RequestedFile'Import'name :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest'RequestedFile'Import b -> m (Data.Capnp.BuiltinTypes.Text b)
get_CodeGeneratorRequest'RequestedFile'Import'name = undefined -- TODO: generate accessor values.

newtype Node'Parameter b = Node'Parameter (Data.Capnp.Untyped.Struct b)

get_Node'Parameter'name :: Data.Capnp.Untyped.ReadCtx m b => Node'Parameter b -> m (Data.Capnp.BuiltinTypes.Text b)
get_Node'Parameter'name = undefined -- TODO: generate accessor values.

data Field'ordinal b
    = Field'ordinal'implicit
    | Field'ordinal'explicit Word16
    | Field'ordinal'unknown' Word16



newtype CodeGeneratorRequest b = CodeGeneratorRequest (Data.Capnp.Untyped.Struct b)

get_CodeGeneratorRequest'nodes :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest b -> m (Data.Capnp.Untyped.ListOf b (Node b))
get_CodeGeneratorRequest'nodes = undefined -- TODO: generate accessor values.

get_CodeGeneratorRequest'requestedFiles :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest b -> m (Data.Capnp.Untyped.ListOf b (CodeGeneratorRequest'RequestedFile b))
get_CodeGeneratorRequest'requestedFiles = undefined -- TODO: generate accessor values.

get_CodeGeneratorRequest'capnpVersion :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest b -> m (CapnpVersion b)
get_CodeGeneratorRequest'capnpVersion = undefined -- TODO: generate accessor values.

data Type'anyPointer b
    = Type'anyPointer'unconstrained (Type'anyPointer'unconstrained'group' b)
    | Type'anyPointer'parameter (Type'anyPointer'parameter'group' b)
    | Type'anyPointer'implicitMethodParameter (Type'anyPointer'implicitMethodParameter'group' b)
    | Type'anyPointer'unknown' Word16
newtype Type'anyPointer'unconstrained'group' b = Type'anyPointer'unconstrained'group' (Data.Capnp.Untyped.Struct b)

get_Type'anyPointer'unconstrained'union' :: Data.Capnp.Untyped.ReadCtx m b => Type'anyPointer'unconstrained'group' b -> m (Type'anyPointer'unconstrained b)
get_Type'anyPointer'unconstrained'union' = undefined -- TODO: generate accessor values.

newtype Type'anyPointer'parameter'group' b = Type'anyPointer'parameter'group' (Data.Capnp.Untyped.Struct b)

get_Type'anyPointer'parameter'scopeId :: Data.Capnp.Untyped.ReadCtx m b => Type'anyPointer'parameter'group' b -> m Word64
get_Type'anyPointer'parameter'scopeId = undefined -- TODO: generate accessor values.

get_Type'anyPointer'parameter'parameterIndex :: Data.Capnp.Untyped.ReadCtx m b => Type'anyPointer'parameter'group' b -> m Word16
get_Type'anyPointer'parameter'parameterIndex = undefined -- TODO: generate accessor values.

newtype Type'anyPointer'implicitMethodParameter'group' b = Type'anyPointer'implicitMethodParameter'group' (Data.Capnp.Untyped.Struct b)

get_Type'anyPointer'implicitMethodParameter'parameterIndex :: Data.Capnp.Untyped.ReadCtx m b => Type'anyPointer'implicitMethodParameter'group' b -> m Word16
get_Type'anyPointer'implicitMethodParameter'parameterIndex = undefined -- TODO: generate accessor values.


data Brand'Binding b
    = Brand'Binding'unbound
    | Brand'Binding'type_ (Type b)
    | Brand'Binding'unknown' Word16



data Value b
    = Value'void
    | Value'bool Bool
    | Value'int8 Int8
    | Value'int16 Int16
    | Value'int32 Int32
    | Value'int64 Int64
    | Value'uint8 Word8
    | Value'uint16 Word16
    | Value'uint32 Word32
    | Value'uint64 Word64
    | Value'float32 Float
    | Value'float64 Double
    | Value'text (Data.Capnp.BuiltinTypes.Text b)
    | Value'data_ (Data.Capnp.BuiltinTypes.Data b)
    | Value'list (Maybe (Data.Capnp.Untyped.Ptr b))
    | Value'enum Word16
    | Value'struct (Maybe (Data.Capnp.Untyped.Ptr b))
    | Value'interface
    | Value'anyPointer (Maybe (Data.Capnp.Untyped.Ptr b))
    | Value'unknown' Word16




















newtype CodeGeneratorRequest'RequestedFile b = CodeGeneratorRequest'RequestedFile (Data.Capnp.Untyped.Struct b)

get_CodeGeneratorRequest'RequestedFile'id :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest'RequestedFile b -> m Word64
get_CodeGeneratorRequest'RequestedFile'id = undefined -- TODO: generate accessor values.

get_CodeGeneratorRequest'RequestedFile'filename :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest'RequestedFile b -> m (Data.Capnp.BuiltinTypes.Text b)
get_CodeGeneratorRequest'RequestedFile'filename = undefined -- TODO: generate accessor values.

get_CodeGeneratorRequest'RequestedFile'imports :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest'RequestedFile b -> m (Data.Capnp.Untyped.ListOf b (CodeGeneratorRequest'RequestedFile'Import b))
get_CodeGeneratorRequest'RequestedFile'imports = undefined -- TODO: generate accessor values.

data Type b
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
    | Type'list (Type'list'group' b)
    | Type'enum (Type'enum'group' b)
    | Type'struct (Type'struct'group' b)
    | Type'interface (Type'interface'group' b)
    | Type'anyPointer (Type'anyPointer'group' b)
    | Type'unknown' Word16














newtype Type'list'group' b = Type'list'group' (Data.Capnp.Untyped.Struct b)

get_Type'list'elementType :: Data.Capnp.Untyped.ReadCtx m b => Type'list'group' b -> m (Type b)
get_Type'list'elementType = undefined -- TODO: generate accessor values.

newtype Type'enum'group' b = Type'enum'group' (Data.Capnp.Untyped.Struct b)

get_Type'enum'typeId :: Data.Capnp.Untyped.ReadCtx m b => Type'enum'group' b -> m Word64
get_Type'enum'typeId = undefined -- TODO: generate accessor values.

get_Type'enum'brand :: Data.Capnp.Untyped.ReadCtx m b => Type'enum'group' b -> m (Brand b)
get_Type'enum'brand = undefined -- TODO: generate accessor values.

newtype Type'struct'group' b = Type'struct'group' (Data.Capnp.Untyped.Struct b)

get_Type'struct'typeId :: Data.Capnp.Untyped.ReadCtx m b => Type'struct'group' b -> m Word64
get_Type'struct'typeId = undefined -- TODO: generate accessor values.

get_Type'struct'brand :: Data.Capnp.Untyped.ReadCtx m b => Type'struct'group' b -> m (Brand b)
get_Type'struct'brand = undefined -- TODO: generate accessor values.

newtype Type'interface'group' b = Type'interface'group' (Data.Capnp.Untyped.Struct b)

get_Type'interface'typeId :: Data.Capnp.Untyped.ReadCtx m b => Type'interface'group' b -> m Word64
get_Type'interface'typeId = undefined -- TODO: generate accessor values.

get_Type'interface'brand :: Data.Capnp.Untyped.ReadCtx m b => Type'interface'group' b -> m (Brand b)
get_Type'interface'brand = undefined -- TODO: generate accessor values.

newtype Type'anyPointer'group' b = Type'anyPointer'group' (Data.Capnp.Untyped.Struct b)

get_Type'anyPointer'union' :: Data.Capnp.Untyped.ReadCtx m b => Type'anyPointer'group' b -> m (Type'anyPointer b)
get_Type'anyPointer'union' = undefined -- TODO: generate accessor values.


data ElementSize b
    = ElementSize'empty
    | ElementSize'bit
    | ElementSize'byte
    | ElementSize'twoBytes
    | ElementSize'fourBytes
    | ElementSize'eightBytes
    | ElementSize'pointer
    | ElementSize'inlineComposite
    | ElementSize'unknown' Word16
newtype CapnpVersion b = CapnpVersion (Data.Capnp.Untyped.Struct b)

get_CapnpVersion'major :: Data.Capnp.Untyped.ReadCtx m b => CapnpVersion b -> m Word16
get_CapnpVersion'major = undefined -- TODO: generate accessor values.

get_CapnpVersion'minor :: Data.Capnp.Untyped.ReadCtx m b => CapnpVersion b -> m Word8
get_CapnpVersion'minor = undefined -- TODO: generate accessor values.

get_CapnpVersion'micro :: Data.Capnp.Untyped.ReadCtx m b => CapnpVersion b -> m Word8
get_CapnpVersion'micro = undefined -- TODO: generate accessor values.

newtype Node'NestedNode b = Node'NestedNode (Data.Capnp.Untyped.Struct b)

get_Node'NestedNode'name :: Data.Capnp.Untyped.ReadCtx m b => Node'NestedNode b -> m (Data.Capnp.BuiltinTypes.Text b)
get_Node'NestedNode'name = undefined -- TODO: generate accessor values.

get_Node'NestedNode'id :: Data.Capnp.Untyped.ReadCtx m b => Node'NestedNode b -> m Word64
get_Node'NestedNode'id = undefined -- TODO: generate accessor values.

newtype Node b = Node (Data.Capnp.Untyped.Struct b)

get_Node''id :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m Word64
get_Node''id = undefined -- TODO: generate accessor values.

get_Node''displayName :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m (Data.Capnp.BuiltinTypes.Text b)
get_Node''displayName = undefined -- TODO: generate accessor values.

get_Node''displayNamePrefixLength :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m Word32
get_Node''displayNamePrefixLength = undefined -- TODO: generate accessor values.

get_Node''scopeId :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m Word64
get_Node''scopeId = undefined -- TODO: generate accessor values.

get_Node''nestedNodes :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m (Data.Capnp.Untyped.ListOf b (Node'NestedNode b))
get_Node''nestedNodes = undefined -- TODO: generate accessor values.

get_Node''annotations :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m (Data.Capnp.Untyped.ListOf b (Annotation b))
get_Node''annotations = undefined -- TODO: generate accessor values.

get_Node''parameters :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m (Data.Capnp.Untyped.ListOf b (Node'Parameter b))
get_Node''parameters = undefined -- TODO: generate accessor values.

get_Node''isGeneric :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m Bool
get_Node''isGeneric = undefined -- TODO: generate accessor values.

get_Node''union' :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m (Node' b)
get_Node''union' = undefined -- TODO: generate accessor values.

data Node' b
    = Node'file
    | Node'struct (Node'struct'group' b)
    | Node'enum (Node'enum'group' b)
    | Node'interface (Node'interface'group' b)
    | Node'const (Node'const'group' b)
    | Node'annotation (Node'annotation'group' b)
    | Node'unknown' Word16

newtype Node'struct'group' b = Node'struct'group' (Data.Capnp.Untyped.Struct b)

get_Node'struct'dataWordCount :: Data.Capnp.Untyped.ReadCtx m b => Node'struct'group' b -> m Word16
get_Node'struct'dataWordCount = undefined -- TODO: generate accessor values.

get_Node'struct'pointerCount :: Data.Capnp.Untyped.ReadCtx m b => Node'struct'group' b -> m Word16
get_Node'struct'pointerCount = undefined -- TODO: generate accessor values.

get_Node'struct'preferredListEncoding :: Data.Capnp.Untyped.ReadCtx m b => Node'struct'group' b -> m (ElementSize b)
get_Node'struct'preferredListEncoding = undefined -- TODO: generate accessor values.

get_Node'struct'isGroup :: Data.Capnp.Untyped.ReadCtx m b => Node'struct'group' b -> m Bool
get_Node'struct'isGroup = undefined -- TODO: generate accessor values.

get_Node'struct'discriminantCount :: Data.Capnp.Untyped.ReadCtx m b => Node'struct'group' b -> m Word16
get_Node'struct'discriminantCount = undefined -- TODO: generate accessor values.

get_Node'struct'discriminantOffset :: Data.Capnp.Untyped.ReadCtx m b => Node'struct'group' b -> m Word32
get_Node'struct'discriminantOffset = undefined -- TODO: generate accessor values.

get_Node'struct'fields :: Data.Capnp.Untyped.ReadCtx m b => Node'struct'group' b -> m (Data.Capnp.Untyped.ListOf b (Field b))
get_Node'struct'fields = undefined -- TODO: generate accessor values.

newtype Node'enum'group' b = Node'enum'group' (Data.Capnp.Untyped.Struct b)

get_Node'enum'enumerants :: Data.Capnp.Untyped.ReadCtx m b => Node'enum'group' b -> m (Data.Capnp.Untyped.ListOf b (Enumerant b))
get_Node'enum'enumerants = undefined -- TODO: generate accessor values.

newtype Node'interface'group' b = Node'interface'group' (Data.Capnp.Untyped.Struct b)

get_Node'interface'methods :: Data.Capnp.Untyped.ReadCtx m b => Node'interface'group' b -> m (Data.Capnp.Untyped.ListOf b (Method b))
get_Node'interface'methods = undefined -- TODO: generate accessor values.

get_Node'interface'superclasses :: Data.Capnp.Untyped.ReadCtx m b => Node'interface'group' b -> m (Data.Capnp.Untyped.ListOf b (Superclass b))
get_Node'interface'superclasses = undefined -- TODO: generate accessor values.

newtype Node'const'group' b = Node'const'group' (Data.Capnp.Untyped.Struct b)

get_Node'const'type_ :: Data.Capnp.Untyped.ReadCtx m b => Node'const'group' b -> m (Type b)
get_Node'const'type_ = undefined -- TODO: generate accessor values.

get_Node'const'value :: Data.Capnp.Untyped.ReadCtx m b => Node'const'group' b -> m (Value b)
get_Node'const'value = undefined -- TODO: generate accessor values.

newtype Node'annotation'group' b = Node'annotation'group' (Data.Capnp.Untyped.Struct b)

get_Node'annotation'type_ :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m (Type b)
get_Node'annotation'type_ = undefined -- TODO: generate accessor values.

get_Node'annotation'targetsFile :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsFile = undefined -- TODO: generate accessor values.

get_Node'annotation'targetsConst :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsConst = undefined -- TODO: generate accessor values.

get_Node'annotation'targetsEnum :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsEnum = undefined -- TODO: generate accessor values.

get_Node'annotation'targetsEnumerant :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsEnumerant = undefined -- TODO: generate accessor values.

get_Node'annotation'targetsStruct :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsStruct = undefined -- TODO: generate accessor values.

get_Node'annotation'targetsField :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsField = undefined -- TODO: generate accessor values.

get_Node'annotation'targetsUnion :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsUnion = undefined -- TODO: generate accessor values.

get_Node'annotation'targetsGroup :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsGroup = undefined -- TODO: generate accessor values.

get_Node'annotation'targetsInterface :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsInterface = undefined -- TODO: generate accessor values.

get_Node'annotation'targetsMethod :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsMethod = undefined -- TODO: generate accessor values.

get_Node'annotation'targetsParam :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsParam = undefined -- TODO: generate accessor values.

get_Node'annotation'targetsAnnotation :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsAnnotation = undefined -- TODO: generate accessor values.


newtype Annotation b = Annotation (Data.Capnp.Untyped.Struct b)

get_Annotation'id :: Data.Capnp.Untyped.ReadCtx m b => Annotation b -> m Word64
get_Annotation'id = undefined -- TODO: generate accessor values.

get_Annotation'value :: Data.Capnp.Untyped.ReadCtx m b => Annotation b -> m (Value b)
get_Annotation'value = undefined -- TODO: generate accessor values.

get_Annotation'brand :: Data.Capnp.Untyped.ReadCtx m b => Annotation b -> m (Brand b)
get_Annotation'brand = undefined -- TODO: generate accessor values.
