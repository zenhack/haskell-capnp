{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.Capnp.ById.Xa93fc509624c72d9 where

-- generated from /usr/include/capnp/schema.capnp

import Data.Int
import Data.Word
import qualified Data.Bits

import qualified Data.Capnp.BuiltinTypes
import qualified Data.Capnp.TraversalLimit
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
get_Brand'scopes (Brand struct) = undefined -- TODO: handle pointer fields
newtype Method b = Method (Data.Capnp.Untyped.Struct b)

get_Method'name :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m (Data.Capnp.BuiltinTypes.Text b)
get_Method'name (Method struct) = undefined -- TODO: handle pointer fields
get_Method'codeOrder :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m Word16
get_Method'codeOrder (Method struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_Method'paramStructType :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m Word64
get_Method'paramStructType (Method struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Method'resultStructType :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m Word64
get_Method'resultStructType (Method struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 2 struct)

get_Method'annotations :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m (Data.Capnp.Untyped.ListOf b (Annotation b))
get_Method'annotations (Method struct) = undefined -- TODO: handle pointer fields
get_Method'paramBrand :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m (Brand b)
get_Method'paramBrand (Method struct) = undefined -- TODO: handle pointer fields
get_Method'resultBrand :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m (Brand b)
get_Method'resultBrand (Method struct) = undefined -- TODO: handle pointer fields
get_Method'implicitParameters :: Data.Capnp.Untyped.ReadCtx m b => Method b -> m (Data.Capnp.Untyped.ListOf b (Node'Parameter b))
get_Method'implicitParameters (Method struct) = undefined -- TODO: handle pointer fields
newtype Enumerant b = Enumerant (Data.Capnp.Untyped.Struct b)

get_Enumerant'name :: Data.Capnp.Untyped.ReadCtx m b => Enumerant b -> m (Data.Capnp.BuiltinTypes.Text b)
get_Enumerant'name (Enumerant struct) = undefined -- TODO: handle pointer fields
get_Enumerant'codeOrder :: Data.Capnp.Untyped.ReadCtx m b => Enumerant b -> m Word16
get_Enumerant'codeOrder (Enumerant struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_Enumerant'annotations :: Data.Capnp.Untyped.ReadCtx m b => Enumerant b -> m (Data.Capnp.Untyped.ListOf b (Annotation b))
get_Enumerant'annotations (Enumerant struct) = undefined -- TODO: handle pointer fields
newtype Field b = Field (Data.Capnp.Untyped.Struct b)

get_Field''name :: Data.Capnp.Untyped.ReadCtx m b => Field b -> m (Data.Capnp.BuiltinTypes.Text b)
get_Field''name (Field struct) = undefined -- TODO: handle pointer fields
get_Field''codeOrder :: Data.Capnp.Untyped.ReadCtx m b => Field b -> m Word16
get_Field''codeOrder (Field struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_Field''annotations :: Data.Capnp.Untyped.ReadCtx m b => Field b -> m (Data.Capnp.Untyped.ListOf b (Annotation b))
get_Field''annotations (Field struct) = undefined -- TODO: handle pointer fields
get_Field''discriminantValue :: Data.Capnp.Untyped.ReadCtx m b => Field b -> m Word16
get_Field''discriminantValue (Field struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 65535
    . (`Data.Bits.shiftR` 16)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_Field''ordinal :: Data.Capnp.Untyped.ReadCtx m b => Field b -> m (Field'ordinal b)
get_Field''ordinal (Field struct) = undefined -- TODO: handle groups/anonymous union fields
get_Field''union' :: Data.Capnp.Untyped.ReadCtx m b => Field b -> m (Field' b)
get_Field''union' (Field struct) = undefined -- TODO: handle groups/anonymous union fields
data Field' b
    = Field'slot (Field'slot'group' b)
    | Field'group (Field'group'group' b)
    | Field'unknown' Word16
newtype Field'slot'group' b = Field'slot'group' (Data.Capnp.Untyped.Struct b)

get_Field'slot'offset :: Data.Capnp.Untyped.ReadCtx m b => Field'slot'group' b -> m Word32
get_Field'slot'offset (Field'slot'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 32)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_Field'slot'type_ :: Data.Capnp.Untyped.ReadCtx m b => Field'slot'group' b -> m (Type b)
get_Field'slot'type_ (Field'slot'group' struct) = undefined -- TODO: handle pointer fields
get_Field'slot'defaultValue :: Data.Capnp.Untyped.ReadCtx m b => Field'slot'group' b -> m (Value b)
get_Field'slot'defaultValue (Field'slot'group' struct) = undefined -- TODO: handle pointer fields
get_Field'slot'hadExplicitDefault :: Data.Capnp.Untyped.ReadCtx m b => Field'slot'group' b -> m Bool
get_Field'slot'hadExplicitDefault (Field'slot'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 2 struct)

newtype Field'group'group' b = Field'group'group' (Data.Capnp.Untyped.Struct b)

get_Field'group'typeId :: Data.Capnp.Untyped.ReadCtx m b => Field'group'group' b -> m Word64
get_Field'group'typeId (Field'group'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 2 struct)


newtype Superclass b = Superclass (Data.Capnp.Untyped.Struct b)

get_Superclass'id :: Data.Capnp.Untyped.ReadCtx m b => Superclass b -> m Word64
get_Superclass'id (Superclass struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_Superclass'brand :: Data.Capnp.Untyped.ReadCtx m b => Superclass b -> m (Brand b)
get_Superclass'brand (Superclass struct) = undefined -- TODO: handle pointer fields
newtype Brand'Scope b = Brand'Scope (Data.Capnp.Untyped.Struct b)

get_Brand'Scope''scopeId :: Data.Capnp.Untyped.ReadCtx m b => Brand'Scope b -> m Word64
get_Brand'Scope''scopeId (Brand'Scope struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_Brand'Scope''union' :: Data.Capnp.Untyped.ReadCtx m b => Brand'Scope b -> m (Brand'Scope' b)
get_Brand'Scope''union' (Brand'Scope struct) = undefined -- TODO: handle groups/anonymous union fields
data Brand'Scope' b
    = Brand'Scope'bind (Data.Capnp.Untyped.ListOf b (Brand'Binding b))
    | Brand'Scope'inherit
    | Brand'Scope'unknown' Word16



newtype CodeGeneratorRequest'RequestedFile'Import b = CodeGeneratorRequest'RequestedFile'Import (Data.Capnp.Untyped.Struct b)

get_CodeGeneratorRequest'RequestedFile'Import'id :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest'RequestedFile'Import b -> m Word64
get_CodeGeneratorRequest'RequestedFile'Import'id (CodeGeneratorRequest'RequestedFile'Import struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_CodeGeneratorRequest'RequestedFile'Import'name :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest'RequestedFile'Import b -> m (Data.Capnp.BuiltinTypes.Text b)
get_CodeGeneratorRequest'RequestedFile'Import'name (CodeGeneratorRequest'RequestedFile'Import struct) = undefined -- TODO: handle pointer fields
newtype Node'Parameter b = Node'Parameter (Data.Capnp.Untyped.Struct b)

get_Node'Parameter'name :: Data.Capnp.Untyped.ReadCtx m b => Node'Parameter b -> m (Data.Capnp.BuiltinTypes.Text b)
get_Node'Parameter'name (Node'Parameter struct) = undefined -- TODO: handle pointer fields
data Field'ordinal b
    = Field'ordinal'implicit
    | Field'ordinal'explicit Word16
    | Field'ordinal'unknown' Word16



newtype CodeGeneratorRequest b = CodeGeneratorRequest (Data.Capnp.Untyped.Struct b)

get_CodeGeneratorRequest'nodes :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest b -> m (Data.Capnp.Untyped.ListOf b (Node b))
get_CodeGeneratorRequest'nodes (CodeGeneratorRequest struct) = undefined -- TODO: handle pointer fields
get_CodeGeneratorRequest'requestedFiles :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest b -> m (Data.Capnp.Untyped.ListOf b (CodeGeneratorRequest'RequestedFile b))
get_CodeGeneratorRequest'requestedFiles (CodeGeneratorRequest struct) = undefined -- TODO: handle pointer fields
get_CodeGeneratorRequest'capnpVersion :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest b -> m (CapnpVersion b)
get_CodeGeneratorRequest'capnpVersion (CodeGeneratorRequest struct) = undefined -- TODO: handle pointer fields
data Type'anyPointer b
    = Type'anyPointer'unconstrained (Type'anyPointer'unconstrained'group' b)
    | Type'anyPointer'parameter (Type'anyPointer'parameter'group' b)
    | Type'anyPointer'implicitMethodParameter (Type'anyPointer'implicitMethodParameter'group' b)
    | Type'anyPointer'unknown' Word16
newtype Type'anyPointer'unconstrained'group' b = Type'anyPointer'unconstrained'group' (Data.Capnp.Untyped.Struct b)

get_Type'anyPointer'unconstrained'union' :: Data.Capnp.Untyped.ReadCtx m b => Type'anyPointer'unconstrained'group' b -> m (Type'anyPointer'unconstrained b)
get_Type'anyPointer'unconstrained'union' (Type'anyPointer'unconstrained'group' struct) = undefined -- TODO: handle groups/anonymous union fields
newtype Type'anyPointer'parameter'group' b = Type'anyPointer'parameter'group' (Data.Capnp.Untyped.Struct b)

get_Type'anyPointer'parameter'scopeId :: Data.Capnp.Untyped.ReadCtx m b => Type'anyPointer'parameter'group' b -> m Word64
get_Type'anyPointer'parameter'scopeId (Type'anyPointer'parameter'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 2 struct)

get_Type'anyPointer'parameter'parameterIndex :: Data.Capnp.Untyped.ReadCtx m b => Type'anyPointer'parameter'group' b -> m Word16
get_Type'anyPointer'parameter'parameterIndex (Type'anyPointer'parameter'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 16)
    )
    (Data.Capnp.Untyped.getData 1 struct)

newtype Type'anyPointer'implicitMethodParameter'group' b = Type'anyPointer'implicitMethodParameter'group' (Data.Capnp.Untyped.Struct b)

get_Type'anyPointer'implicitMethodParameter'parameterIndex :: Data.Capnp.Untyped.ReadCtx m b => Type'anyPointer'implicitMethodParameter'group' b -> m Word16
get_Type'anyPointer'implicitMethodParameter'parameterIndex (Type'anyPointer'implicitMethodParameter'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 16)
    )
    (Data.Capnp.Untyped.getData 1 struct)


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
get_CodeGeneratorRequest'RequestedFile'id (CodeGeneratorRequest'RequestedFile struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_CodeGeneratorRequest'RequestedFile'filename :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest'RequestedFile b -> m (Data.Capnp.BuiltinTypes.Text b)
get_CodeGeneratorRequest'RequestedFile'filename (CodeGeneratorRequest'RequestedFile struct) = undefined -- TODO: handle pointer fields
get_CodeGeneratorRequest'RequestedFile'imports :: Data.Capnp.Untyped.ReadCtx m b => CodeGeneratorRequest'RequestedFile b -> m (Data.Capnp.Untyped.ListOf b (CodeGeneratorRequest'RequestedFile'Import b))
get_CodeGeneratorRequest'RequestedFile'imports (CodeGeneratorRequest'RequestedFile struct) = undefined -- TODO: handle pointer fields
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
get_Type'list'elementType (Type'list'group' struct) = undefined -- TODO: handle pointer fields
newtype Type'enum'group' b = Type'enum'group' (Data.Capnp.Untyped.Struct b)

get_Type'enum'typeId :: Data.Capnp.Untyped.ReadCtx m b => Type'enum'group' b -> m Word64
get_Type'enum'typeId (Type'enum'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Type'enum'brand :: Data.Capnp.Untyped.ReadCtx m b => Type'enum'group' b -> m (Brand b)
get_Type'enum'brand (Type'enum'group' struct) = undefined -- TODO: handle pointer fields
newtype Type'struct'group' b = Type'struct'group' (Data.Capnp.Untyped.Struct b)

get_Type'struct'typeId :: Data.Capnp.Untyped.ReadCtx m b => Type'struct'group' b -> m Word64
get_Type'struct'typeId (Type'struct'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Type'struct'brand :: Data.Capnp.Untyped.ReadCtx m b => Type'struct'group' b -> m (Brand b)
get_Type'struct'brand (Type'struct'group' struct) = undefined -- TODO: handle pointer fields
newtype Type'interface'group' b = Type'interface'group' (Data.Capnp.Untyped.Struct b)

get_Type'interface'typeId :: Data.Capnp.Untyped.ReadCtx m b => Type'interface'group' b -> m Word64
get_Type'interface'typeId (Type'interface'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Type'interface'brand :: Data.Capnp.Untyped.ReadCtx m b => Type'interface'group' b -> m (Brand b)
get_Type'interface'brand (Type'interface'group' struct) = undefined -- TODO: handle pointer fields
newtype Type'anyPointer'group' b = Type'anyPointer'group' (Data.Capnp.Untyped.Struct b)

get_Type'anyPointer'union' :: Data.Capnp.Untyped.ReadCtx m b => Type'anyPointer'group' b -> m (Type'anyPointer b)
get_Type'anyPointer'union' (Type'anyPointer'group' struct) = undefined -- TODO: handle groups/anonymous union fields

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
instance Enum (ElementSize b) where
    toEnum = Data.Capnp.BuiltinTypes.fromWord . fromIntegral
    fromEnum = fromIntegral . Data.Capnp.BuiltinTypes.toWord


instance Data.Capnp.BuiltinTypes.IsWord (ElementSize b) where
    fromWord 7 = ElementSize'inlineComposite
    fromWord 6 = ElementSize'pointer
    fromWord 5 = ElementSize'eightBytes
    fromWord 4 = ElementSize'fourBytes
    fromWord 3 = ElementSize'twoBytes
    fromWord 2 = ElementSize'byte
    fromWord 1 = ElementSize'bit
    fromWord 0 = ElementSize'empty
    fromWord tag = ElementSize'unknown' (fromIntegral tag)
    toWord ElementSize'inlineComposite = 7
    toWord ElementSize'pointer = 6
    toWord ElementSize'eightBytes = 5
    toWord ElementSize'fourBytes = 4
    toWord ElementSize'twoBytes = 3
    toWord ElementSize'byte = 2
    toWord ElementSize'bit = 1
    toWord ElementSize'empty = 0
    toWord (ElementSize'unknown' tag) = fromIntegral tag

newtype CapnpVersion b = CapnpVersion (Data.Capnp.Untyped.Struct b)

get_CapnpVersion'major :: Data.Capnp.Untyped.ReadCtx m b => CapnpVersion b -> m Word16
get_CapnpVersion'major (CapnpVersion struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_CapnpVersion'minor :: Data.Capnp.Untyped.ReadCtx m b => CapnpVersion b -> m Word8
get_CapnpVersion'minor (CapnpVersion struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 16)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_CapnpVersion'micro :: Data.Capnp.Untyped.ReadCtx m b => CapnpVersion b -> m Word8
get_CapnpVersion'micro (CapnpVersion struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 24)
    )
    (Data.Capnp.Untyped.getData 0 struct)

newtype Node'NestedNode b = Node'NestedNode (Data.Capnp.Untyped.Struct b)

get_Node'NestedNode'name :: Data.Capnp.Untyped.ReadCtx m b => Node'NestedNode b -> m (Data.Capnp.BuiltinTypes.Text b)
get_Node'NestedNode'name (Node'NestedNode struct) = undefined -- TODO: handle pointer fields
get_Node'NestedNode'id :: Data.Capnp.Untyped.ReadCtx m b => Node'NestedNode b -> m Word64
get_Node'NestedNode'id (Node'NestedNode struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)

newtype Node b = Node (Data.Capnp.Untyped.Struct b)

get_Node''id :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m Word64
get_Node''id (Node struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_Node''displayName :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m (Data.Capnp.BuiltinTypes.Text b)
get_Node''displayName (Node struct) = undefined -- TODO: handle pointer fields
get_Node''displayNamePrefixLength :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m Word32
get_Node''displayNamePrefixLength (Node struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Node''scopeId :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m Word64
get_Node''scopeId (Node struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 2 struct)

get_Node''nestedNodes :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m (Data.Capnp.Untyped.ListOf b (Node'NestedNode b))
get_Node''nestedNodes (Node struct) = undefined -- TODO: handle pointer fields
get_Node''annotations :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m (Data.Capnp.Untyped.ListOf b (Annotation b))
get_Node''annotations (Node struct) = undefined -- TODO: handle pointer fields
get_Node''parameters :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m (Data.Capnp.Untyped.ListOf b (Node'Parameter b))
get_Node''parameters (Node struct) = undefined -- TODO: handle pointer fields
get_Node''isGeneric :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m Bool
get_Node''isGeneric (Node struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 32)
    )
    (Data.Capnp.Untyped.getData 4 struct)

get_Node''union' :: Data.Capnp.Untyped.ReadCtx m b => Node b -> m (Node' b)
get_Node''union' (Node struct) = undefined -- TODO: handle groups/anonymous union fields
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
get_Node'struct'dataWordCount (Node'struct'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 48)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Node'struct'pointerCount :: Data.Capnp.Untyped.ReadCtx m b => Node'struct'group' b -> m Word16
get_Node'struct'pointerCount (Node'struct'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 3 struct)

get_Node'struct'preferredListEncoding :: Data.Capnp.Untyped.ReadCtx m b => Node'struct'group' b -> m (ElementSize b)
get_Node'struct'preferredListEncoding (Node'struct'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 16)
    )
    (Data.Capnp.Untyped.getData 3 struct)

get_Node'struct'isGroup :: Data.Capnp.Untyped.ReadCtx m b => Node'struct'group' b -> m Bool
get_Node'struct'isGroup (Node'struct'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 32)
    )
    (Data.Capnp.Untyped.getData 3 struct)

get_Node'struct'discriminantCount :: Data.Capnp.Untyped.ReadCtx m b => Node'struct'group' b -> m Word16
get_Node'struct'discriminantCount (Node'struct'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 48)
    )
    (Data.Capnp.Untyped.getData 3 struct)

get_Node'struct'discriminantOffset :: Data.Capnp.Untyped.ReadCtx m b => Node'struct'group' b -> m Word32
get_Node'struct'discriminantOffset (Node'struct'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 4 struct)

get_Node'struct'fields :: Data.Capnp.Untyped.ReadCtx m b => Node'struct'group' b -> m (Data.Capnp.Untyped.ListOf b (Field b))
get_Node'struct'fields (Node'struct'group' struct) = undefined -- TODO: handle pointer fields
newtype Node'enum'group' b = Node'enum'group' (Data.Capnp.Untyped.Struct b)

get_Node'enum'enumerants :: Data.Capnp.Untyped.ReadCtx m b => Node'enum'group' b -> m (Data.Capnp.Untyped.ListOf b (Enumerant b))
get_Node'enum'enumerants (Node'enum'group' struct) = undefined -- TODO: handle pointer fields
newtype Node'interface'group' b = Node'interface'group' (Data.Capnp.Untyped.Struct b)

get_Node'interface'methods :: Data.Capnp.Untyped.ReadCtx m b => Node'interface'group' b -> m (Data.Capnp.Untyped.ListOf b (Method b))
get_Node'interface'methods (Node'interface'group' struct) = undefined -- TODO: handle pointer fields
get_Node'interface'superclasses :: Data.Capnp.Untyped.ReadCtx m b => Node'interface'group' b -> m (Data.Capnp.Untyped.ListOf b (Superclass b))
get_Node'interface'superclasses (Node'interface'group' struct) = undefined -- TODO: handle pointer fields
newtype Node'const'group' b = Node'const'group' (Data.Capnp.Untyped.Struct b)

get_Node'const'type_ :: Data.Capnp.Untyped.ReadCtx m b => Node'const'group' b -> m (Type b)
get_Node'const'type_ (Node'const'group' struct) = undefined -- TODO: handle pointer fields
get_Node'const'value :: Data.Capnp.Untyped.ReadCtx m b => Node'const'group' b -> m (Value b)
get_Node'const'value (Node'const'group' struct) = undefined -- TODO: handle pointer fields
newtype Node'annotation'group' b = Node'annotation'group' (Data.Capnp.Untyped.Struct b)

get_Node'annotation'type_ :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m (Type b)
get_Node'annotation'type_ (Node'annotation'group' struct) = undefined -- TODO: handle pointer fields
get_Node'annotation'targetsFile :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsFile (Node'annotation'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 48)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Node'annotation'targetsConst :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsConst (Node'annotation'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 49)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Node'annotation'targetsEnum :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsEnum (Node'annotation'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 50)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Node'annotation'targetsEnumerant :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsEnumerant (Node'annotation'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 51)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Node'annotation'targetsStruct :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsStruct (Node'annotation'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 52)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Node'annotation'targetsField :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsField (Node'annotation'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 53)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Node'annotation'targetsUnion :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsUnion (Node'annotation'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 54)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Node'annotation'targetsGroup :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsGroup (Node'annotation'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 55)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Node'annotation'targetsInterface :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsInterface (Node'annotation'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 56)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Node'annotation'targetsMethod :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsMethod (Node'annotation'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 57)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Node'annotation'targetsParam :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsParam (Node'annotation'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 58)
    )
    (Data.Capnp.Untyped.getData 1 struct)

get_Node'annotation'targetsAnnotation :: Data.Capnp.Untyped.ReadCtx m b => Node'annotation'group' b -> m Bool
get_Node'annotation'targetsAnnotation (Node'annotation'group' struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 59)
    )
    (Data.Capnp.Untyped.getData 1 struct)


newtype Annotation b = Annotation (Data.Capnp.Untyped.Struct b)

get_Annotation'id :: Data.Capnp.Untyped.ReadCtx m b => Annotation b -> m Word64
get_Annotation'id (Annotation struct) = fmap
    ( Data.Capnp.BuiltinTypes.fromWord
    . Data.Bits.xor 0
    . (`Data.Bits.shiftR` 0)
    )
    (Data.Capnp.Untyped.getData 0 struct)

get_Annotation'value :: Data.Capnp.Untyped.ReadCtx m b => Annotation b -> m (Value b)
get_Annotation'value (Annotation struct) = undefined -- TODO: handle pointer fields
get_Annotation'brand :: Data.Capnp.Untyped.ReadCtx m b => Annotation b -> m (Brand b)
get_Annotation'brand (Annotation struct) = undefined -- TODO: handle pointer fields