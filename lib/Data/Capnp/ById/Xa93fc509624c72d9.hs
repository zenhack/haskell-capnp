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

newtype Method b = Method (Data.Capnp.Untyped.Struct b)

newtype Enumerant b = Enumerant (Data.Capnp.Untyped.Struct b)

newtype Field b = Field (Data.Capnp.Untyped.Struct b)

data Field' b
    = Field'slot (Field'slot'group' b)
    | Field'group (Field'group'group' b)
    | Field'unknown' Word16
newtype Field'slot'group' b = Field'slot'group' (Data.Capnp.Untyped.Struct b)


newtype Field'group'group' b = Field'group'group' (Data.Capnp.Untyped.Struct b)



newtype Superclass b = Superclass (Data.Capnp.Untyped.Struct b)

newtype Brand'Scope b = Brand'Scope (Data.Capnp.Untyped.Struct b)

data Brand'Scope' b
    = Brand'Scope'bind (Data.Capnp.Untyped.ListOf b (Brand'Binding b))
    | Brand'Scope'inherit
    | Brand'Scope'unknown' Word16





newtype CodeGeneratorRequest'RequestedFile'Import b = CodeGeneratorRequest'RequestedFile'Import (Data.Capnp.Untyped.Struct b)

newtype Node'Parameter b = Node'Parameter (Data.Capnp.Untyped.Struct b)

data Field'ordinal b
    = Field'ordinal'implicit
    | Field'ordinal'explicit Word16
    | Field'ordinal'unknown' Word16





newtype CodeGeneratorRequest b = CodeGeneratorRequest (Data.Capnp.Untyped.Struct b)

data Type'anyPointer b
    = Type'anyPointer'unconstrained (Type'anyPointer'unconstrained'group' b)
    | Type'anyPointer'parameter (Type'anyPointer'parameter'group' b)
    | Type'anyPointer'implicitMethodParameter (Type'anyPointer'implicitMethodParameter'group' b)
    | Type'anyPointer'unknown' Word16
newtype Type'anyPointer'unconstrained'group' b = Type'anyPointer'unconstrained'group' (Data.Capnp.Untyped.Struct b)


newtype Type'anyPointer'parameter'group' b = Type'anyPointer'parameter'group' (Data.Capnp.Untyped.Struct b)


newtype Type'anyPointer'implicitMethodParameter'group' b = Type'anyPointer'implicitMethodParameter'group' (Data.Capnp.Untyped.Struct b)



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


newtype Type'enum'group' b = Type'enum'group' (Data.Capnp.Untyped.Struct b)


newtype Type'struct'group' b = Type'struct'group' (Data.Capnp.Untyped.Struct b)


newtype Type'interface'group' b = Type'interface'group' (Data.Capnp.Untyped.Struct b)


newtype Type'anyPointer'group' b = Type'anyPointer'group' (Data.Capnp.Untyped.Struct b)




newtype CapnpVersion b = CapnpVersion (Data.Capnp.Untyped.Struct b)

newtype Node'NestedNode b = Node'NestedNode (Data.Capnp.Untyped.Struct b)

newtype Node b = Node (Data.Capnp.Untyped.Struct b)

data Node' b
    = Node'file
    | Node'struct (Node'struct'group' b)
    | Node'enum (Node'enum'group' b)
    | Node'interface (Node'interface'group' b)
    | Node'const (Node'const'group' b)
    | Node'annotation (Node'annotation'group' b)
    | Node'unknown' Word16


newtype Node'struct'group' b = Node'struct'group' (Data.Capnp.Untyped.Struct b)


newtype Node'enum'group' b = Node'enum'group' (Data.Capnp.Untyped.Struct b)


newtype Node'interface'group' b = Node'interface'group' (Data.Capnp.Untyped.Struct b)


newtype Node'const'group' b = Node'const'group' (Data.Capnp.Untyped.Struct b)


newtype Node'annotation'group' b = Node'annotation'group' (Data.Capnp.Untyped.Struct b)



newtype Annotation b = Annotation (Data.Capnp.Untyped.Struct b)
