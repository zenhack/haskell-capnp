{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Schema where

import Data.Word (Word64, Word32, Word16)
import Data.Text (Text)


type Id = Word64

data Node = Node
  { nodeId :: Id
  , displayName :: Text
  , displayNamePrefixLength :: Word32
  , nodeScopeId :: Id
  , parameters :: [NodeParameter]
  , nestedNodes :: [NestedNode]
  , nodeAnnotations :: [Annotation]
  , nodeType :: NodeType
  }

data NodeType = File
              | StructNode NodeTypeStruct
              | Enum [Enumerant]
              | Interface [Method] [Superclass]
              | Const Type Value
              | NodeTypeAnnotation -- needs fields
              | NodeTypeSourceInfo -- needs fields

data NodeTypeStruct = NodeTypeStruct
  { dataWordCount :: Word16
  , pointerCount :: Word16
  , preferredListEncoding :: ElementSize
  , isGroup :: Bool
  , discriminantCount :: Word16
  , discriminantOffset :: Word32
  , field :: [Field]
  }

data NodeParameter = NodeParameter { nodeParameterName :: Text }

data NestedNode = NestedNode
  { nestedNodeName :: Text
  , nestedNodeId :: Id
  }

noDiscriminant :: Discriminant
noDiscriminant = Discriminant 0xffff

data Field = Field
  { fieldName :: Text
  , fieldCodeOrder :: Word16
  , annotation :: [Annotation]
  , discriminantValue :: Discriminant
  , ordinal :: Maybe Word16
  }

data FieldType = Slot FieldTypeSlot
               | Group Id

newtype Discriminant = Discriminant Word16

data FieldTypeSlot = FieldTypeSlot
  { offset :: Word32
  , slotType :: Type
  , defaultValue :: Value
  , hasdExplicitDefault :: Bool
  }

data Enumerant = Enumerant
  { enumerantName :: Text
  , enumerantCodeOrder :: Word16
  , enumerantImplicitParameters :: [NodeParameter]
  }

data Superclass = Superclass Id Brand

data Method = Method
  { name :: Text
  , codeOrder :: Word16
  , methodImplicitParameters :: [NodeParameter]
  , paramStructType :: Id
  , paramBrand :: Brand
  , resultStructType :: Id
  , resultBrand :: Brand
  , annotations :: [Annotation]
  }

data Type = Type TypeVariant | Untyped AnyPointer

data TypeVariant = Void
                 | Bool
                 | Int8
                 | Int16
                 | Int32
                 | Int64
                 | Float32
                 | Float64
                 | Text
                 | Data
                 | List Type
                 | EnumType Id Brand
                 | Struct Id Brand
                 | InterfaceType Id Brand


data AnyPointer = AnyKind
                | AnyStruct
                | AnyList
                | Capability
                | Parameter Id Word16
                | ImplicitMethodParameter Word16

newtype Brand = Brand [Scope]

data Scope = Bind Id [Maybe Type]
           | Inherit

data Value = Value
data Annotation = Annotation
data ElementSize = ElementSize
data CodeGeneratorRequest = CodeGeneratorRequest
