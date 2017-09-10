{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Schema where

import Data.Word (Word64, Word32, Word16)
import Data.Text (Text)


type Id = Word64

data Node = Node
  { nodeId :: Id
  , displayName :: Text
  , displayNamePrefixLength :: Word32
  , scopeId :: Id
  , parameters :: [NodeParameter]
  , nestedNodes :: [NestedNode]
  , nodeAnnotations :: [Annotation]
  , nodeType :: NodeType
  }

data NodeType =
    File
  | Struct NodeTypeStruct
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

-- do something with a typeclass?
noDiscriminant :: Discriminant
noDiscriminant = Discriminant 0xffff

data Field = Field
  { fieldName :: Text
  , codeOrder :: Word16
  , annotation :: [Annotation]
  , discriminantValue :: Discriminant
  }

newtype Discriminant = Discriminant Word16

data Enumerant = Enumerant
data Superclass = Superclass
data Method = Method
data Type = Type
data Brand = Brand
data Value = Value
data Annotation = Annotation
data ElementSize = ElementSize
data CodeGeneratorRequest = CodeGeneratorRequest
