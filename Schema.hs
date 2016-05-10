
import PrimTypes (List, AnyPointer)
import Encoding
import Data.Text (Text)
import Data.Word
import Data.Int
import Data.ByteString (ByteString)

fileId = 0xa93fc509624c72d9 :: Word64

type Id = Word64

data Node = Node
    { node_id :: Id
    , displayName :: Text
    , displayNamePrefixLength :: Word32
    , node_scopeId :: Id
    , parameters :: List Node_Parameter
    , isGeneric :: Bool
    , nestedNodes :: List NestedNode
    , node_annotations :: List Annotation
    , node_union :: Node_Union
    }

data Node_Parameter = Node_Parameter
    { parameter_name :: Text
    }

data NestedNode = NestedNode
    { nestedNode_name :: Text
    , nestedNode_id :: Id
    }

data Node_Union
    = File
    | Node_Struct Node_Struct_Group
    | Node_Enum Node_Enum_Group
    | Node_Interface Node_Interface_Group
    | Const Const_Group
    | Node_Annotation Node_Annotation_Group

data Node_Struct_Group = Node_Struct_Group
    { dataWordCount :: Word16
    , pointerCount :: Word16
    , preferredListEncoding :: ElementSize
    , isGroup :: Bool
    , discriminantCount :: Word16
    , discriminantOffset :: Word32
    , fields :: List Field
    }

data Node_Enum_Group = Node_Enum_Group
    { enumerants :: List Enumerant
    }

data Node_Interface_Group = Node_Interface_Group
    { methods :: List Method
    , superclasses :: List Superclass
    }

data Const_Group = Const_Group
    { const_type :: Type
    , const_value :: Value
    }

data Node_Annotation_Group = Node_Annotation_Group
    { annotation_type :: Type
    , targetsFile :: Bool
    , targetsConst :: Bool
    , targetsEnum :: Bool
    , targetsEnumerant :: Bool
    , targetsStruct :: Bool
    , targetsFiled :: Bool
    , targetsUnion :: Bool
    , targetsGroup :: Bool
    , targetsInterface :: Bool
    , targetsMethod :: Bool
    , targetsParam :: Bool
    , targetsAnnotation :: Bool
    }

noDiscriminant = 0xffff :: Word16

data Field = Field
    { field_name :: Text
    , field_codeOrder :: Word16
    , field_annotations :: List Annotation
    , disriminantValue :: Word16
    , field_union :: Field_Union
    , ordinal :: Ordinal
    }

data Field_Union
    = Slot Slot_Group
    | Group Group_Group

data Slot_Group = Slot_Group
    { offset :: Word32
    , slot_type :: Type
    , defaultValue :: Value
    , hasExplicitDefault :: Bool
    }

data Group_Group = Group_Group
    { group_typeId :: Id
    }

data Ordinal
    = Implciit
    | Explicit Word16

data Enumerant = Enumerant
    { enumerant_name :: Text
    , enumerant_codeOrder :: Word16
    , enumerant_annotations :: List Annotation
    }

data Superclass = Superclass
    { superclass_id :: Id
    , superclass_brand :: Brand
    }

data Method = Method
    { method_name :: Text
    , method_codeOrder :: Word16
    , implicitParameters :: List Node_Parameter
    , paramStructType :: Id
    , paramBrand :: Brand
    , resultStructType :: Id
    , resultBrand :: Brand
    , method_annotations :: List Annotation
    }

data Type
    = Type_Void
    | Type_Bool
    | Type_Int8
    | Type_Int16
    | Type_Int32
    | Type_Int64
    | Type_Uint8
    | Type_Uint16
    | Type_Uint32
    | Type_Uint64
    | Type_Float32
    | Type_Float64
    | Type_Text
    | Type_Data
    | Type_List List_Group
    | Type_Enum Type_Enum_Group
    | Type_Struct Type_Struct_Group
    | Type_Interface Type_Interface_Group
    | Type_AnyPointer Type_AnyPointer

data List_Group = List_Group
    { elementType :: Type
    }

data Type_Enum_Group = Type_Enum_Group
    { enum_typeId :: Id
    , enum_brand :: Brand
    }

data Type_Struct_Group = Type_Struct_Group
    { struct_typeId :: Id
    , struct_brand :: Brand
    }

data Type_Interface_Group = Type_Interface_Group
    { interface_typeId :: Id
    , interface_brand :: Brand
    }

data Type_AnyPointer
    = Unconstrained Unconstrained_Union
    | AnyPointer_Parameter Parameter_Group
    | ImplicitMethodParameter Word16

data Unconstrained_Union
    = AnyKind
    | Unconstrained_Struct
    | Unconstrained_List
    | Capability

data Parameter_Group = Parameter_Group
    { parameter_scopeId :: Id
    , parameterIndex :: Word16
    }

data Brand = Brand (List Scope)

data Scope = Scope
    { scope_scopeId :: Id
    , scope_union :: Scope_Union
    }

data Scope_Union
    = Bind (List Binding)
    | Inherit

data Binding
    = Unbound
    | Type Type

data Value
    = Value_Void
    | Value_Bool Bool
    | Value_Int8 Int8
    | Value_Int16 Int16
    | Value_Int32 Int32
    | Value_Int64 Int64
    | Value_Uint8 Word8
    | Value_Uint16 Word16
    | Value_Uint32 Word32
    | Value_Uint64 Word64
    | Value_Float32 Float
    | Value_Float64 Double
    | Value_Text Text
    | Value_Data ByteString
    | Value_List AnyPointer
    | Value_Enum Word16
    | Value_Struct AnyPointer
    | Value_AnyPointer AnyPointer

data Annotation = Annotation
    { annotation_id :: Id
    , annotation_brand :: Brand
    , annotation_value :: Value
    }

data ElementSize
    = Empty
    | Bit
    | Byte
    | TwoBytes
    | FourBytes
    | EightBytes
    | Pointer
    | InlineComposite

data CodeGeneratorRequest = CodeGeneratorRequest
    { nodes :: List Node
    , requestedFiles :: List RequestedFile
    }

data RequestedFile = RequestedFile
    { requestedFile_Id :: Id
    , filename :: Text
    , imports :: List Import
    }

data Import = Import
    { import_id :: Id
    , import_name :: Text
    }
