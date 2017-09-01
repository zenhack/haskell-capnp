module Schema.CapNProto.Reader.Schema where

import qualified Data.CapNProto.Untyped as U
import Data.Word

type Id = Word64

newtype Node b = Node (U.Struct b)
newtype Field b = Field (U.Struct b)
newtype Superclass b = Superclass (U.Struct b)
newtype Method b = Method (U.Struct b)
newtype Type b = Type (U.Struct b)
newtype Brand b = Brand (U.Struct b)
newtype Value b = Value (U.Struct b)
newtype Annotation b = Annotation (U.Struct b)
newtype ElementSize b = ElementSize Word16
newtype CodeGeneratorRequest b = CodeGeneratorRequest (U.Struct b)
