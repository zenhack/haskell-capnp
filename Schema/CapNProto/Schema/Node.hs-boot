module Schema.CapNProto.Schema.Node where

import Data.Word (Word16, Word32)

import Schema.CapNProto.Schema as S

id :: Field S.Node Id
displayName :: Field S.Node Text
displayNamePrefixLength :: Field S.Node Text
scopeId :: Field S.Node Id
parameters :: Field S.Node (List Parameter)
isGeneric :: Field S.Node Bool
nestedNodes :: Field S.Node (List NestedNode)
annotations :: Field S.Node (List Annotations)

data Parameter = Parameter
data NestedNode = NestedNode

data Union_ = Union_

file :: Field Union_ ()
struct :: Field Union_ Union_struct

data Union_struct = Union_struct

dataWordCount :: Field Union_struct Word16
pointerCount :: Field Union_struct Word16
preferredListEncoding :: Field Union_struct S.ElementSize
isGroup :: Field Union_struct Bool
discriminantCount :: Field Union_struct Word16
discriminantOffset :: Field Union_struct Word32