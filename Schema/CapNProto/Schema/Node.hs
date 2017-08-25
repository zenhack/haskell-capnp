module Schema.CapNProto.Schema.Node where

import qualified Data.CapNProto.Schema as DS

import Data.Text (Text)
import Data.Word (Word16, Word32)
import qualified Schema.CapNProto.Schema as S

id :: DS.Field S.Node S.Id
id = DS.fromBits 0 64

displayName :: DS.Field S.Node Text
displayName = DS.PtrField 0

displayNamePrefixLength :: DS.Field S.Node Text
displayNamePrefixLength = DS.fromBits 64 96

scopeId :: DS.Field S.Node S.Id
scopeId = DS.fromBits 128 192

parameters :: DS.Field S.Node (DS.List Parameter)
parameters = DS.PtrField 5

isGeneric :: DS.Field S.Node Bool
isGeneric = DS.fromBits 288 289

nestedNodes :: DS.Field S.Node (DS.List NestedNode)
nestedNodes = DS.PtrField 1

annotations :: DS.Field S.Node (DS.List S.Annotation)
annotations = DS.PtrField 2

data Parameter = Parameter
data NestedNode = NestedNode

data Union' = Union'

