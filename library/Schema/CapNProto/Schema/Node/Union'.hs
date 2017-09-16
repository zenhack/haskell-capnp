module Schema.CapNProto.Schema.Node.Union' where

import qualified Data.CapNProto.Schema as DS
import Schema.CapNProto.Schema.Node (Union')

file :: DS.UnionVariant Union' ()
file = DS.UnionVariant 0

struct :: DS.UnionVariant Union' Struct'
struct = DS.UnionVariant 1

data Struct' = Struct'
