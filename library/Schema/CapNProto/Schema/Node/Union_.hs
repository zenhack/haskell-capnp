module Schema.CapNProto.Schema.Node.Union_
    (module Schema.CapNProto.Schema.Node.Union_)
  where

import qualified Data.CapNProto.Schema        as DS
import           Schema.CapNProto.Schema.Node (Union_)

file :: DS.UnionVariant Union_ ()
file = DS.UnionVariant 0

struct :: DS.UnionVariant Union_ Struct_
struct = DS.UnionVariant 1

data Struct_ = Struct_
