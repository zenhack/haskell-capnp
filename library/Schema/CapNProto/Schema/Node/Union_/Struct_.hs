module Schema.CapNProto.Schema.Node.Union_.Struct_ where

import qualified Data.CapNProto.Schema               as DS
import qualified Schema.CapNProto.Schema             as S
import           Schema.CapNProto.Schema.Node.Union_ (Struct_)

import Data.Word

dataWordCount :: DS.Field Struct_ Word16
dataWordCount = DS.fromBits 112 128

pointerCount :: DS.Field Struct_ Word16
pointerCount = DS.fromBits 192 208

preferredListEncoding :: DS.Field Struct_ S.ElementSize
preferredListEncoding = DS.fromBits 208 224

isGroup :: DS.Field Struct_ Bool
isGroup = DS.fromBits 224 225

discriminantCount :: DS.Field Struct_ Word16
discriminantCount = DS.fromBits 240 256

discriminantOffset :: DS.Field Struct_ Word32
discriminantOffset = DS.fromBits 256 288
