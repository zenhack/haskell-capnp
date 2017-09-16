module Schema.CapNProto.Schema.Node.Union'.Struct' where

import Schema.CapNProto.Schema.Node.Union' (Struct')
import qualified Schema.CapNProto.Schema as S
import qualified Data.CapNProto.Schema as DS

import Data.Word

dataWordCount :: DS.Field Struct' Word16
dataWordCount = DS.fromBits 112 128

pointerCount :: DS.Field Struct' Word16
pointerCount = DS.fromBits 192 208

preferredListEncoding :: DS.Field Struct' S.ElementSize
preferredListEncoding = DS.fromBits 208 224

isGroup :: DS.Field Struct' Bool
isGroup = DS.fromBits 224 225

discriminantCount :: DS.Field Struct' Word16
discriminantCount = DS.fromBits 240 256

discriminantOffset :: DS.Field Struct' Word32
discriminantOffset = DS.fromBits 256 288
