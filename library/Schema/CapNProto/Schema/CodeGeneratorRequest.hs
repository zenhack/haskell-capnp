module Schema.CapNProto.Schema.CodeGeneratorRequest
    (module Schema.CapNProto.Schema.CodeGeneratorRequest)
  where

import qualified Data.CapNProto.Schema   as DS
import qualified Schema.CapNProto.Schema as S

data RequestedFile = RequestedFile

nodes :: DS.Field S.CodeGeneratorRequest (Maybe (DS.List S.Node))
nodes = DS.PtrField 0

requestedFiles :: DS.Field S.CodeGeneratorRequest (Maybe (DS.List RequestedFile))
requestedFiles = DS.PtrField 1
