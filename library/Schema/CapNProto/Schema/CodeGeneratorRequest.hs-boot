module Schema.CapNProto.Schema.CodeGeneratorRequest where

import qualified Data.CapNProto.Schema as DS
import qualified Schema.CapNProto.Schema as S

data RequestedFile = RequestedFile

nodes :: DS.Field S.CodeGeneratorRequest (DS.List S.Node)
requestedFiles :: DS.Field S.CodeGeneratorRequest (DS.List RequestedFile)
