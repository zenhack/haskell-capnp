{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Node.Union_
    (module Schema.CapNProto.Reader.Schema.Node.Union_)
  where

import qualified Schema.CapNProto.Reader.Schema as S

$(mkStructWrappers
    [ "Struct"
    , "Enum"
    , "Interface"
    , "Const"
    , "Annotation"
    ])

$(mkUnionReaders ''Union_
    [ ("file",       0, UnionVoid)
    , ("struct",     1, UnionGroup ''Struct)
    , ("enum",       2, UnionGroup ''Enum)
    , ("interface",  3, UnionGroup ''Interface)
    , ("const",      4, UnionGroup ''Const)
    , ("annotation", 5, UnionGroup ''Annotation)
    ])
