{-# LANGUAGE TemplateHaskell #-}
module Schema.CapNProto.Reader.Schema.Field.Union_
    (module Schema.CapNProto.Reader.Schema.Field.Union_)
  where

import Language.CapNProto.TH

$(mkStructWrappers
    [ "Slot"
    , "Group"
    ])
