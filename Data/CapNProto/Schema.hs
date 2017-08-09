module Data.CapNProto.Schema where

import Data.Word

data FieldLoc p c
    = PtrField !Word16 -- in pointer section
    | DataField -- in data section
        !Word16 -- word in which the field lies
        !Word16 -- bit offset to the start of the field.
    | GroupField -- field is a group; sub-fields are relative to this field's
                 -- parent.
    deriving(Show)

data Field p c = Field
    { fieldLoc :: !(FieldLoc p c) -- location of field
    , fieldTag :: !(Maybe Word16) -- tag value for which this is valid.
    } deriving(Show)

-- @fromBits lo hi@ is a @Field p c@ covering the bits [lo, hi) in the
-- data section of the struct. This is a convienence helper to make
-- transcribing fields from capnp compile -ocapnp easy.
fromBits lo _hi = Field { fieldLoc = DataField (lo `div` 64) (lo `mod` 64)
                        , fieldTag = Nothing
                        }

ptrField :: Word16 -> Field p c
ptrField n = Field { fieldLoc = PtrField n, fieldTag = Nothing }

data List a
