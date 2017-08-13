module Data.CapNProto.Schema where

import Data.Word

-- | The location of a field within a struct
data FieldLoc p c
    = PtrField !Word16 -- in pointer section
    | DataField -- in data section
        !Word16 -- word in which the field lies
        !Word16 -- bit offset to the start of the field.
    | GroupField -- field is a group; sub-fields are relative to this field's
                 -- parent.
    deriving(Show)

-- | A field within a struct
data Field p c = Field
    { fieldLoc :: !(FieldLoc p c) -- location of field
    , fieldTag :: !(Maybe Word16) -- tag value for which this is valid.
                                  -- 'Nothing' indicates that it is not
                                  -- part of a union.
    } deriving(Show)

-- | @fromBits lo hi@ is a @Field p c@ covering the bits [lo, hi) in the
-- data section of the struct. This is a convienence helper to make
-- transcribing fields from capnp compile -ocapnp easy.
fromBits lo _hi = Field { fieldLoc = DataField (lo `div` 64) (lo `mod` 64)
                        , fieldTag = Nothing
                        }

-- | @ptrFiled n@ is a @Field p c@ corresponding to the index @n@ in the
-- pointer section, which is not restricted to a field tag. This is a
-- convienence helper to make transcribing fields easier.
ptrField :: Word16 -> Field p c
ptrField n = Field { fieldLoc = PtrField n, fieldTag = Nothing }

data List a
