module Data.Capnp.Schema
    ( Field(..)
    , fromBits
    , List
    , UnionVariant(..)
    )
  where

import Data.Word

-- | A field within a struct
data Field p c
    = PtrField !Word16 -- in pointer section
    | DataField -- in data section
        !Word16 -- word in which the field lies
        !Word16 -- bit offset to the start of the field.
    | GroupField -- field is a group; sub-fields are relative to this field's
                 -- parent.
    | UnionField -- field is a union; sub-fields are relative to this field's
                 -- parent
        !Word16 -- offset to the word in the data section containing the tag
        !Word16 -- bit offset of the tag within the word
    deriving(Show)

-- | @fromBits lo hi@ is a @Field p c@ covering the bits [lo, hi) in the
-- data section of the struct. This is a convienence helper to make
-- transcribing fields from capnp compile -ocapnp easy.
fromBits lo _hi = DataField (lo `div` 64) (lo `mod` 64)

data List a

-- thinking:

-- | A variant in a union field
newtype UnionVariant p c
    = UnionVariant Word16 -- the tag for the field
    deriving(Show)
