-- Data types that are used by more than one intermediate form.
-- See also IR.Name, which defines identifiers, which are also
-- used in more than one IR.
module IR.Common where

data PrimType
    = PTyVoid
    | PTyBool
    | PTyInt Sign IntSize
    | PTyFloat32
    | PTyFloat64
    deriving(Show, Read, Eq)

data Sign = Signed | Unsigned deriving(Show, Read, Eq)

data IntSize = Sz8 | Sz16 | Sz32 | Sz64 deriving(Show, Read, Eq)
