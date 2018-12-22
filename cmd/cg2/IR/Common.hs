-- Data types that are used by more than one intermediate form.
-- See also IR.Name, which defines identifiers, which are also
-- used in more than one IR.
module IR.Common where

import Data.Word

data IntType = IntType !Sign !IntSize
    deriving(Show, Read, Eq)

data Sign
    = Signed
    | Unsigned
    deriving(Show, Read, Eq)

data IntSize
    = Sz8
    | Sz16
    | Sz32
    | Sz64
    deriving(Show, Read, Eq)

sizeBits :: IntSize -> Int
sizeBits Sz8  = 8
sizeBits Sz16 = 16
sizeBits Sz32 = 32
sizeBits Sz64 = 64

-- | Return the size in bits of a type that belongs in the data section of a struct.
dataFieldSize :: WordType -> Int
dataFieldSize fieldType = case fieldType of
    EnumType _                          -> 16
    PrimWord (PrimInt (IntType _ size)) -> sizeBits size
    PrimWord PrimFloat32                -> 32
    PrimWord PrimFloat64                -> 64
    PrimWord PrimBool                   -> 1

-- Capnproto types.

data Type
    = CompositeType CompositeType
    | VoidType
    | WordType WordType
    | PtrType PtrType
    deriving(Show, Read, Eq)

data CompositeType
    = StructType Word64
    deriving(Show, Read, Eq)

data WordType
    = EnumType Word64
    | PrimWord PrimWord
    deriving(Show, Read, Eq)

data PtrType
    = ListOf Type
    | PrimPtr PrimPtr
    | PtrComposite CompositeType
    | PtrInterface Word64
    deriving(Show, Read, Eq)

data PrimWord
    = PrimInt IntType
    | PrimFloat32
    | PrimFloat64
    | PrimBool
    deriving(Show, Read, Eq)

data PrimPtr
    = PrimText
    | PrimData
    | PrimAnyPtr AnyPtr
    deriving(Show, Read, Eq)

data AnyPtr
    = Struct
    | List
    | Cap
    | Ptr
    deriving(Show, Read, Eq)

-- | The type and location of a field.
data FieldLocType
    -- | The field is in the struct's data section.
    = DataField DataLoc WordType
    -- | The field is in the struct's pointer section (the argument is the
    -- index).
    | PtrField !Word16 PtrType
    -- | The field is a group or union; it's "location" is the whole struct.
    | HereField CompositeType
    -- | The field is of type void (and thus is zero-size).
    | VoidField
    deriving(Show, Read, Eq)

-- | The location of a field within a struct's data section.
data DataLoc = DataLoc
    { dataIdx :: !Int
    -- ^ The index of the 64-bit word containing the field.
    , dataOff :: !Int
    , dataDef :: !Word64
    -- ^ The value is stored xor-ed with this value. This is used
    -- to allow for encoding default values. Note that this is xor-ed
    -- with the bits representing the value, not the whole word.
    }
    deriving(Show, Read, Eq)

-- | Extract the type from a 'FildLocType'.
fieldType :: FieldLocType -> Type
fieldType (DataField _ ty) = WordType ty
fieldType (PtrField _ ty)  = PtrType ty
fieldType (HereField ty)   = CompositeType ty
fieldType VoidField        = VoidType
