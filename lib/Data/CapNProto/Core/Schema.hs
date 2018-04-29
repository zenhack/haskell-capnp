{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Data.CapNProto.Core.Schema where

import Prelude hiding (id)

import Data.CapNProto.Errors (Error(SchemaViolationError), ThrowError(..))
import Data.Default          (def)
import Data.ReinterpretCast  (wordToDouble, wordToFloat)

import Data.Bits
import Data.CapNProto.Untyped.ADT
import Data.Int
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

class Decerialize from to where
    decerialize :: (ThrowError m, Monad m) => from -> m to

type Id = Word64

data Node = Node
    { id                      :: Id
    , displayName             :: Text
    , displayNamePrefixLength :: Word32
    , scopeId                 :: Id
    , parameters              :: List Node'Parameter
    , isGeneric               :: Bool
    , nestedNodes             :: List Node'NestedNode
    , union'                  :: Node'Union'
    }

data Node'Parameter = Node'Parameter
    { name :: Text
    }
    deriving(Show, Read, Eq)

instance Decerialize Struct Node'Parameter where
    decerialize (Struct _ ptrs) =
        Node'Parameter <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)

data Node'NestedNode = Node'NestedNode
    { name :: Text
    , id   :: Id
    }
    deriving(Show, Read, Eq)

instance Decerialize Struct Node'NestedNode where
    decerialize (Struct words ptrs) = Node'NestedNode
        <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)
        <*> pure (sliceIndex 0 words)

data Node'Union'
    = Node'File
    | Node'Struct Node'Struct'
    | Node'Enum Node'Enum'
    | Node'Interface Node'Interface'
    | Node'Const Node'Const'
    | Node'Annotation Node'Annotation'
    | Node'Unknown' Word16


field'noDiscriminant :: Word16
field'noDiscriminant = 0xffff

data Field = Field
    { name              :: Text
    , codeOrder         :: Word16
    , annotations       :: List Annotation
    , discriminantValue :: Word16
    , union'            :: Field'Union'
    , ordinal           :: Field'Ordinal
    }
    deriving(Show, Read, Eq)

data Field'Union'
    = Field'Slot Field'Slot'
    | Field'Group Field'Group'
    | Field'Unknown' Word16
    deriving(Show, Read, Eq)

data Field'Slot' = Field'Slot'
    { offset            :: Word32
    , type'             :: Type
    , defaultValue      :: Value
    , hadExplcitDefault :: Bool
    }
    deriving(Show, Read, Eq)

data Field'Group' = Field'Group'
    { typeId :: Id
    }
    deriving(Show, Read, Eq)

data Field'Ordinal
    = Field'Ordinal'Implicit
    | Field'Ordinal'Explicit Word16
    | Field'Ordinal'Unknown' Word16
    deriving(Show, Read, Eq)

instance Decerialize Struct Field where
    decerialize struct@(Struct words ptrs) = Field
        <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)
        <*> pure (fromIntegral (sliceIndex 0 words) :: Word16)
        <*> (listStruct (sliceIndex 1 ptrs) >>= traverse decerialize)
        <*> pure (field'noDiscriminant `xor` fromIntegral (sliceIndex 0 words `shiftR` 16))
        <*> case fromIntegral (sliceIndex 1 words) :: Word16 of
                0 -> Field'Slot <$> (Field'Slot'
                        (fromIntegral $ sliceIndex 0 words `shiftR` 32)
                        <$> (ptrStruct (sliceIndex 2 ptrs) >>= decerialize)
                        <*> (ptrStruct (sliceIndex 3 ptrs) >>= decerialize)
                        <*> pure ((sliceIndex 2 words .&. 1) == 1))
                1 -> pure $ Field'Group $ Field'Group' (sliceIndex 2 words)
                tag -> pure $ Field'Unknown' tag
        <*> case fromIntegral (sliceIndex 1 words `shiftR` 16) :: Word16 of
                0 -> pure Field'Ordinal'Implicit
                1 -> pure $ Field'Ordinal'Explicit $ fromIntegral $ sliceIndex 1 words `shiftR` 32
                tag -> pure $ Field'Ordinal'Unknown' tag

data Value = Value
    { union' :: Value'Union'
    }
    deriving(Show, Read, Eq)

data Value'Union'
    = Value'Void
    | Value'Bool Bool
    | Value'Int8 Int8
    | Value'Int16 Int16
    | Value'Int32 Int32
    | Value'Int64 Int64
    | Value'Uint8 Word8
    | Value'Uint16 Word16
    | Value'Uint32 Word32
    | Value'Uint64 Word64
    | Value'Float32 Float
    | Value'Float64 Double
    | Value'Text Text
    | Value'Data Data
    | Value'List (Maybe PtrType)
    | Value'Enum Word16
    | Value'Struct (Maybe PtrType)
    | Value'Interface
    | Value'AnyPointer (Maybe PtrType)
    | Value'Unknown' Word16
    deriving(Show, Read, Eq)

instance Decerialize Struct Value where
    decerialize struct = Value <$> decerialize struct

instance Decerialize Struct Value'Union' where
    decerialize (Struct words ptrs) =
        let tag = fromIntegral (sliceIndex 0 words) :: Word16
        in case tag of
            0 -> pure Value'Void
            1 -> pure $ Value'Bool $ ((sliceIndex 0 words `shiftR` 16) .&. 1) == 1
            2 -> pure $ Value'Int8 $ fromIntegral (sliceIndex 0 words `shiftR` 16)
            3 -> pure $ Value'Int16 $ fromIntegral (sliceIndex 0 words `shiftR` 16)
            4 -> pure $ Value'Int32 $ fromIntegral (sliceIndex 0 words `shiftR` 32)
            5 -> pure $ Value'Int64 $ fromIntegral $ sliceIndex 1 words
            6 -> pure $ Value'Uint8 $ fromIntegral (sliceIndex 0 words `shiftR` 16)
            7 -> pure $ Value'Uint16 $ fromIntegral (sliceIndex 0 words `shiftR` 16)
            8 -> pure $ Value'Uint32 $ fromIntegral (sliceIndex 0 words `shiftR` 32)
            9 -> pure $ Value'Uint64 $ fromIntegral $ sliceIndex 1 words
            10 -> pure $ Value'Float32 $ wordToFloat $ fromIntegral (sliceIndex 0 words `shiftR` 32)
            11 -> pure $ Value'Float64 $ wordToDouble $ sliceIndex 1 words
            12 -> Value'Text <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)
            13 -> Value'Data <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)
            14 -> pure $ Value'List (sliceIndex 0 ptrs)
            15 -> pure $ Value'Enum $ fromIntegral (sliceIndex 0 words `shiftR` 16)
            16 -> pure $ Value'Struct (sliceIndex 0 ptrs)
            17 -> pure Value'Interface
            18 -> pure $ Value'AnyPointer (sliceIndex 0 ptrs)
            _ -> pure $ Value'Unknown' tag

-- TODO: move these to somewhere common.
instance Decerialize (List Word8) Data where
    decerialize (List vec) = pure $ vec2BS vec

instance Decerialize (List Word8) Text where
    decerialize bytes = Text <$> (decerialize bytes >>= trim)
      where
        trim bs
            | BS.length bs == 0 = throwError $ SchemaViolationError "Text had zero length (no NUL byte)"
            | BS.index bs (BS.length bs - 1) /= 0 = throwError $ SchemaViolationError "Text did not end with NUL"
            | otherwise = pure $ BS.take (BS.length bs - 1) bs

ptrStruct :: ThrowError f => Maybe PtrType -> f Struct
ptrStruct Nothing              = pure def
ptrStruct (Just (PtrStruct s)) = pure s
ptrStruct (Just _)             = expected "pointer to struct"

list0 :: ThrowError f => Maybe PtrType -> f (List ())
list0 Nothing                     = pure def
list0 (Just (PtrList (List0' l))) = pure l
list0 _                           = expected "pointer to list with element size 0"

list1 :: ThrowError f => Maybe PtrType -> f (List Bool)
list1 Nothing                     = pure def
list1 (Just (PtrList (List1' l))) = pure l
list1 _                           = expected "pointer to list with element size 1"

list8 :: ThrowError f => Maybe PtrType -> f (List Word8)
list8 Nothing                     = pure def
list8 (Just (PtrList (List8' l))) = pure l
list8 _                           = expected "pointer to list with element size 8"

list16 :: ThrowError f => Maybe PtrType -> f (List Word16)
list16 Nothing                      = pure def
list16 (Just (PtrList (List16' l))) = pure l
list16 _                            = expected "pointer to list with element size 16"

list32 :: ThrowError f => Maybe PtrType -> f (List Word32)
list32 Nothing                      = pure def
list32 (Just (PtrList (List32' l))) = pure l
list32 _                            = expected "pointer to list with element size 32"

list64 :: ThrowError f => Maybe PtrType -> f (List Word64)
list64 Nothing                      = pure def
list64 (Just (PtrList (List64' l))) = pure l
list64 _                            = expected "pointer to list with element size 64"

listPtr :: ThrowError f => Maybe PtrType -> f (List (Maybe PtrType))
listPtr Nothing                       = pure def
listPtr (Just (PtrList (ListPtr' l))) = pure l
listPtr _                             = expected "pointer to list of pointers"

listStruct :: ThrowError f => Maybe PtrType -> f (List Struct)
listStruct Nothing         = pure def
listStruct (Just (PtrList (ListStruct' l))) = pure l
listStruct _               = expected "pointer to list of structs"

expected msg = throwError $ SchemaViolationError $ "expected " ++ msg

vec2BS :: V.Vector Word8 -> BS.ByteString
-- TODO: replace this with some existing library function (I'm sure one exists)
vec2BS = BS.pack . V.toList

data Brand = Brand
    { scopes :: List Brand'Scope
    }
    deriving(Show, Read, Eq)

instance Decerialize Struct Brand where
    decerialize (Struct _ ptrs) = Brand <$>
        (listStruct (sliceIndex 0 ptrs) >>= traverse decerialize)

data Brand'Scope = Brand'Scope
    { scopeId :: Word64
    , union'  :: Brand'Scope'Union'
    }
    deriving(Show, Read, Eq)

data Brand'Scope'Union'
    = Brand'Scope'Bind (List Brand'Binding)
    | Brand'Scope'Inherit
    | Brand'Scope'Unknown' Word16
    deriving(Show, Read, Eq)

instance Decerialize Struct Brand'Scope where
    decerialize (Struct words ptrs) = Brand'Scope (sliceIndex 0 words) <$>
        case fromIntegral (sliceIndex 1 words) :: Word16 of
            0 -> Brand'Scope'Bind <$>
                    (listStruct (sliceIndex 0 ptrs)
                    >>= traverse decerialize)
            1 -> pure Brand'Scope'Inherit
            tag -> pure $ Brand'Scope'Unknown' tag

data Brand'Binding
    = Brand'Binding'Unbound
    | Brand'Binding'Type Type
    | Brand'Binding'Unknown' Word16
    deriving(Show, Read, Eq)

instance Decerialize Struct Brand'Binding where
    decerialize (Struct words ptrs) =
        case fromIntegral (sliceIndex 0 words) :: Word16 of
            0 -> pure Brand'Binding'Unbound
            1 -> Brand'Binding'Type <$> (ptrStruct (sliceIndex 0 ptrs) >>= decerialize)
            tag -> pure $ Brand'Binding'Unknown' tag

data Enumerant = Enumerant
    { name        :: Text
    , codeOrder   :: Word16
    , annotations :: List Annotation
    }
    deriving(Show, Read, Eq)

instance Decerialize Struct Enumerant where
    decerialize (Struct words ptrs) = Enumerant
        <$> (list8 (sliceIndex 0 ptrs) >>= decerialize)
        <*> pure (fromIntegral $ sliceIndex 0 words)
        <*> (listStruct (sliceIndex 1 ptrs) >>= traverse decerialize)

data Superclass = Superclass
    { id    :: Word64
    , brand :: Brand
    }
    deriving(Show, Read, Eq)

instance Decerialize Struct Superclass where
    decerialize (Struct words ptrs) = Superclass
        (sliceIndex 0 words)
        <$> (ptrStruct (sliceIndex 0 ptrs) >>= decerialize)

data Type = Type
    { union' :: Type'Union'
    }
    deriving(Show, Read, Eq)

data Type'Union'
    = Type'Void
    | Type'Bool
    | Type'Int8
    | Type'Int16
    | Type'Int32
    | Type'Int64
    | Type'Uint8
    | Type'Uint16
    | Type'Uint32
    | Type'Uint64
    | Type'Float32
    | Type'Float64
    | Type'Text
    | Type'Data
    | Type'List
        { elementType :: Type
        }
    | Type'Enum
        { typeId :: Word64
        , brand  :: Brand
        }
    | Type'Struct
        { typeId :: Word64
        , brand  :: Brand
        }
    | Type'Interface
        { typeId :: Word64
        , brand  :: Brand
        }
    | Type'AnyPointer Type'AnyPointer
    | Type'Unknown' Word16
    deriving(Show, Read, Eq)

data Type'AnyPointer
    = Type'AnyPointer'Unconstrained
        { union' :: Type'AnyPointer'Unconstrained'Union'
        }
    | Type'AnyPointer'Parameter
        { scopeId        :: Word64
        , parameterIndex :: Word16
        }
    | Type'AnyPointer'ImplicitMethodParameter
        { parameterIndex :: Word16
        }
    | Type'AnyPointer'Unknown' Word16
    deriving(Show, Read, Eq)

data Type'AnyPointer'Unconstrained'Union'
    = Unconstrained'AnyKind
    | Unconstrained'Struct
    | Unconstrained'List
    | Unconstrained'Capability
    | Unconstrained'Unknown' Word16
    deriving(Show, Read, Eq)

instance Decerialize Struct Type'AnyPointer where
    decerialize (Struct words _) = pure $
        case fromIntegral (sliceIndex 1 words) :: Word16 of
            0 -> Type'AnyPointer'Unconstrained $
                case fromIntegral (sliceIndex 1 words `shiftR` 16) :: Word16 of
                    0   -> Unconstrained'AnyKind
                    1   -> Unconstrained'Struct
                    2   -> Unconstrained'List
                    3   -> Unconstrained'Capability
                    tag -> Unconstrained'Unknown' tag
            1 -> Type'AnyPointer'Parameter
                    (sliceIndex 2 words)
                    (fromIntegral $ sliceIndex 1 words `shiftR` 16)
            2 -> Type'AnyPointer'ImplicitMethodParameter
                    (fromIntegral $ sliceIndex 1 words `shiftR` 16)
            tag -> Type'AnyPointer'Unknown' tag

instance Decerialize Struct Type where
    decerialize struct@(Struct words ptrs) = Type <$>
        case fromIntegral (sliceIndex 0 words) :: Word16 of
            0 -> pure Type'Void
            1 -> pure Type'Bool
            2 -> pure Type'Int8
            3 -> pure Type'Int16
            4 -> pure Type'Int32
            5 -> pure Type'Int64
            6 -> pure Type'Uint8
            7 -> pure Type'Uint16
            8 -> pure Type'Uint32
            9 -> pure Type'Uint64
            10 -> pure Type'Float32
            11 -> pure Type'Float64
            12 -> pure Type'Text
            13 -> pure Type'Data
            14 -> Type'List <$> (ptrStruct (sliceIndex 0 ptrs) >>= decerialize)
            15 -> Type'Enum (sliceIndex 1 words) <$>
                    (ptrStruct (sliceIndex 0 ptrs) >>= decerialize)
            16 -> Type'Struct (sliceIndex 1 words) <$>
                    (ptrStruct (sliceIndex 0 ptrs) >>= decerialize)
            17 -> Type'Interface (sliceIndex 1 words) <$>
                    (ptrStruct (sliceIndex 0 ptrs) >>= decerialize)
            18 -> Type'AnyPointer <$> decerialize struct
            tag -> pure $ Type'Unknown' tag

data CapnpVersion = CapnpVersion
    { major :: Word16
    , minor :: Word8
    , micro :: Word8
    }
    deriving(Show, Read, Eq)

instance Decerialize Struct CapnpVersion where
    decerialize (Struct words _) = pure $ CapnpVersion
        (fromIntegral $ sliceIndex 0 words)
        (fromIntegral $ sliceIndex 0 words `shiftR` 16)
        (fromIntegral $ sliceIndex 0 words `shiftR` 24)

data Annotation = Annotation
    { id    :: Word64
    , brand :: Brand
    , value :: Value
    }
    deriving(Show, Read, Eq)

instance Decerialize Struct Annotation where
    decerialize (Struct words ptrs) = Annotation
        (sliceIndex 0 words)
        <$> (ptrStruct (sliceIndex 1 ptrs) >>= decerialize)
        <*> (ptrStruct (sliceIndex 0 ptrs) >>= decerialize)

-- Still need to implement these, but put them here so the other stuff at least
-- builds.
data Node'Struct'
data Node'Interface'
data Node'Const'
data Node'Enum'
data Node'Annotation'
