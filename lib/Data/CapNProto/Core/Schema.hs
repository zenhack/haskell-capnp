{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
module Data.CapNProto.Core.Schema where

import Prelude hiding (id)

import Data.CapNProto.Errors (Error(SchemaViolationError), ThrowError(..))
import Data.ReinterpretCast  (wordToDouble, wordToFloat)

import Data.Bits
import Data.CapNProto.Untyped.ADT
import Data.Int
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

type Id = Word64

data Node = Node
    { id                      :: Id
    , displayName             :: Maybe Text
    , displayNamePrefixLength :: Word32
    , scopeId                 :: Id
    , parameters              :: Maybe (List Parameter)
    , isGeneric               :: Bool
    , nestedNodes             :: Maybe (List Node'NestedNode)
    , union'                  :: Node'Union'
    }

data Node'NestedNode = Node'NestedNode
    { name :: Maybe Text
    , id   :: Id
    }

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
    { name              :: Maybe Text
    , codeOrder         :: Word16
    , annotations       :: Maybe (List Annotation)
    , discriminantValue :: Word16
    , union'            :: Field'Union'
    , ordinal           :: Field'Ordinal
    }

data Field'Union'
    = Field'Slot Field'Slot'
    | Field'Group Field'Group'

data Field'Slot' = Field'Slot'
    { offset            :: Word32
    , type'             :: Type
    , defaultValue      :: Value
    , hadExplcitDefault :: Bool
    }

data Field'Group' = Field'Group'
    { typeId :: Id
    }

data Field'Ordinal
    = Field'Oridinal'Implicit
    | Field'Oridinal'Excplicit Word16
    | Field'Oridinal'Unknown' Word16

data Value = Value
    { union' :: Value'Union'
    }

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

readValue :: (ThrowError m, Monad m) => Struct -> m Value
readValue struct = Value <$> readValue'Union' struct

readValue'Union' :: (ThrowError m, Monad m) => Struct -> m Value'Union'
readValue'Union' (Struct words ptrs) =
    let tag = fromIntegral (sliceIndex 0 words) :: Word16
    in case tag of
        0 -> pure Value'Void
        1 -> pure $ Value'Bool $ ((sliceIndex 0 words `shiftR` 16) .&. 1) == 1
        2 -> pure $ Value'Int8 $ fromIntegral (sliceIndex 0 words `shiftR` 24)
        3 -> pure $ Value'Int16 $ fromIntegral (sliceIndex 0 words `shiftR` 24)
        4 -> pure $ Value'Int32 $ fromIntegral (sliceIndex 0 words `shiftR` 32)
        5 -> pure $ Value'Int64 $ fromIntegral $ sliceIndex 1 words
        6 -> pure $ Value'Uint8 $ fromIntegral (sliceIndex 0 words `shiftR` 24)
        7 -> pure $ Value'Uint16 $ fromIntegral (sliceIndex 0 words `shiftR` 24)
        8 -> pure $ Value'Uint32 $ fromIntegral (sliceIndex 0 words `shiftR` 32)
        9 -> pure $ Value'Uint64 $ fromIntegral $ sliceIndex 1 words
        10 -> pure $ Value'Float32 $ wordToFloat $ fromIntegral (sliceIndex 0 words `shiftR` 32)
        11 -> pure $ Value'Float64 $ wordToDouble $ sliceIndex 1 words
        12 -> Value'Text <$> readText (sliceIndex 0 ptrs)
        13 -> Value'Data <$> readData (sliceIndex 0 ptrs)
        14 -> pure $ Value'List (sliceIndex 0 ptrs)
        15 -> pure $ Value'Enum $ fromIntegral (sliceIndex 0 words `shiftR` 24)
        16 -> pure $ Value'Struct (sliceIndex 0 ptrs)
        17 -> pure Value'Interface
        18 -> pure $ Value'AnyPointer (sliceIndex 0 ptrs)
        _ -> pure $ Value'Unknown' tag

-- TODO: move readData/readText to somewhere common.
readData :: (ThrowError m, Monad m) => Maybe PtrType -> m Data
readData = \case
    Nothing -> pure ""
    Just (PtrList (List8' (List vec))) -> pure (vec2BS vec)
    Just _ -> throwError $ SchemaViolationError "expected list (elt size = 8)"

readText :: (ThrowError m, Monad m) => Maybe PtrType -> m Text
readText ptr = Text <$> (trim =<< readData ptr) where
    trim bs
        | BS.length bs == 0 = throwError $ SchemaViolationError "Text had zero length (no NUL byte)"
        | BS.index bs 0 /= 0 = throwError $ SchemaViolationError "Text did not end with NUL"
        | otherwise = pure $ BS.take (BS.length bs - 1) bs

vec2BS :: V.Vector Word8 -> BS.ByteString
-- TODO: replace this with some existing library function (I'm sure one exists)
vec2BS = BS.pack . V.toList


-- Still need to implement these, but put them here so the other stuff at least
-- builds.
data Type
data Annotation
data Parameter
data Node'Struct'
data Node'Interface'
data Node'Const'
data Node'Enum'
data Node'Annotation'
