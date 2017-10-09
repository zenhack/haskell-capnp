{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Data.CapNProto.Untyped.ADT
    ( Cap(..)
    , Slice(..)
    , Message(..)
    , PtrType(..)
    , Struct(..)
    , List'(..)
    , List
    , Text(..)
    , length
    , (!!)
    , sliceIndex
    )
  where

import Prelude hiding (length, (!!))

import qualified Data.ByteString as BS
import Data.Primitive.Array (Array)
import Data.Word
import Data.Default (Default(def))

type Cap = Word32

newtype Slice a = Slice (List a)

newtype Message = Message (Array BS.ByteString)

newtype Text = Text BS.ByteString

data PtrType
    = PtrStruct !Struct
    | PtrList   !List'
    | PtrCap    !Cap

data Struct = Struct
    { structData :: Slice Word64
    , structPtrs :: Slice (Maybe PtrType)
    }

data List'
    = List0'  (List ())
    | List1'  (List Bool)
    | List8'  (List Word8)
    | List16' (List Word16)
    | List32' (List Word32)
    | List64' (List Word64)
    | ListPtr' (List (Maybe PtrType))
    | ListStruct' (List (Struct))

data List a where
    List0      :: !Int ->                         List ()
    List1      :: !Int ->        BS.ByteString -> List Bool
    List8      :: !Int ->        BS.ByteString -> List Word8
    List16     :: !Int ->        BS.ByteString -> List Word16
    List32     :: !Int ->        BS.ByteString -> List Word32
    List64     ::                BS.ByteString -> List Word64
    ListPtr    :: {-# UNPACK #-} !ListOfPtr    -> List (Maybe PtrType)
    ListStruct :: {-# UNPACK #-} !ListOfStruct -> List Struct
    ListMapped :: List a ->      (a -> b)      -> List b

data ListOfPtr = ListOfPtr
    { ptrListLen :: !Int
    , ptrListSeg :: !Int
    , ptrListOff :: !Int
    , ptrListMsg :: Message
    }

data ListOfStruct = ListOfStruct
    { structListLen  :: !Int
    , structListSeg  :: !Int
    , structListOff  :: !Int
    , structListData :: !Word16
    , structListPtrs :: !Word16
    , structListMsg  :: Message
    }

length :: List a -> Int
length (List0      len)              = len
length (List1      len _)            = len
length (List8      len _)            = len
length (List16     len _)            = len
length (List32     len _)            = len
length (List64     bytes)            = BS.length bytes `div` 8
length (ListPtr    ListOfPtr{..})    = ptrListLen
length (ListStruct ListOfStruct{..}) = structListLen
length (ListMapped list _)           = length list

(!!) :: List a -> Int -> a
(!!) = undefined

instance Functor List where
    fmap f (ListMapped list g) = ListMapped list (f . g)
    fmap f list                = ListMapped list f

sliceIndex :: Default a => Slice a -> Int -> a
sliceIndex (Slice list) i
    | i < length list = list !! i
    | otherwise = def
