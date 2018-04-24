{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}
{-| This module provides an idiomatic Haskell interface for untyped capnp
    data, based on algebraic datatypes. It forgoes some of the benefits of
    the capnp wire format in favor of a more convienient API.

    In addition to the algebraic data types themselves, this module also
    provides support for converting from the lower-level types in
    Data.CapNProto.Untyped.
-}
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

    -- Converting from Data.CapNProto.Untyped.
    , readStruct
    )
  where

import Prelude hiding (length, readList, (!!))

import qualified Data.ByteString        as BS
import qualified Data.CapNProto.Untyped as U
import           Data.Default           (Default(def))
import           Data.Primitive.Array   (Array)
import qualified Data.Vector            as V
import           Data.Word

type Cap = Word32

newtype Slice a = Slice (List a)

newtype Message = Message (Array BS.ByteString)

newtype Text = Text BS.ByteString

data PtrType
    = PtrStruct !Struct
    | PtrList   !List'
    | PtrCap    !Cap

data Struct = Struct
    { structData :: BS.ByteString
    , structPtrs :: V.Vector (Maybe PtrType)
    }

data List'
    = List0'  (List ())
    | List1'  (List Bool)
    | List8'  (List Word8)
    | List16' (List Word16)
    | List32' (List Word32)
    | List64' (List Word64)
    | ListPtr' (List (Maybe PtrType))
    | ListStruct' (List Struct)

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

-- | Parse a struct into its ADT form.
readStruct :: U.ReadCtx m BS.ByteString => U.Struct BS.ByteString -> m Struct
readStruct struct = Struct
    <$> U.rawBytes (U.dataSection struct)
    <*> readPtrSection
  where
    readPtrSection = do
        let ptrs = U.ptrSection struct
        V.generateM (U.length ptrs) $ \i -> do
            elt <- U.index i ptrs
            readPtr elt

-- | Parse a (possibly null) pointer into its ADT form.
readPtr :: U.ReadCtx m BS.ByteString
    => Maybe (U.Ptr BS.ByteString)
    -> m (Maybe PtrType)
readPtr Nothing               = return Nothing
readPtr (Just ptr) = Just <$> case ptr of
    U.PtrCap cap       -> return (PtrCap cap)
    U.PtrStruct struct -> PtrStruct <$> readStruct struct
    U.PtrList list     -> PtrList <$> readList list

-- | Parse a list into its ADT form.
readList :: U.ReadCtx m BS.ByteString => U.List BS.ByteString -> m List'
readList = undefined
