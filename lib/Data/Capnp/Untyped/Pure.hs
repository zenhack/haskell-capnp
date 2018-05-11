{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-| This module provides an idiomatic Haskell interface for untyped capnp
    data, based on algebraic datatypes. It forgoes some of the benefits of
    the capnp wire format in favor of a more convienient API.

    In addition to the algebraic data types themselves, this module also
    provides support for converting from the lower-level types in
    Data.Capnp.Untyped.
-}
module Data.Capnp.Untyped.Pure
    ( Cap(..)
    , Slice(..)
    , Message(..)
    , PtrType(..)
    , Struct(..)
    , List'(..)
    , List(..)
    , length
    , sliceIndex

    -- Converting from Data.Capnp.Untyped.
    , readStruct

    -- TODO: figure out a better places to put these:
    , ptrStruct
    , list0
    , list1
    , list8
    , list16
    , list32
    , list64
    , listStruct
    )
  where

import Prelude hiding (length, readList)

import Data.Default.Instances.Vector ()

import GHC.Exts     (IsList(..))
import GHC.Generics (Generic)

import qualified Data.ByteString      as BS
import qualified Data.Capnp.Untyped   as U
import           Data.Default         (Default(def))
import           Data.Primitive.Array (Array)
import qualified Data.Vector          as V
import           Data.Word

import Codec.Capnp       (expected)
import Data.Capnp.Errors (ThrowError)

type Cap = Word32

newtype Slice a = Slice (List a)
    deriving(Generic, Show, Read, Eq, Ord, Functor, Default)

newtype Message = Message (Array BS.ByteString)
    deriving(Generic, Show, Read, Eq, Ord)

data PtrType
    = PtrStruct !Struct
    | PtrList   !List'
    | PtrCap    !Cap
    deriving(Generic, Show, Read, Eq)

data Struct = Struct
    { structData :: Slice Word64
    , structPtrs :: Slice (Maybe PtrType)
    }
    deriving(Generic, Show, Read, Eq)
instance Default Struct

data List'
    = List0'  (List ())
    | List1'  (List Bool)
    | List8'  (List Word8)
    | List16' (List Word16)
    | List32' (List Word32)
    | List64' (List Word64)
    | ListPtr' (List (Maybe PtrType))
    | ListStruct' (List Struct)
    deriving(Generic, Show, Read, Eq)

type List a = V.Vector a

-- Cookie-cutter IsList instances. These are derivable with
-- GeneralizedNewtypeDeriving as of ghc >= 8.2.1, but not on
-- 8.0.x, due to the associated type.
instance IsList Message where
    type Item Message = BS.ByteString
    toList (Message segs) = toList segs
    fromList = Message . fromList
    fromListN n = Message . fromListN n
instance IsList (Slice a) where
    type Item (Slice a) = a
    toList (Slice list) = toList list
    fromList = Slice . fromList
    fromListN n = Slice . fromListN n

length :: List a -> Int
length = V.length

sliceIndex :: Default a => Int -> Slice a -> a
sliceIndex i (Slice vec)
    | i < V.length vec = vec V.! i
    | otherwise = def

-- | Parse a struct into its ADT form.
readStruct :: U.ReadCtx m BS.ByteString => U.Struct BS.ByteString -> m Struct
readStruct struct = Struct
    <$> (Slice <$> readList (U.dataSection struct) pure)
    <*> (Slice <$> readList (U.ptrSection struct) readPtr)

-- | Parse a (possibly null) pointer into its ADT form.
readPtr :: U.ReadCtx m BS.ByteString
    => Maybe (U.Ptr BS.ByteString)
    -> m (Maybe PtrType)
readPtr Nothing               = return Nothing
readPtr (Just ptr) = Just <$> case ptr of
    U.PtrCap cap       -> return (PtrCap cap)
    U.PtrStruct struct -> PtrStruct <$> readStruct struct
    U.PtrList list     -> PtrList <$> readList' list

-- | @'readList' list readElt@ parses a list into its ADT form. @readElt@ is
-- used to parse the elements.
readList :: U.ReadCtx m BS.ByteString => U.ListOf BS.ByteString a -> (a -> m b) -> m (List b)
readList list readElt =
    V.generateM (U.length list) (\i -> U.index i list >>= readElt)

readList' :: U.ReadCtx m BS.ByteString => U.List BS.ByteString -> m List'
readList' (U.List0 l)      = List0' <$> readList l pure
readList' (U.List1 l)      = List1' <$> readList l pure
readList' (U.List8 l)      = List8' <$> readList l pure
readList' (U.List16 l)     = List16' <$> readList l pure
readList' (U.List32 l)     = List32' <$> readList l pure
readList' (U.List64 l)     = List64' <$> readList l pure
readList' (U.ListPtr l)    = ListPtr' <$> readList l readPtr
readList' (U.ListStruct l) = ListStruct' <$> readList l readStruct

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
