{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-|
Module: Capnp.Untyped.Pure
Description: high-level API for working with untyped Cap'N Proto values.

This module provides an idiomatic Haskell interface for untyped capnp
data, based on algebraic datatypes. It forgoes some of the benefits of
the capnp wire format in favor of a more convienient API.

In addition to the algebraic data types themselves, this module also
provides support for converting from the lower-level types in
"Capnp.Untyped".
-}
module Capnp.Untyped.Pure
    ( Slice(..)
    , PtrType(..)
    , Struct(..)
    , List(..)
    , ListOf(..)
    , length
    , sliceIndex
    )
  where

import Prelude hiding (length)

import Data.Word

import Control.Monad                 (forM_)
import Data.Default                  (Default(def))
import Data.Default.Instances.Vector ()
import GHC.Exts                      (IsList(..))
import GHC.Generics                  (Generic)

import Capnp.Classes
    (Cerialize(..), Decerialize(..), IsPtr(..), Marshal(..))
import Internal.Gen.Instances ()

import qualified Capnp.Message as M
import qualified Capnp.Untyped as U
import qualified Data.Vector   as V

-- | A one of a struct's sections (data or pointer).
--
-- This is just a newtype wrapper around 'ListOf' (which is itself just
-- 'V.Vector'), but critically the notion of equality is different. Two
-- slices are considered equal if all of their elements are equal, but
-- If the slices are different lengths, missing elements are treated as
-- having default values. Accordingly, equality is only defined if the
-- element type is an instance of 'Default'.
newtype Slice a = Slice (ListOf a)
    deriving(Generic, Show, Read, Ord, Functor, Default, IsList)

-- | A capnproto pointer type.
data PtrType
    = PtrStruct !Struct
    | PtrList   !List
    | PtrCap    !M.Client
    deriving(Generic, Show, Read, Eq)

-- | A capnproto struct.
data Struct = Struct
    { structData :: Slice Word64
    -- ^ The struct's data section
    , structPtrs :: Slice (Maybe PtrType)
    -- ^ The struct's pointer section
    }
    deriving(Generic, Show, Read, Eq)
instance Default Struct

-- | An untyped list.
data List
    = List0  (ListOf ())
    | List1  (ListOf Bool)
    | List8  (ListOf Word8)
    | List16 (ListOf Word16)
    | List32 (ListOf Word32)
    | List64 (ListOf Word64)
    | ListPtr (ListOf (Maybe PtrType))
    | ListStruct (ListOf Struct)
    deriving(Generic, Show, Read, Eq)

-- | Alias for 'V.Vector'. Using this alias may make upgrading to future
-- versions of the library easier, as we will likely switch to a more
-- efficient representation at some point.
type ListOf a = V.Vector a

-- | Alias for vector's 'V.length'.
length :: ListOf a -> Int
length = V.length

-- | Index into a slice, returning a default value if the the index is past
-- the end of the array.
sliceIndex :: Default a => Int -> Slice a -> a
sliceIndex i (Slice vec)
    | i < V.length vec = vec V.! i
    | otherwise = def

instance (Default a, Eq a) => Eq (Slice a) where
    -- We define equality specially (rather than just deriving), such that
    -- slices are padded out with the default values of their elements.
    l@(Slice vl) == r@(Slice vr) = go (max (length vl) (length vr) - 1)
      where
        go (-1) = True -- can happen if both slices are empty.
        go 0    = True
        go i    = sliceIndex i l == sliceIndex i r && go (i-1)

instance Decerialize Struct where
    type Cerial msg Struct = U.Struct msg

    decerialize struct = Struct
        <$> (Slice <$> decerializeListOfWord (U.dataSection struct))
        <*> (Slice <$> decerializeListOf     (U.ptrSection struct))

instance Marshal Struct where
    marshalInto raw (Struct (Slice dataSec) (Slice ptrSec)) = do
        forM_ [0..V.length dataSec - 1] $ \i ->
            U.setData (dataSec V.! i) i raw
        forM_ [0..V.length ptrSec - 1] $ \i -> do
            ptr <- cerialize (U.message raw) (ptrSec V.! i)
            U.setPtr ptr i raw

instance Cerialize s Struct where
    cerialize msg struct@(Struct (Slice dataSec) (Slice ptrSec)) = do
        raw <- U.allocStruct
            msg
            (fromIntegral $ V.length dataSec)
            (fromIntegral $ V.length ptrSec)
        marshalInto raw struct
        pure raw

instance Decerialize (Maybe PtrType) where
    type Cerial msg (Maybe PtrType) = Maybe (U.Ptr msg)

    decerialize Nothing = pure Nothing
    decerialize (Just ptr) = Just <$> case ptr of
        U.PtrCap cap       -> PtrCap <$> U.getClient cap
        U.PtrStruct struct -> PtrStruct <$> decerialize struct
        U.PtrList list     -> PtrList <$> decerialize list

instance Cerialize s (Maybe PtrType) where
    cerialize _ Nothing                     = pure Nothing
    cerialize msg (Just (PtrStruct struct)) = cerialize msg struct >>= toPtr msg
    cerialize msg (Just (PtrList     list)) = Just . U.PtrList <$> cerialize msg list
    cerialize msg (Just (PtrCap       cap)) = Just . U.PtrCap <$> U.appendCap msg cap

-- | Decerialize an untyped list, whose elements are instances of Decerialize. This isn't
-- an instance, since it would have to be an instance of (List a), which conflicts with
-- the above.
decerializeListOf :: (U.ReadCtx m M.ConstMsg, Decerialize a)
    => U.ListOf M.ConstMsg (Cerial M.ConstMsg a) -> m (ListOf a)
decerializeListOf raw = V.generateM (U.length raw) (\i -> U.index i raw >>= decerialize)

-- | Decerialize an untyped list, leaving the elements of the list as-is. The is most
-- interesting for types that go in the data section of a struct, hence the name.
decerializeListOfWord :: (U.ReadCtx m M.ConstMsg)
    => U.ListOf M.ConstMsg a -> m (ListOf a)
decerializeListOfWord raw = V.generateM (U.length raw) (`U.index` raw)

instance Decerialize List where
    type Cerial msg List = U.List msg

    decerialize (U.List0 l)      = List0 <$> decerializeListOfWord l
    decerialize (U.List1 l)      = List1 <$> decerializeListOfWord l
    decerialize (U.List8 l)      = List8 <$> decerializeListOfWord l
    decerialize (U.List16 l)     = List16 <$> decerializeListOfWord l
    decerialize (U.List32 l)     = List32 <$> decerializeListOfWord l
    decerialize (U.List64 l)     = List64 <$> decerializeListOfWord l
    decerialize (U.ListPtr l)    = ListPtr <$> decerializeListOf l
    decerialize (U.ListStruct l) = ListStruct <$> decerializeListOf l

instance Cerialize s List where
    cerialize msg (List0   l) = U.List0  <$> U.allocList0 msg (length l)
    cerialize msg (List1   l) = U.List1  <$> cerializeListOfWord (U.allocList1  msg) l
    cerialize msg (List8   l) = U.List8  <$> cerializeListOfWord (U.allocList8  msg) l
    cerialize msg (List16  l) = U.List16 <$> cerializeListOfWord (U.allocList16 msg) l
    cerialize msg (List32  l) = U.List32 <$> cerializeListOfWord (U.allocList32 msg) l
    cerialize msg (List64  l) = U.List64 <$> cerializeListOfWord (U.allocList64 msg) l
    cerialize msg (ListPtr l) = do
        raw <- U.allocListPtr msg (length l)
        forM_ [0..length l - 1] $ \i -> do
            ptr <- cerialize msg (l V.! i)
            U.setIndex ptr i raw
        pure $ U.ListPtr raw
    cerialize msg (ListStruct l) = do
        let (maxData, maxPtrs) = measureStructSizes l
        raw <- U.allocCompositeList msg maxData maxPtrs (length l)
        forM_ [0..length l - 1] $ \i -> do
            elt <- U.index i raw
            marshalInto elt (l V.! i)
        pure $ U.ListStruct raw
      where
        -- Find the maximum sizes of each section of any of the structs
        -- in the list. This is the size we need to set in the tag word.
        measureStructSizes :: ListOf Struct -> (Word16, Word16)
        measureStructSizes = foldl
            (\(!dataSz, !ptrSz) (Struct (Slice dataSec) (Slice ptrSec)) ->
                ( max dataSz (fromIntegral $ length dataSec)
                , max ptrSz  (fromIntegral $ length ptrSec)
                )
            )
            (0, 0)


cerializeListOfWord :: U.RWCtx m s => (Int -> m (U.ListOf (M.MutMsg s) a)) -> ListOf a -> m (U.ListOf (M.MutMsg s) a)
cerializeListOfWord alloc list = do
    ret <- alloc (length list)
    marshalListOfWord ret list
    pure ret

marshalListOfWord :: U.RWCtx m s => U.ListOf (M.MutMsg s) a -> ListOf a -> m ()
marshalListOfWord raw l =
    forM_ [0..length l - 1] $ \i ->
        U.setIndex (l V.! i) i raw
