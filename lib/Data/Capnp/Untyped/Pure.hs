{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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

import Prelude hiding (length)

import Data.Word

import Codec.Capnp                   (Decerialize(..), expected)
import Control.Monad.Catch           (MonadThrow)
import Data.Default                  (Default(def))
import Data.Default.Instances.Vector ()
import Data.Primitive.Array          (Array)
import GHC.Exts                      (IsList(..))
import GHC.Generics                  (Generic)

import qualified Codec.Capnp        as C
import qualified Data.ByteString    as BS
import qualified Data.Capnp.Message as M
import qualified Data.Capnp.Untyped as U
import qualified Data.Vector        as V

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

instance Decerialize (U.Struct M.ConstMessage) Struct where
    decerialize struct = Struct
        <$> (Slice <$> decerialize (U.dataSection struct))
        <*> (Slice <$> decerialize (U.ptrSection struct))

instance Decerialize (Maybe (U.Ptr M.ConstMessage)) (Maybe PtrType) where
    decerialize Nothing = pure Nothing
    decerialize (Just ptr) = Just <$> case ptr of
        U.PtrCap _ cap     -> return (PtrCap cap)
        U.PtrStruct struct -> PtrStruct <$> decerialize struct
        U.PtrList list     -> PtrList <$> decerialize list

instance (C.ListElem M.ConstMessage a, Decerialize a b) => Decerialize (C.List M.ConstMessage a) (List b) where
    decerialize raw = V.generateM (C.length raw) (\i -> C.index i raw >>= decerialize)

instance Decerialize a b => Decerialize (U.ListOf M.ConstMessage a) (List b) where
    decerialize raw = V.generateM (U.length raw) (\i -> U.index i raw >>= decerialize)

instance Decerialize (U.List M.ConstMessage) List' where
    decerialize (U.List0 l)      = List0' <$> decerialize l
    decerialize (U.List1 l)      = List1' <$> decerialize l
    decerialize (U.List8 l)      = List8' <$> decerialize l
    decerialize (U.List16 l)     = List16' <$> decerialize l
    decerialize (U.List32 l)     = List32' <$> decerialize l
    decerialize (U.List64 l)     = List64' <$> decerialize l
    decerialize (U.ListPtr l)    = ListPtr' <$> decerialize l
    decerialize (U.ListStruct l) = ListStruct' <$> decerialize l

ptrStruct :: MonadThrow f => Maybe PtrType -> f Struct
ptrStruct Nothing              = pure def
ptrStruct (Just (PtrStruct s)) = pure s
ptrStruct (Just _)             = expected "pointer to struct"

list0 :: MonadThrow f => Maybe PtrType -> f (List ())
list0 Nothing                     = pure def
list0 (Just (PtrList (List0' l))) = pure l
list0 _                           = expected "pointer to list with element size 0"

list1 :: MonadThrow f => Maybe PtrType -> f (List Bool)
list1 Nothing                     = pure def
list1 (Just (PtrList (List1' l))) = pure l
list1 _                           = expected "pointer to list with element size 1"

list8 :: MonadThrow f => Maybe PtrType -> f (List Word8)
list8 Nothing                     = pure def
list8 (Just (PtrList (List8' l))) = pure l
list8 _                           = expected "pointer to list with element size 8"

list16 :: MonadThrow f => Maybe PtrType -> f (List Word16)
list16 Nothing                      = pure def
list16 (Just (PtrList (List16' l))) = pure l
list16 _                            = expected "pointer to list with element size 16"

list32 :: MonadThrow f => Maybe PtrType -> f (List Word32)
list32 Nothing                      = pure def
list32 (Just (PtrList (List32' l))) = pure l
list32 _                            = expected "pointer to list with element size 32"

list64 :: MonadThrow f => Maybe PtrType -> f (List Word64)
list64 Nothing                      = pure def
list64 (Just (PtrList (List64' l))) = pure l
list64 _                            = expected "pointer to list with element size 64"

listPtr :: MonadThrow f => Maybe PtrType -> f (List (Maybe PtrType))
listPtr Nothing                       = pure def
listPtr (Just (PtrList (ListPtr' l))) = pure l
listPtr _                             = expected "pointer to list of pointers"

listStruct :: MonadThrow f => Maybe PtrType -> f (List Struct)
listStruct Nothing         = pure def
listStruct (Just (PtrList (ListStruct' l))) = pure l
listStruct _               = expected "pointer to list of structs"
