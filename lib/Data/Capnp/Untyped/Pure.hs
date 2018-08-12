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

import Control.Monad (forM_)
import Data.Word

import Internal.Gen.Instances ()

import Codec.Capnp
    (Cerialize(..), Decerialize(..), IsPtr(..), Marshal(..), expected)
import Control.Monad.Catch           (MonadThrow(..))
import Data.Default                  (Default(def))
import Data.Default.Instances.Vector ()
import Data.Primitive.Array          (Array)
import GHC.Exts                      (IsList(..))
import GHC.Generics                  (Generic)

import qualified Data.ByteString    as BS
import qualified Data.Capnp.Errors  as E
import qualified Data.Capnp.Message as M
import qualified Data.Capnp.Untyped as U
import qualified Data.Vector        as V

import qualified Codec.Capnp as C

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
        U.PtrCap _ cap     -> return (PtrCap cap)
        U.PtrStruct struct -> PtrStruct <$> decerialize struct
        U.PtrList list     -> PtrList <$> decerialize list

instance Marshal (Maybe PtrType) where
    marshalInto Nothing Nothing = pure ()
    marshalInto (Just (U.PtrStruct   raw)) (Just (PtrStruct struct)) = marshalInto raw struct
    marshalInto (Just (U.PtrList     raw)) (Just (PtrList     list)) = marshalList' raw list
    marshalInto (Just (U.PtrCap _msg raw)) (Just (PtrCap       cap)) = pure () -- TODO
    marshalInto _ _ = throwM $ E.SchemaViolationError "Mismatched pointer types"

instance Cerialize s (Maybe PtrType) where
    cerialize _ Nothing                     = pure Nothing
    cerialize msg (Just (PtrStruct struct)) = toPtr <$> cerialize msg struct
    cerialize msg (Just (PtrList     list)) = Just . U.PtrList <$> cerializeList' msg list
    -- TODO: when we actually support it, we need to insert the cap into the message:
    cerialize msg (Just (PtrCap       cap)) = pure $ Just (U.PtrCap msg cap)

-- Generic decerialize instances for lists. TODO: this doesn't really belong
-- in Untyped, since this is mostly used for typed lists. maybe Basics.
instance
    ( C.ListElem M.ConstMsg (Cerial M.ConstMsg a)
    , Decerialize a
    ) => Decerialize (List a)
  where
    type Cerial msg (List a) = C.List msg (Cerial msg a)
    decerialize raw = V.generateM (C.length raw) (\i -> C.index i raw >>= decerialize)

-- | Decerialize an untyped list, whose elements are instances of Decerialize. This isn't
-- an instance, since it would have to be an instance of (List a), which conflicts with
-- the above.
decerializeListOf :: (U.ReadCtx m M.ConstMsg, Decerialize a)
    => U.ListOf M.ConstMsg (Cerial M.ConstMsg a) -> m (List a)
decerializeListOf raw = V.generateM (U.length raw) (\i -> U.index i raw >>= decerialize)

-- | Decerialize an untyped list, leaving the elements of the list as-is. The is most
-- interesting for types that go in the data section of a struct, hence the name.
decerializeListOfWord :: (U.ReadCtx m M.ConstMsg)
    => U.ListOf M.ConstMsg a -> m (List a)
decerializeListOfWord raw = V.generateM (U.length raw) (`U.index` raw)

instance Decerialize List' where
    type Cerial msg List' = U.List msg

    decerialize (U.List0 l)      = List0' <$> decerializeListOfWord l
    decerialize (U.List1 l)      = List1' <$> decerializeListOfWord l
    decerialize (U.List8 l)      = List8' <$> decerializeListOfWord l
    decerialize (U.List16 l)     = List16' <$> decerializeListOfWord l
    decerialize (U.List32 l)     = List32' <$> decerializeListOfWord l
    decerialize (U.List64 l)     = List64' <$> decerializeListOfWord l
    decerialize (U.ListPtr l)    = ListPtr' <$> decerializeListOf l
    decerialize (U.ListStruct l) = ListStruct' <$> decerializeListOf l

marshalList' :: U.RWCtx m s => U.List (M.MutMsg s) -> List' -> m ()
marshalList' (U.List0  _) (List0' _)            = pure ()
marshalList' (U.List1 raw) (List1' l)           = marshalListOfWord raw l
marshalList' (U.List8 raw) (List8' l)           = marshalListOfWord raw l
marshalList' (U.List16 raw) (List16' l)         = marshalListOfWord raw l
marshalList' (U.List32 raw) (List32' l)         = marshalListOfWord raw l
marshalList' (U.List64 raw) (List64' l)         = marshalListOfWord raw l
marshalList' (U.ListPtr raw) (ListPtr' l)       = error "TODO"
marshalList' (U.ListStruct raw) (ListStruct' l) = error "TODO"
marshalList' _ _ = throwM $ E.SchemaViolationError "mismatched list types"

marshalListOfWord :: U.RWCtx m s => U.ListOf (M.MutMsg s) a -> List a -> m ()
marshalListOfWord raw l =
    forM_ [0..length l - 1] $ \i ->
        U.setIndex (l V.! i) i raw

cerializeList' :: U.RWCtx m s => M.MutMsg s -> List' -> m (U.List (M.MutMsg s))
cerializeList' = error "TODO"

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
