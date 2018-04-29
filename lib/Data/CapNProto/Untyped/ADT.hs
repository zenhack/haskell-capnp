{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
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
    , List(..)
    , Text(..)
    , Data(..)
    , length
    , sliceIndex

    -- Converting from Data.CapNProto.Untyped.
    , readStruct
    )
  where

import Prelude hiding (length, readList)

import Data.String  (IsString)
import GHC.Exts     (IsList)
import GHC.Generics (Generic)

import qualified Data.ByteString        as BS
import qualified Data.CapNProto.Untyped as U
import           Data.Default           (Default(def))
import           Data.Primitive.Array   (Array)
import qualified Data.Vector            as V
import           Data.Word

type Cap = Word32
type Data = BS.ByteString

newtype Slice a = Slice (List a)
    deriving(Generic, Show, Read, Eq, Ord, Functor, IsList, Default)

newtype Message = Message (Array BS.ByteString)
    deriving(Generic, Show, Read, Eq, Ord, IsList)

newtype Text = Text { toBytes :: BS.ByteString }
    deriving(Generic, Show, Read, Eq, Ord, IsString)

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

newtype List a = List { toVector :: V.Vector a }
    deriving(Generic, Show, Read, Eq, Ord, Functor, IsList, Foldable, Traversable)

instance Default (List a) where
    def = List V.empty

length :: List a -> Int
length (List vec) = V.length vec

sliceIndex :: Default a => Int -> Slice a -> a
sliceIndex i (Slice (List vec))
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
    List <$> V.generateM (U.length list) (\i -> U.index i list >>= readElt)

readList' :: U.ReadCtx m BS.ByteString => U.List BS.ByteString -> m List'
readList' (U.List0 l)      = List0' <$> readList l pure
readList' (U.List1 l)      = List1' <$> readList l pure
readList' (U.List8 l)      = List8' <$> readList l pure
readList' (U.List16 l)     = List16' <$> readList l pure
readList' (U.List32 l)     = List32' <$> readList l pure
readList' (U.List64 l)     = List64' <$> readList l pure
readList' (U.ListPtr l)    = ListPtr' <$> readList l readPtr
readList' (U.ListStruct l) = ListStruct' <$> readList l readStruct
