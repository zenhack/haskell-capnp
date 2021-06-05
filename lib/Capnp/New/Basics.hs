{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Capnp.New.Basics
    ( Text
    , Data
    , AnyPointer
    , AnyList
    , AnyStruct
    , Capability
    ) where

import qualified Capnp.New.Classes as C
import qualified Capnp.Repr        as R
import qualified Capnp.Untyped     as U
import qualified Data.ByteString   as BS
import           Data.Foldable     (for_)
import qualified Data.Vector       as V
import           Data.Word

data Text
data Data
data AnyPointer
data AnyList
data AnyStruct
data Capability

type instance R.ReprFor Data = R.ReprFor (R.List Word8)
type instance R.ReprFor Text = R.ReprFor (R.List Word8)
type instance R.ReprFor AnyPointer = 'R.Ptr 'Nothing
type instance R.ReprFor AnyList = 'R.Ptr ('Just ('R.List 'Nothing))
type instance R.ReprFor AnyStruct = 'R.Ptr ('Just 'R.Struct)
type instance R.ReprFor Capability = 'R.Ptr ('Just 'R.Cap)

data instance C.Parsed AnyPointer
    = PtrNull
    | PtrStruct (C.Parsed AnyStruct)
    | PtrList (C.Parsed AnyList)
    | PtrCap (C.Parsed Capability)
    deriving(Show, Eq)

data instance C.Parsed AnyStruct = Struct
    { structData :: V.Vector Word64
    , structPtrs :: V.Vector (C.Parsed AnyPointer)
    }
    deriving(Show, Eq)

-- TODO(cleanup): It would be nice if we could reuse Capnp.Repr.Parsed.Parsed
-- here, but that would cause a circular import dependency.
type ParsedList a = V.Vector a

data instance C.Parsed AnyList
    = ListPtr (ParsedList (C.Parsed AnyPointer))
    | ListStruct (ParsedList (C.Parsed AnyStruct))
    | List0 (ParsedList ())
    | List1 (ParsedList Bool)
    | List8 (ParsedList Word8)
    | List16 (ParsedList Word16)
    | List32 (ParsedList Word32)
    | List64 (ParsedList Word64)
    deriving(Show, Eq)

data instance C.Parsed Capability -- TODO
    deriving(Show, Eq)

instance C.Allocate Text where
    type AllocHint Text = Int
    new len msg = R.Raw <$> U.allocList8 msg (len + 1)

-- Instances for Data
instance C.Parse Data BS.ByteString where
    parse = U.rawBytes . R.fromRaw
    encode msg value = do
        raw <- C.new (BS.length value) msg
        C.marshalInto raw value
        pure raw

instance C.Allocate Data where
    type AllocHint Data = Int
    new len msg = R.Raw <$> U.allocList8 msg len

instance C.Marshal Data BS.ByteString where
    marshalInto (R.Raw list) bytes =
        for_ [0..BS.length bytes - 1] $ \i ->
            U.setIndex (BS.index bytes i) i list

-- Instances for AnyStruct
instance C.Allocate AnyStruct where
    type AllocHint AnyStruct = (Word16, Word16)
    new (nWords, nPtrs) msg = R.Raw <$> U.allocStruct msg nWords nPtrs
