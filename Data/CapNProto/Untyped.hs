{-|
Module: Data.CapNProto.Untyped
Description: Utilities for manipulating capnproto messages with no schema.

The types and functions in this module know about things like structs and
lists, but are not schema aware.
-}
module Data.CapNProto.Untyped
    ( Ptr(..), List(..)
    , Struct, PtrTo, ListOf
    , dataSection, ptrSection
    , get, index
    )
  where

-- Just mocking up the API for now.

-- Questions:
--
-- * Enforce the binding from value to message somehow? Have a couple ideas
--   of how to do this.

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Quota (MonadQuota)
import qualified Data.CapNProto.Message as M
import Data.Word

data Ptr
    = PtrCap Word32
    | PtrFar (PtrTo Ptr)
    | PtrList (PtrTo List)
    | PtrStruct (PtrTo Struct)

data List
    = List0 (ListOf ())
    | List1 (ListOf Bool)
    | List8 (ListOf Word8)
    | List16 (ListOf Word16)
    | List32 (ListOf Word32)
    | List64 (ListOf Word64)
    | ListPtr (ListOf Ptr)
    | ListStruct (ListOf Struct)

data PtrTo a
data ListOf a
data Struct

index :: (MonadQuota m, MonadThrow m, M.Message msg seg)
    => Int -> ListOf a -> msg (seg Word64) -> m (PtrTo a)
get :: (MonadQuota m, MonadThrow m, M.Message msg seg)
    => PtrTo a -> msg (seg Word64) -> m a
dataSection :: (MonadQuota m, MonadThrow m) => Struct -> m (ListOf Word64)
ptrSection  :: (MonadQuota m, MonadThrow m) => Struct -> m (ListOf Ptr)


index = undefined
get = undefined
dataSection = undefined
ptrSection = undefined
