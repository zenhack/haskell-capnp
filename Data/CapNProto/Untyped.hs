{-# LANGUAGE GADTs, KindSignatures, RecordWildCards #-}
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
    , get, index, length
    )
  where

-- Questions:
--
-- * Enforce the binding from value to message somehow? Have a couple ideas
--   of how to do this.

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Quota (MonadQuota, invoice)
import qualified Data.CapNProto.Message as M
import qualified Data.CapNProto.Pointer as P
import Data.CapNProto.Address (WordAddr(..))
import qualified Data.CapNProto.Errors as E
import Data.Bits
import Data.Word

import Prelude hiding (length)

data Ptr msg seg
    = PtrCap Word32
    | PtrFar (PtrTo msg seg (Ptr msg seg))
    | PtrList (PtrTo msg seg (List msg seg))
    | PtrStruct (PtrTo msg seg (Struct msg seg))

data List msg seg
    = List0 (ListOf msg seg ())
    | List1 (ListOf msg seg Bool)
    | List8 (ListOf msg seg Word8)
    | List16 (ListOf msg seg Word16)
    | List32 (ListOf msg seg Word32)
    | List64 (ListOf msg seg Word64)
    | ListPtr (ListOf msg seg (Ptr msg seg))
    | ListStruct (ListOf msg seg (Struct msg seg))

data PtrTo msg seg a where
    PtrToVoid :: PtrTo msg seg ()
    PtrToBool :: AbsWord msg seg -> PtrTo msg seg Bool
    PtrToWord8 :: AbsWord msg seg -> PtrTo msg seg Word8
    PtrToWord16 :: AbsWord msg seg -> PtrTo msg seg Word16
    PtrToWord32 :: AbsWord msg seg -> PtrTo msg seg Word32
    PtrToWord64 :: msg (seg Word64) -> WordAddr -> PtrTo msg seg Word64
    PtrToPtr :: msg (seg Word64) -> WordAddr -> PtrTo msg seg (Ptr msg seg)
    PtrToStruct
        :: msg (seg Word64)
        -> Struct msg seg
        -> PtrTo msg seg (Struct msg seg)

data AbsWord msg seg = AbsWord (msg (seg Word64)) WordAddr Int

data ListOf msg seg a where
    ListOfVoid
        :: Int -- number of elements
        -> ListOf msg seg ()
    ListOfStruct
        :: msg (seg Word64)
        -> Struct msg seg -- First element. data/ptr sizes are the same for
                          -- all elements.
        -> Int -- Number of elements
        -> ListOf msg seg (Struct msg seg)

data Struct msg seg
    = Struct
        (msg (seg Word64))
        WordAddr -- Start of struct
        Word16 -- Data section size.
        Word16 -- Pointer section size.

index :: (MonadQuota m, MonadThrow m, M.Message msg seg)
    => Int -> ListOf msg seg a ->  m (PtrTo msg seg a)
length :: (MonadQuota m, MonadThrow m, M.Message msg seg)
    => ListOf msg seg a -> m Int
get :: (MonadQuota m, MonadThrow m, M.Message msg seg)
    => PtrTo msg seg a -> m a
dataSection :: (MonadQuota m, MonadThrow m)
    => Struct msg seg -> m (ListOf msg seg Word64)
ptrSection  :: (MonadQuota m, MonadThrow m)
    => Struct msg seg -> m (ListOf msg seg (Ptr msg seg))

get PtrToVoid = return ()
get (PtrToBool (AbsWord msg addr shift)) = do
    word <- M.getWord addr msg
    return $ ((word `shiftR` shift) .&. 1) == 1
get (PtrToWord8 absWord) = getSubWord absWord
get (PtrToWord16 absWord) = getSubWord absWord
get (PtrToWord32 absWord) = getSubWord absWord
get (PtrToWord64 msg addr) = M.getWord addr msg
get (PtrToPtr msg addr@WordAt{..}) = do
    word <- M.getWord addr msg
    case P.parsePtr word of
        P.StructPtr offset dataSz ptrSz ->
            return $ PtrStruct $ PtrToStruct msg $ Struct
                msg
                addr { wordIndex = wordIndex + 1 + fromIntegral offset }
                dataSz
                ptrSz
        P.CapPtr cap -> return $ PtrCap cap
        _ -> undefined
get (PtrToStruct msg struct) = return struct

getSubWord :: (MonadQuota m, MonadThrow m, M.Message msg seg, Integral a)
    => AbsWord msg seg -> m a
getSubWord (AbsWord msg addr shift) = do
    word <- M.getWord addr msg
    return $ fromIntegral $ word `shiftR` shift

index i (ListOfVoid len)
    | i < len = invoice 1 >> return PtrToVoid
    | otherwise = throwM $ E.BoundsError { E.index = i, E.maxIndex = len - 1 }
index i (ListOfStruct msg (Struct _ addr@WordAt{..} dataSz ptrSz) len)
    | i < len = do
        let offset = i * (fromIntegral $ dataSz + ptrSz)
        let addr' = addr { wordIndex = wordIndex + offset }
        return $ PtrToStruct msg $ Struct msg addr' dataSz ptrSz
    | otherwise = throwM $ E.BoundsError { E.index = i, E.maxIndex = len }

length = undefined
dataSection = undefined
ptrSection = undefined
