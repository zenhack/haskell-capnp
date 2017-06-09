{-# LANGUAGE GADTs, KindSignatures, RecordWildCards #-}
{-|
Module: Data.CapNProto.Untyped
Description: Utilities for manipulating capnproto messages with no schema.

The types and functions in this module know about things like structs and
lists, but are not schema aware.

Each of the data types exported by this module is parametrized over at least
@msg@ and @seg@ types; these are the type of the message containing the value,
and are not explicitly called out in every location.
-}
module Data.CapNProto.Untyped
    ( Ptr(..), List(..)
    , Struct, PtrTo, ListOf
    , dataSection, ptrSection
    , get, index, length
    , rootPtr
    )
  where

-- TODO: clear up exactly where we're doing the quota deduction, and document
-- it. Right now, we *mostly* do it it @get@ by virtue of M.getWord, but this
-- doesn't cover elements of void lists. For those, we currently do the invoice
-- in index. Need to make this clear & consistent.

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Quota (MonadQuota, invoice)
import qualified Data.CapNProto.Message as M
import qualified Data.CapNProto.Pointer as P
import Data.CapNProto.Address (WordAddr(..))
import qualified Data.CapNProto.Errors as E
import Data.Bits
import Data.Word

import Prelude hiding (length)

-- | A pointer to a value (of arbitrary type) in a message.
data Ptr msg seg
    = PtrCap Word32
    | PtrFar (PtrTo msg seg (Maybe (Ptr msg seg)))
    | PtrList (PtrTo msg seg (List msg seg))
    | PtrStruct (PtrTo msg seg (Struct msg seg))

-- | A list of values (of arbitrary type) in a message.
data List msg seg
    = List0 (ListOf msg seg ())
    | List1 (ListOf msg seg Bool)
    | List8 (ListOf msg seg Word8)
    | List16 (ListOf msg seg Word16)
    | List32 (ListOf msg seg Word32)
    | List64 (ListOf msg seg Word64)
    | ListPtr (ListOf msg seg (Ptr msg seg))
    | ListStruct (ListOf msg seg (Struct msg seg))

-- | A Pointer to a value of type 'a' inside a message.
data PtrTo msg seg a where
    PtrToVoid :: PtrTo msg seg ()
    PtrToBool :: AbsWord msg seg -> PtrTo msg seg Bool
    PtrToWord8 :: AbsWord msg seg -> PtrTo msg seg Word8
    PtrToWord16 :: AbsWord msg seg -> PtrTo msg seg Word16
    PtrToWord32 :: AbsWord msg seg -> PtrTo msg seg Word32
    PtrToWord64 :: msg (seg Word64) -> WordAddr -> PtrTo msg seg Word64
    PtrToPtr :: msg (seg Word64) -> WordAddr -> PtrTo msg seg (Maybe (Ptr msg seg))
    PtrToStruct
        :: msg (seg Word64)
        -> Struct msg seg
        -> PtrTo msg seg (Struct msg seg)

data AbsWord msg seg = AbsWord (msg (seg Word64)) WordAddr Int

-- | A list of values of type 'a' in a message.
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
    ListOfWord64
        :: msg (seg Word64)
        -> WordAddr -- Start of list
        -> Int -- length, in elements
        -> ListOf msg seg Word64
    ListOfPtr
        -- arguments are the same as with Word64
        :: msg (seg Word64)
        -> WordAddr
        -> Int
        -> ListOf msg seg (Ptr msg seg)

-- | A struct value in a message.
data Struct msg seg
    = Struct
        (msg (seg Word64))
        WordAddr -- Start of struct
        Word16 -- Data section size.
        Word16 -- Pointer section size.

-- | @index i list@ returns a pointer to the ith element in @list@
index :: (MonadQuota m, MonadThrow m, M.Message msg seg)
    => Int -> ListOf msg seg a ->  m (PtrTo msg seg a)

-- | Returns the length of a list
length :: (MonadQuota m, MonadThrow m, M.Message msg seg)
    => ListOf msg seg a -> m Int

-- | Returns the value pointed to by a pointer
get :: (MonadQuota m, MonadThrow m, M.Message msg seg)
    => PtrTo msg seg a -> m a

-- | Returns the data section of a struct, as a list of Word64
dataSection :: (MonadQuota m, MonadThrow m)
    => Struct msg seg -> m (ListOf msg seg Word64)

-- | Returns the pointer section of a struct, as a list of Ptr
ptrSection  :: (MonadQuota m, MonadThrow m)
    => Struct msg seg -> m (ListOf msg seg (Ptr msg seg))

-- | Returns the root pointer of a message.
rootPtr :: (MonadQuota m, MonadThrow m, M.Message msg seg)
    => msg (seg Word64) -> m (Maybe (Ptr msg seg))

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
    return $ flip fmap (P.parsePtr word) $ \p -> case p of
        P.StructPtr offset dataSz ptrSz ->
            PtrStruct $ PtrToStruct msg $ Struct
                msg
                addr { wordIndex = wordIndex + 1 + fromIntegral offset }
                dataSz
                ptrSz
        P.CapPtr cap -> PtrCap cap
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
    | otherwise = throwM $ E.BoundsError { E.index = i, E.maxIndex = len - 1}
index i (ListOfWord64 msg addr@WordAt{..} len)
    | i < len = return $ PtrToWord64 msg addr { wordIndex = wordIndex + i }
    | otherwise = throwM $ E.BoundsError { E.index = i, E.maxIndex = len - 1}


length (ListOfVoid len) = return len
length (ListOfStruct _ _ len) = return len
length (ListOfWord64 _ _ len) = return len
length (ListOfPtr _ _ len) = return len

dataSection (Struct msg addr dataSz _) =
    return $ ListOfWord64 msg addr (fromIntegral dataSz)
ptrSection (Struct msg addr@WordAt{..} dataSz ptrSz) =
    return $ ListOfPtr
        msg
        addr { wordIndex = wordIndex + fromIntegral dataSz }
        (fromIntegral ptrSz)

rootPtr msg = get (PtrToPtr msg (WordAt 0 0))
