{-# LANGUAGE GADTs, RecordWildCards #-}
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

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Quota (MonadQuota, invoice)
import qualified Data.CapNProto.Message as M
import qualified Data.CapNProto.Pointer as P
import Data.CapNProto.Address (Addr(WordAddr), WordAddr(..), resolvePtr)
import qualified Data.CapNProto.Errors as E
import Data.CapNProto.Blob (Blob)
import Data.Bits
import Data.Word

import Prelude hiding (length)

-- | A pointer to a value (of arbitrary type) in a message.
data Ptr b
    = PtrCap Word32
    | PtrFar (PtrTo b (Maybe (Ptr b)))
    | PtrList (PtrTo b (List b))
    | PtrStruct (PtrTo b (Struct b))

-- | A list of values (of arbitrary type) in a message.
data List b
    = List0 (ListOf b ())
    | List1 (ListOf b Bool)
    | List8 (ListOf b Word8)
    | List16 (ListOf b Word16)
    | List32 (ListOf b Word32)
    | List64 (ListOf b Word64)
    | ListPtr (ListOf b (Ptr b))
    | ListStruct (ListOf b (Struct b))

-- | A Pointer to a value of type 'a' inside a message.
data PtrTo b a where
    PtrToVoid :: PtrTo b ()
    PtrToBool :: AbsWord b -> PtrTo b Bool
    PtrToWord8 :: AbsWord b -> PtrTo b Word8
    PtrToWord16 :: AbsWord b -> PtrTo b Word16
    PtrToWord32 :: AbsWord b -> PtrTo b Word32
    PtrToWord64 :: M.Message b -> WordAddr -> PtrTo b Word64
    PtrToPtr :: M.Message b -> WordAddr -> PtrTo b (Maybe (Ptr b))
    PtrToStruct
        :: M.Message b
        -> Struct b
        -> PtrTo b (Struct b)
    PtrToListComposite
        :: M.Message b
        -> WordAddr -- address of the list's *tag*
        -> Int -- size of the list in words.
        -> PtrTo b (List b)
    PtrToListNormal
        :: M.Message b
        -> WordAddr -- address of the start of the list
        -> Int -- Number of elements
        -> PtrTo b (List b)

data AbsWord b = AbsWord (M.Message b) WordAddr Int

-- | A list of values of type 'a' in a message.
data ListOf b a where
    ListOfVoid
        :: Int -- number of elements
        -> ListOf b ()
    ListOfStruct
        :: M.Message b
        -> Struct b -- First element. data/ptr sizes are the same for
                    -- all elements.
        -> Int -- Number of elements
        -> ListOf b (Struct b)
    ListOfWord64
        :: M.Message b
        -> WordAddr -- Start of list
        -> Int -- length, in elements
        -> ListOf b Word64
    ListOfPtr
        -- arguments are the same as with Word64
        :: M.Message b
        -> WordAddr
        -> Int
        -> ListOf b (Maybe (Ptr b))

-- | A struct value in a message.
data Struct b
    = Struct
        (M.Message b)
        WordAddr -- Start of struct
        Word16 -- Data section size.
        Word16 -- Pointer section size.

-- | @index i list@ returns a pointer to the ith element in @list@. Deducts
-- 1 from the quota
index :: (MonadQuota m, MonadThrow m, Blob m b)
    => Int -> ListOf b a -> m (PtrTo b a)

-- | Returns the length of a list
length :: (MonadQuota m, MonadThrow m, Blob m b) => ListOf b a -> m Int

-- | Returns the value pointed to by a pointer. Deducts 1 from the quota.
get :: (MonadQuota m, MonadThrow m, Blob m b) => PtrTo b a -> m a

-- | Returns the data section of a struct, as a list of Word64
dataSection :: (MonadQuota m, MonadThrow m, Blob m b)
    => Struct b -> m (ListOf b Word64)

-- | Returns the pointer section of a struct, as a list of Ptr
ptrSection :: (MonadQuota m, MonadThrow m, Blob m b)
    => Struct b -> m (ListOf b (Maybe (Ptr b)))

-- | Returns the root pointer of a message.
rootPtr :: (MonadQuota m, MonadThrow m, Blob m b)
    => M.Message b -> m (Maybe (Ptr b))

get ptr = invoice 1 >> get' ptr
  where
    get' :: (MonadQuota m, MonadThrow m, Blob m b) => PtrTo b a -> m a
    get' PtrToVoid = return ()
    get' (PtrToBool (AbsWord msg addr shift)) = do
        word <- M.getWord addr msg
        return $ ((word `shiftR` shift) .&. 1) == 1
    get' (PtrToWord8 absWord) = getSubWord absWord
    get' (PtrToWord16 absWord) = getSubWord absWord
    get' (PtrToWord32 absWord) = getSubWord absWord
    get' (PtrToWord64 msg addr) = M.getWord addr msg
    get' (PtrToPtr msg addr@WordAt{..}) = do
        word <- M.getWord addr msg
        return $ flip fmap (P.parsePtr word) $ \p -> case p of
            P.StructPtr offset dataSz ptrSz ->
                PtrStruct $ PtrToStruct msg $ Struct
                    msg
                    (asWordAddr $ resolvePtr addr p)
                    dataSz
                    ptrSz
            P.CapPtr cap -> PtrCap cap
            P.ListPtr _ (P.EltComposite len) -> PtrList $
                PtrToListComposite
                    msg
                    (asWordAddr $ resolvePtr addr p)
                    (fromIntegral len)
            P.ListPtr _ (P.EltNormal _ len) -> PtrList $
                PtrToListNormal
                    msg
                    (asWordAddr $ resolvePtr addr p)
                    (fromIntegral len)
            _ -> undefined
    get' (PtrToStruct msg struct) = return struct
    -- an unsafe downcast Addr -> WordAddr; we use this in a couple places
    -- where we *know* it's a word address.
    asWordAddr (WordAddr addr) = addr
    asWordAddr _ = error "Pointer is not a word address!"

getSubWord :: (MonadQuota m, MonadThrow m, Integral a, Blob m b)
    => AbsWord b -> m a
getSubWord (AbsWord msg addr shift) = do
    word <- M.getWord addr msg
    return $ fromIntegral $ word `shiftR` shift

index i list = invoice 1 >> index' i list
  where
    index' :: (MonadQuota m, MonadThrow m) => Int -> ListOf b a -> m (PtrTo b a)
    index' i (ListOfVoid len)
        | i < len = return PtrToVoid
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1 }
    index' i (ListOfStruct msg (Struct _ addr@WordAt{..} dataSz ptrSz) len)
        | i < len = do
            let offset = i * fromIntegral (dataSz + ptrSz)
            let addr' = addr { wordIndex = wordIndex + offset }
            return $ PtrToStruct msg $ Struct msg addr' dataSz ptrSz
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1}
    index' i (ListOfWord64 msg addr@WordAt{..} len)
        | i < len = return $ PtrToWord64 msg addr { wordIndex = wordIndex + i }
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1}
    index' i (ListOfPtr msg addr@WordAt{..} len)
        | i < len = return $ PtrToPtr msg addr { wordIndex = wordIndex + i }
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1}


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
