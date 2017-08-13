{-# LANGUAGE GADTs, RecordWildCards #-}
{-|
Module: Data.CapNProto.Untyped
Description: Utilities for reading capnproto messages with no schema.

The types and functions in this module know about things like structs and
lists, but are not schema aware.

Each of the data types exported by this module is parametrized over a Blob
instance, used as the underlying storage.
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
import Data.CapNProto.Pointer (ElementSize(..))
import Data.CapNProto.Address (Addr(WordAddr), WordAddr(..), resolvePtr)
import qualified Data.CapNProto.Errors as E
import Data.CapNProto.Blob (Blob)
import Data.CapNProto.Bits (WordCount(..))
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
        -> WordCount -- size of the list in words.
        -> PtrTo b (List b)
    PtrToListNormal
        :: NormalList b
        -> ElementSize
        -> PtrTo b (List b)

data AbsWord b = AbsWord (M.Message b) WordAddr Int

data NormalList b = NormalList (M.Message b) WordAddr Int

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
    ListOfBool   :: NormalList b -> ListOf b Bool
    ListOfWord8  :: NormalList b -> ListOf b Word8
    ListOfWord16 :: NormalList b -> ListOf b Word16
    ListOfWord32 :: NormalList b -> ListOf b Word32
    ListOfWord64 :: NormalList b -> ListOf b Word64
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


-- | Returns the value pointed to by a pointer. Deducts 1 from the quota.
get :: (MonadQuota m, MonadThrow m, Blob m b) => PtrTo b a -> m a
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
            P.ListPtr _ (P.EltNormal sz len) -> PtrList $
                PtrToListNormal
                    (NormalList
                        msg
                        (asWordAddr $ resolvePtr addr p)
                        (fromIntegral len))
                    sz
    get' (PtrToStruct msg struct) = return struct
    get' (PtrToListNormal nlist@(NormalList _ _ len) sz) = return $ case sz of
        Sz0  -> List0  (ListOfVoid    len)
        Sz1  -> List1  (ListOfBool    nlist)
        Sz8  -> List8  (ListOfWord8   nlist)
        Sz16 -> List16 (ListOfWord16  nlist)
        Sz32 -> List32 (ListOfWord32  nlist)
        Sz64 -> List64 (ListOfWord64  nlist)
    -- an unsafe downcast Addr -> WordAddr; we use this in a couple places
    -- where we *know* it's a word address.
    asWordAddr (WordAddr addr) = addr
    asWordAddr _ = error "Pointer is not a word address!"

getSubWord :: (MonadQuota m, MonadThrow m, Integral a, Blob m b)
    => AbsWord b -> m a
getSubWord (AbsWord msg addr shift) = do
    word <- M.getWord addr msg
    return $ fromIntegral $ word `shiftR` shift

-- | @index i list@ returns a pointer to the ith element in @list@. Deducts
-- 1 from the quota
index :: (MonadQuota m, MonadThrow m, Blob m b)
    => Int -> ListOf b a -> m (PtrTo b a)
index i list = invoice 1 >> index' i list
  where
    index' :: (MonadQuota m, MonadThrow m) => Int -> ListOf b a -> m (PtrTo b a)
    index' i (ListOfVoid len)
        | i < len = return PtrToVoid
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1 }
    index' i (ListOfStruct msg (Struct _ addr@WordAt{..} dataSz ptrSz) len)
        | i < len = do
            let offset = WordCount $ i * fromIntegral (dataSz + ptrSz)
            let addr' = addr { wordIndex = wordIndex + offset }
            return $ PtrToStruct msg $ Struct msg addr' dataSz ptrSz
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1}
    index' i (ListOfBool   nlist) = indexNList nlist PtrToBool  64
    index' i (ListOfWord8  nlist) = indexNList nlist PtrToWord8  8
    index' i (ListOfWord16 nlist) = indexNList nlist PtrToWord16 4
    index' i (ListOfWord32 nlist) = indexNList nlist PtrToWord32 2
    index' i (ListOfWord64 (NormalList msg addr@WordAt{..} len))
        | i < len = return $ PtrToWord64 msg addr { wordIndex = wordIndex + WordCount i }
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1}
    index' i (ListOfPtr msg addr@WordAt{..} len)
        | i < len = return $ PtrToPtr msg addr { wordIndex = wordIndex + WordCount i }
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1}
    indexNList :: MonadThrow m => NormalList b -> (AbsWord b -> PtrTo b a) -> Int -> m (PtrTo b a)
    indexNList (NormalList msg addr@WordAt{..} len) cons sz
        | i < len = return $ cons $
            AbsWord msg addr { wordIndex = wordIndex + WordCount (i `div` sz) }
                             ((i `mod` sz) * sz)
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1 }


-- | Returns the length of a list
length :: (MonadQuota m, MonadThrow m, Blob m b) => ListOf b a -> m Int
length (ListOfVoid len) = return len
length (ListOfStruct _ _ len) = return len
length (ListOfBool   nlist) = nLen nlist
length (ListOfWord8  nlist) = nLen nlist
length (ListOfWord16 nlist) = nLen nlist
length (ListOfWord32 nlist) = nLen nlist
length (ListOfWord64 nlist) = nLen nlist
length (ListOfPtr _ _ len) = return len

nLen :: (Monad m) => NormalList b -> m Int
nLen (NormalList _ _ len) = return len

-- | Returns the data section of a struct, as a list of Word64
dataSection :: (MonadQuota m, MonadThrow m, Blob m b)
    => Struct b -> m (ListOf b Word64)
dataSection (Struct msg addr dataSz _) =
    return $ ListOfWord64 $ NormalList msg addr (fromIntegral dataSz)

-- | Returns the pointer section of a struct, as a list of Ptr
ptrSection :: (MonadQuota m, MonadThrow m, Blob m b)
    => Struct b -> m (ListOf b (Maybe (Ptr b)))
ptrSection (Struct msg addr@WordAt{..} dataSz ptrSz) =
    return $ ListOfPtr
        msg
        addr { wordIndex = wordIndex + fromIntegral dataSz }
        (fromIntegral ptrSz)

-- | Returns the root pointer of a message.
rootPtr :: (MonadQuota m, MonadThrow m, Blob m b)
    => M.Message b -> m (Maybe (Ptr b))
rootPtr msg = get (PtrToPtr msg (WordAt 0 0))
