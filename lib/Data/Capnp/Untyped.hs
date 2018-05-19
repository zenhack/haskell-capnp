{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module: Data.Capnp.Untyped
Description: Utilities for reading capnproto messages with no schema.

The types and functions in this module know about things like structs and
lists, but are not schema aware.

Each of the data types exported by this module is parametrized over a Blob
instance, used as the underlying storage.
-}
module Data.Capnp.Untyped
    ( Ptr(..), List(..), Struct, ListOf
    , dataSection, ptrSection
    , getData, getPtr
    , get, index, length
    , emptyList
    , rootPtr
    , rawBytes
    , ReadCtx
    )
  where

import Prelude hiding (length)

import Data.Bits
import Data.Word

import Control.Monad.Catch       (MonadThrow(throwM))
import Data.Capnp.Address        (WordAddr(..))
import Data.Capnp.Bits
    (ByteCount(..), Word1(..), WordCount(..), wordsToBytes)
import Data.Capnp.Blob           (Blob, Slice(..))
import Data.Capnp.Pointer        (ElementSize(..))
import Data.Capnp.TraversalLimit (MonadLimit(invoice))

import qualified Data.Capnp.Errors  as E
import qualified Data.Capnp.Message as M
import qualified Data.Capnp.Pointer as P

-- | Type (constraint) synonym for the constraints needed for most read
-- operations.
type ReadCtx m b = (MonadThrow m, MonadLimit m, Blob m b, Slice m b)

-- | A an absolute pointer to a value (of arbitrary type) in a message.
-- Note that there is no variant for far pointers, which don't make sense
-- with absolute addressing.
data Ptr b
    = PtrCap !Word32
    | PtrList (List b)
    | PtrStruct (Struct b)

-- | A list of values (of arbitrary type) in a message.
data List b
    = List0 (ListOf b ())
    | List1 (ListOf b Bool)
    | List8 (ListOf b Word8)
    | List16 (ListOf b Word16)
    | List32 (ListOf b Word32)
    | List64 (ListOf b Word64)
    | ListPtr (ListOf b (Maybe (Ptr b)))
    | ListStruct (ListOf b (Struct b))

-- | A "normal" (non-composite) list.
data NormalList b = NormalList
    { nMsg  :: M.Message b
    , nAddr :: WordAddr
    , nLen  :: Int
    }

-- | A list of values of type 'a' in a message.
data ListOf b a where
    ListOfVoid
        :: !Int -- number of elements
        -> ListOf b ()
    ListOfStruct
        :: Struct b -- First element. data/ptr sizes are the same for
                    -- all elements.
        -> !Int -- Number of elements
        -> ListOf b (Struct b)
    ListOfBool   :: !(NormalList b) -> ListOf b Bool
    ListOfWord8  :: !(NormalList b) -> ListOf b Word8
    ListOfWord16 :: !(NormalList b) -> ListOf b Word16
    ListOfWord32 :: !(NormalList b) -> ListOf b Word32
    ListOfWord64 :: !(NormalList b) -> ListOf b Word64
    ListOfPtr    :: !(NormalList b) -> ListOf b (Maybe (Ptr b))
    -- wrapper that converts an untyped value to a typed one:
    ListOfMapped :: ListOf b a -> (a -> c) -> ListOf b c


instance Functor (ListOf b) where
    fmap f (ListOfMapped list g) = ListOfMapped list (f . g)
    fmap f list                  = ListOfMapped list f

-- | A struct value in a message.
data Struct b
    = Struct
        (M.Message b)
        !WordAddr -- Start of struct
        !Word16 -- Data section size.
        !Word16 -- Pointer section size.


-- | An empty list, of any type.
emptyList :: ListOf b a
emptyList = ListOfMapped (ListOfVoid 0) undefined


-- | @get msg addr@ returns the Ptr stored at @addr@ in @msg@.
-- Deducts 1 from the quota for each word read (which may be multiple in the
-- case of far pointers).
get :: ReadCtx m b => M.Message b -> WordAddr -> m (Maybe (Ptr b))
get msg addr = do
    word <- getWord msg addr
    case P.parsePtr word of
        Nothing -> return Nothing
        Just p -> case p of
            P.CapPtr cap -> return $ Just $ PtrCap cap
            P.StructPtr off dataSz ptrSz -> return $ Just $ PtrStruct $
                Struct msg (resolveOffset addr off) dataSz ptrSz
            P.ListPtr off eltSpec -> Just <$> getList (resolveOffset addr off) eltSpec
            P.FarPtr twoWords offset segment -> do
                let addr' = WordAt { wordIndex = fromIntegral offset
                                   , segIndex = fromIntegral segment
                                   }
                if not twoWords
                    then get msg addr'
                    else do
                        landingPad <- getWord msg addr'
                        case P.parsePtr landingPad of
                            Just (P.FarPtr False off seg) -> do
                                tagWord <- getWord
                                            msg
                                            addr' { wordIndex = wordIndex addr' + 1 }
                                let finalAddr = WordAt { wordIndex = fromIntegral off
                                                       , segIndex = fromIntegral seg
                                                       }
                                case P.parsePtr tagWord of
                                    Just (P.StructPtr 0 dataSz ptrSz) ->
                                        return $ Just $ PtrStruct $
                                            Struct msg finalAddr dataSz ptrSz
                                    Just (P.ListPtr 0 eltSpec) ->
                                        Just <$> getList finalAddr eltSpec
                                    -- TODO: I'm not sure whether far pointers to caps are
                                    -- legal; it's clear how they would work, but I don't
                                    -- see a use, and the spec is unclear. Should check
                                    -- how the reference implementation does this, copy
                                    -- that, and submit a patch to the spec.
                                    Just (P.CapPtr cap) ->
                                        return $ Just $ PtrCap cap
                                    ptr -> throwM $ E.InvalidDataError $
                                        "The tag word of a far pointer's " ++
                                        "2-word landing pad should be an intra " ++
                                        "segment pointer with offset 0, but " ++
                                        "we read " ++ show ptr
                            ptr -> throwM $ E.InvalidDataError $
                                "The first word of a far pointer's 2-word " ++
                                "landing pad should be another far pointer " ++
                                "(with a one-word landing pad), but we read " ++
                                show ptr

  where
    getWord msg addr = invoice 1 *> M.getWord msg addr
    resolveOffset addr@WordAt{..} off =
        addr { wordIndex = wordIndex + fromIntegral off + 1 }
    getList addr@WordAt{..} eltSpec = PtrList <$>
        case eltSpec of
            P.EltNormal sz len -> pure $ case sz of
                Sz0   -> List0  (ListOfVoid    (fromIntegral len))
                Sz1   -> List1  (ListOfBool    nlist)
                Sz8   -> List8  (ListOfWord8   nlist)
                Sz16  -> List16 (ListOfWord16  nlist)
                Sz32  -> List32 (ListOfWord32  nlist)
                Sz64  -> List64 (ListOfWord64  nlist)
                SzPtr -> ListPtr (ListOfPtr nlist)
              where
                nlist = NormalList msg addr (fromIntegral len)
            P.EltComposite _ -> do
                tagWord <- getWord msg addr
                case P.parsePtr tagWord of
                    Just (P.StructPtr numElts dataSz ptrSz) ->
                        pure $ ListStruct $ ListOfStruct
                            (Struct msg
                                    addr { wordIndex = wordIndex + 1 }
                                    dataSz
                                    ptrSz)
                            (fromIntegral numElts)
                    tag -> throwM $ E.InvalidDataError $
                        "Composite list tag was not a struct-" ++
                        "formatted word: " ++ show tag


-- | @index i list@ returns the ith element in @list@. Deducts 1 from the quota
index :: ReadCtx m b => Int -> ListOf b a -> m a
index i list = invoice 1 >> index' list
  where
    index' :: ReadCtx m b => ListOf b a -> m a
    index' (ListOfVoid len)
        | i < len = pure ()
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1 }
    index' (ListOfStruct (Struct msg addr@WordAt{..} dataSz ptrSz) len)
        | i < len = do
            let offset = WordCount $ i * fromIntegral (dataSz + ptrSz)
            let addr' = addr { wordIndex = wordIndex + offset }
            return $ Struct msg addr' dataSz ptrSz
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1}
    index' (ListOfBool   nlist) = do
        Word1 val <- indexNList nlist 64
        pure val
    index' (ListOfWord8  nlist) = indexNList nlist 8
    index' (ListOfWord16 nlist) = indexNList nlist 4
    index' (ListOfWord32 nlist) = indexNList nlist 2
    index' (ListOfWord64 (NormalList msg addr@WordAt{..} len))
        | i < len = M.getWord msg addr { wordIndex = wordIndex + WordCount i }
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1}
    index' (ListOfPtr (NormalList msg addr@WordAt{..} len))
        | i < len = get msg addr { wordIndex = wordIndex + WordCount i }
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1}
    index' (ListOfMapped list f) = f <$> index' list
    indexNList :: (ReadCtx m b, Integral a) => NormalList b -> Int -> m a
    indexNList (NormalList msg addr@WordAt{..} len) eltsPerWord
        | i < len = do
            let wordIndex' = wordIndex + WordCount (i `div` eltsPerWord)
            word <- M.getWord msg addr { wordIndex = wordIndex' }
            let shift = (i `mod` eltsPerWord) * (64 `div` eltsPerWord)
            pure $ fromIntegral $ word `shiftR` shift
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1 }


-- | Returns the length of a list
length :: ListOf b a -> Int
length (ListOfVoid len)      = len
length (ListOfStruct _ len)  = len
length (ListOfBool   nlist)  = nLen nlist
length (ListOfWord8  nlist)  = nLen nlist
length (ListOfWord16 nlist)  = nLen nlist
length (ListOfWord32 nlist)  = nLen nlist
length (ListOfWord64 nlist)  = nLen nlist
length (ListOfPtr    nlist)  = nLen nlist
length (ListOfMapped list _) = length list

-- | The data section of a struct, as a list of Word64
dataSection :: Struct b -> ListOf b Word64
dataSection (Struct msg addr dataSz _) =
    ListOfWord64 $ NormalList msg addr (fromIntegral dataSz)

-- | The pointer section of a struct, as a list of Ptr
ptrSection :: Struct b -> ListOf b (Maybe (Ptr b))
ptrSection (Struct msg addr@WordAt{..} dataSz ptrSz) =
    ListOfPtr $ NormalList
        msg
        addr { wordIndex = wordIndex + fromIntegral dataSz }
        (fromIntegral ptrSz)

-- | @'getData' i struct@ gets the @i@th word from the struct's data section,
-- returning 0 if it is absent.
getData :: ReadCtx m b => Int -> Struct b -> m Word64
getData i struct
    | length (dataSection struct) <= i = 0 <$ invoice 1
    | otherwise = index i (dataSection struct)

-- | @'getPtr' i struct@ gets the @i@th word from the struct's pointer section,
-- returning Nothing if it is absent.
getPtr :: ReadCtx m b => Int -> Struct b -> m (Maybe (Ptr b))
getPtr i struct
    | length (ptrSection struct) <= i = Nothing <$ invoice 1
    | otherwise = index i (ptrSection struct)


-- | @rawBytes list@ returns the raw storage corresponding to the list.
--
-- Caveats:
--
-- * This returns the underlying *storage*, not the value, so e.g.
--   @rawBytes (fmap (+1) list) == rawBytes list@, since fmap does not modify
--   the message.
-- * This doesn't work (raises an error) for composite lists, or lists whose
--   element size is less than < 1 byte (() and Bool).
rawBytes :: (ReadCtx m b, Slice m b) => ListOf b a -> m b
rawBytes (ListOfWord8 (NormalList msg WordAt{..} len)) = do
    seg <- M.getSegment msg segIndex
    slice seg (wordsToBytes wordIndex) (ByteCount len)
rawBytes (ListOfWord16 nlist@NormalList{..}) = rawBytes (ListOfWord8 nlist { nLen = nLen * 2 })
rawBytes (ListOfWord32 nlist@NormalList{..}) = rawBytes (ListOfWord8 nlist { nLen = nLen * 4 })
rawBytes (ListOfWord64 nlist@NormalList{..}) = rawBytes (ListOfWord8 nlist { nLen = nLen * 8 })
rawBytes (ListOfPtr    nlist@NormalList{..}) = rawBytes (ListOfWord8 nlist { nLen = nLen * 8 })
rawBytes (ListOfMapped list _) = rawBytes list
rawBytes _ = throwM $ E.SchemaViolationError
    -- XXX: SchemaViolationError doesn't have *quite* the semantics we want
    -- here. It's *almost* right, in that the caller is asking for a
    -- transformation that assumes a certain shape of data, but we should
    -- come up with something that more clearly expresses what's going on.
    "rawBytes called on something other than a list of bytes."


-- | Returns the root pointer of a message.
rootPtr :: ReadCtx m b => M.Message b -> m (Struct b)
rootPtr msg = do
    root <- get msg (WordAt 0 0)
    case root of
        Just (PtrStruct struct) -> pure struct
        _ -> throwM $ E.SchemaViolationError
                "Unexpected root type; expected struct."
