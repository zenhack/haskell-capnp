{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-|
Module: Data.Capnp.Untyped.Generic
Description: Utilities for reading capnproto messages with no schema.

The types and functions in this module know about things like structs and
lists, but are not schema aware.

Each of the data types exported by this module is parametrized over a Blob
instance, used as the underlying storage.
-}
module Data.Capnp.Untyped.Generic
    ( Ptr(..), List(..), Struct, ListOf
    , dataSection, ptrSection
    , getData, getPtr
    , get, index, length
    , rootPtr
    , rawBytes
    , extractElts
    , ReadCtx
    , HasMessage(..), MessageDefault(..)
    )
  where

import Prelude hiding (length)

import Data.Bits
import Data.Word

import Control.Monad.Catch       (MonadThrow(throwM))
import Data.Capnp.Address        (WordAddr(..))
import Data.Capnp.Bits
    (ByteCount(..), Word1(..), WordCount(..), wordsToBytes)
import Data.Capnp.Pointer        (ElementSize(..))
import Data.Capnp.TraversalLimit (MonadLimit(invoice))

import qualified Data.ByteString            as BS
import qualified Data.Capnp.Errors          as E
import qualified Data.Capnp.Message.Generic as GM
import qualified Data.Capnp.Pointer         as P

-- | Type (constraint) synonym for the constraints needed for most read
-- operations.
type ReadCtx m = (MonadThrow m, MonadLimit m)

-- | A an absolute pointer to a value (of arbitrary type) in a message.
-- Note that there is no variant for far pointers, which don't make sense
-- with absolute addressing.
data Ptr msg
    = PtrCap msg !Word32
    | PtrList (List msg)
    | PtrStruct (Struct msg)

-- | A list of values (of arbitrary type) in a message.
data List msg
    = List0 (ListOf msg ())
    | List1 (ListOf msg Bool)
    | List8 (ListOf msg Word8)
    | List16 (ListOf msg Word16)
    | List32 (ListOf msg Word32)
    | List64 (ListOf msg Word64)
    | ListPtr (ListOf msg (Maybe (Ptr msg)))
    | ListStruct (ListOf msg (Struct msg))

-- | A "normal" (non-composite) list.
data NormalList msg = NormalList
    { nMsg  :: msg
    , nAddr :: WordAddr
    , nLen  :: Int
    }

-- | A list of values of type 'a' in a message.
data ListOf msg a where
    ListOfVoid
        :: msg
        -> !Int -- number of elements
        -> ListOf msg ()
    ListOfStruct
        :: Struct msg -- First element. data/ptr sizes are the same for
                      -- all elements.
        -> !Int       -- Number of elements
        -> ListOf msg (Struct msg)
    ListOfBool   :: !(NormalList msg) -> ListOf msg Bool
    ListOfWord8  :: !(NormalList msg) -> ListOf msg Word8
    ListOfWord16 :: !(NormalList msg) -> ListOf msg Word16
    ListOfWord32 :: !(NormalList msg) -> ListOf msg Word32
    ListOfWord64 :: !(NormalList msg) -> ListOf msg Word64
    ListOfPtr    :: !(NormalList msg) -> ListOf msg (Maybe (Ptr msg))
    -- wrapper that converts an untyped value to a typed one:
    ListOfMapped :: ListOf msg a -> (a -> c) -> ListOf msg c

    ListOfExtract :: (forall (m :: * -> *). ReadCtx m => a -> m b) -> ListOf msg a -> ListOf msg b

extractElts :: (forall (m :: * -> *). ReadCtx m => a -> m b) -> ListOf msg a -> ListOf msg b
extractElts = ListOfExtract

instance Functor (ListOf msg) where
    fmap f (ListOfMapped list g) = ListOfMapped list (f . g)
    fmap f list                  = ListOfMapped list f

-- | A struct value in a message.
data Struct msg
    = Struct
        msg
        !WordAddr -- Start of struct
        !Word16 -- Data section size.
        !Word16 -- Pointer section size.

-- | Types @a@ whose storage is owned by a message with blob type @b@.
class HasMessage a msg where
    -- | Get the message in which the @a@ is stored.
    message :: a -> msg

-- | Types which have a "default" value, but require a message
-- to construct it.
--
-- The default is usually conceptually zero-size. This is mostly useful
-- for generated code, so that it can use standard decoding techniques
-- on default values.
class HasMessage a msg => MessageDefault a msg where
    messageDefault :: msg -> a

instance HasMessage (Ptr msg) msg where
    message (PtrCap msg _)     = msg
    message (PtrList list)     = message list
    message (PtrStruct struct) = message struct

instance HasMessage (Struct msg) msg where
    message (Struct msg _ _ _) = msg

instance MessageDefault (Struct msg) msg where
    messageDefault msg = Struct msg (WordAt 0 0) 0 0

instance HasMessage (List msg) msg where
    message (List0 list)      = message list
    message (List1 list)      = message list
    message (List8 list)      = message list
    message (List16 list)     = message list
    message (List32 list)     = message list
    message (List64 list)     = message list
    message (ListPtr list)    = message list
    message (ListStruct list) = message list

instance HasMessage (ListOf msg a) msg where
    message (ListOfVoid msg _)     = msg
    message (ListOfStruct tag _)   = message tag
    message (ListOfBool list)      = message list
    message (ListOfWord8 list)     = message list
    message (ListOfWord16 list)    = message list
    message (ListOfWord32 list)    = message list
    message (ListOfWord64 list)    = message list
    message (ListOfPtr list)       = message list
    message (ListOfMapped list _)  = message list
    message (ListOfExtract _ list) = message list

instance MessageDefault (ListOf msg ()) msg where
    messageDefault msg = ListOfVoid msg 0
instance MessageDefault (ListOf msg (Struct msg)) msg where
    messageDefault msg = ListOfStruct (messageDefault msg) 0
instance MessageDefault (ListOf msg Bool) msg where
    messageDefault msg = ListOfBool (messageDefault msg)
instance MessageDefault (ListOf msg Word8) msg where
    messageDefault msg = ListOfWord8 (messageDefault msg)
instance MessageDefault (ListOf msg Word16) msg where
    messageDefault msg = ListOfWord16 (messageDefault msg)
instance MessageDefault (ListOf msg Word32) msg where
    messageDefault msg = ListOfWord32 (messageDefault msg)
instance MessageDefault (ListOf msg Word64) msg where
    messageDefault msg = ListOfWord64 (messageDefault msg)
instance MessageDefault (ListOf msg (Maybe (Ptr msg))) msg where
    messageDefault msg = ListOfPtr (messageDefault msg)

instance HasMessage (NormalList msg) msg where
    message = nMsg

instance MessageDefault (NormalList msg) msg where
    messageDefault msg = NormalList msg (WordAt 0 0) 0

-- | @get msg addr@ returns the Ptr stored at @addr@ in @msg@.
-- Deducts 1 from the quota for each word read (which may be multiple in the
-- case of far pointers).
get :: (GM.Message m msg seg, ReadCtx m) => msg -> WordAddr -> m (Maybe (Ptr msg))
get msg addr = do
    word <- getWord msg addr
    case P.parsePtr word of
        Nothing -> return Nothing
        Just p -> case p of
            P.CapPtr cap -> return $ Just $ PtrCap msg cap
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
                                        return $ Just $ PtrCap msg cap
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
    getWord msg addr = invoice 1 *> GM.getWord msg addr
    resolveOffset addr@WordAt{..} off =
        addr { wordIndex = wordIndex + fromIntegral off + 1 }
    getList addr@WordAt{..} eltSpec = PtrList <$>
        case eltSpec of
            P.EltNormal sz len -> pure $ case sz of
                Sz0   -> List0  (ListOfVoid    msg (fromIntegral len))
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
index :: (GM.Message m msg seg, ReadCtx m) => Int -> ListOf msg a -> m a
index i list = invoice 1 >> index' list
  where
    index' :: (GM.Message m msg seg, ReadCtx m) => ListOf msg a -> m a
    index' (ListOfVoid _ len)
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
        | i < len = GM.getWord msg addr { wordIndex = wordIndex + WordCount i }
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1}
    index' (ListOfPtr (NormalList msg addr@WordAt{..} len))
        | i < len = get msg addr { wordIndex = wordIndex + WordCount i }
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1}
    index' (ListOfMapped list f) = f <$> index' list
    index' (ListOfExtract f list) = index' list >>= f
    indexNList :: (GM.Message m msg seg, ReadCtx m, Integral a) => NormalList msg -> Int -> m a
    indexNList (NormalList msg addr@WordAt{..} len) eltsPerWord
        | i < len = do
            let wordIndex' = wordIndex + WordCount (i `div` eltsPerWord)
            word <- GM.getWord msg addr { wordIndex = wordIndex' }
            let shift = (i `mod` eltsPerWord) * (64 `div` eltsPerWord)
            pure $ fromIntegral $ word `shiftR` shift
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1 }

-- | Returns the length of a list
length :: ListOf msg a -> Int
length (ListOfVoid _ len)     = len
length (ListOfStruct _ len)   = len
length (ListOfBool   nlist)   = nLen nlist
length (ListOfWord8  nlist)   = nLen nlist
length (ListOfWord16 nlist)   = nLen nlist
length (ListOfWord32 nlist)   = nLen nlist
length (ListOfWord64 nlist)   = nLen nlist
length (ListOfPtr    nlist)   = nLen nlist
length (ListOfMapped list _)  = length list
length (ListOfExtract _ list) = length list

-- | The data section of a struct, as a list of Word64
dataSection :: Struct msg -> ListOf msg Word64
dataSection (Struct msg addr dataSz _) =
    ListOfWord64 $ NormalList msg addr (fromIntegral dataSz)

-- | The pointer section of a struct, as a list of Ptr
ptrSection :: Struct msg -> ListOf msg (Maybe (Ptr msg))
ptrSection (Struct msg addr@WordAt{..} dataSz ptrSz) =
    ListOfPtr $ NormalList
        msg
        addr { wordIndex = wordIndex + fromIntegral dataSz }
        (fromIntegral ptrSz)

-- | @'getData' i struct@ gets the @i@th word from the struct's data section,
-- returning 0 if it is absent.
getData :: (GM.Message m msg seg, ReadCtx m) => Int -> Struct msg -> m Word64
getData i struct
    | length (dataSection struct) <= i = 0 <$ invoice 1
    | otherwise = index i (dataSection struct)

-- | @'getPtr' i struct@ gets the @i@th word from the struct's pointer section,
-- returning Nothing if it is absent.
getPtr :: (GM.Message m msg seg, ReadCtx m) => Int -> Struct msg -> m (Maybe (Ptr msg))
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
rawBytes :: (GM.Message m msg seg, ReadCtx m) => ListOf msg a -> m BS.ByteString
rawBytes (ListOfWord8 (NormalList msg WordAt{..} len)) = do
    bytes <- GM.getSegment msg segIndex >>= GM.toByteString
    let ByteCount byteOffset = wordsToBytes wordIndex
    pure $ BS.take len $ BS.drop byteOffset bytes
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
rootPtr :: (GM.Message m msg seg, ReadCtx m) => msg -> m (Struct msg)
rootPtr msg = do
    root <- get msg (WordAt 0 0)
    case root of
        Just (PtrStruct struct) -> pure struct
        _ -> throwM $ E.SchemaViolationError
                "Unexpected root type; expected struct."
