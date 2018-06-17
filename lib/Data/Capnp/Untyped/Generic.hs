{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-|
Module: Data.Capnp.Untyped.Generic
Description: Utilities for reading capnproto messages with no schema.

The types and functions in this module know about things like structs and
lists, but are not schema aware.

Each of the data types exported by this module is parametrized over a Message
type (see 'Data.Capnp.Message.Generic'), used as the underlying storage.
-}
module Data.Capnp.Untyped.Generic
    ( Ptr(..), List(..), Struct, ListOf
    , dataSection, ptrSection
    , getData, getPtr
    , get, index, length
    , rootPtr
    , rawBytes
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

-- | A struct value in a message.
data Struct msg
    = Struct
        msg
        !WordAddr -- Start of struct
        !Word16 -- Data section size.
        !Word16 -- Pointer section size.

instance GM.Mutable m mmsg cmsg => GM.Mutable m (Ptr mmsg) (Ptr cmsg) where
    thaw (PtrCap cmsg n) = do
        mmsg <- GM.thaw cmsg
        pure $ PtrCap mmsg n
    thaw (PtrList l) = PtrList <$> GM.thaw l
    thaw (PtrStruct s) = PtrStruct <$> GM.thaw s
    freeze (PtrCap mmsg n) = do
        cmsg <- GM.freeze mmsg
        pure $ PtrCap cmsg n
    freeze (PtrList l) = PtrList <$> GM.freeze l
    freeze (PtrStruct s) = PtrStruct <$> GM.freeze s
instance GM.Mutable m mmsg cmsg => GM.Mutable m (List mmsg) (List cmsg) where
    thaw (List0 l)      = List0 <$> GM.thaw l
    thaw (List1 l)      = List1 <$> GM.thaw l
    thaw (List8 l)      = List8 <$> GM.thaw l
    thaw (List16 l)     = List16 <$> GM.thaw l
    thaw (List32 l)     = List32 <$> GM.thaw l
    thaw (List64 l)     = List64 <$> GM.thaw l
    thaw (ListPtr l)    = ListPtr <$> GM.thaw l
    thaw (ListStruct l) = ListStruct <$> GM.thaw l
    freeze (List0 l)      = List0 <$> GM.freeze l
    freeze (List1 l)      = List1 <$> GM.freeze l
    freeze (List8 l)      = List8 <$> GM.freeze l
    freeze (List16 l)     = List16 <$> GM.freeze l
    freeze (List32 l)     = List32 <$> GM.freeze l
    freeze (List64 l)     = List64 <$> GM.freeze l
    freeze (ListPtr l)    = ListPtr <$> GM.freeze l
    freeze (ListStruct l) = ListStruct <$> GM.freeze l
instance GM.Mutable m mmsg cmsg => GM.Mutable m (NormalList mmsg) (NormalList cmsg) where
    thaw NormalList{..} = do
        mmsg <- GM.thaw nMsg
        pure NormalList { nMsg = mmsg, .. }
    freeze NormalList{..} = do
        cmsg <- GM.freeze nMsg
        pure NormalList { nMsg = cmsg, .. }
instance GM.Mutable m mmsg cmsg => GM.Mutable m (ListOf mmsg ()) (ListOf cmsg ()) where
    thaw (ListOfVoid cmsg n) = do
        mmsg <- GM.thaw cmsg
        pure $ ListOfVoid mmsg n
    freeze (ListOfVoid mmsg n) = do
        cmsg <- GM.freeze mmsg
        pure $ ListOfVoid cmsg n
-- Annoyingly, we can't just have an instance @Mutable m (ListOf mmsg a) (ListOf cmsg a)@,
-- because that would require that the implementation to be valid for e.g.
-- @Mutable m (ListOf mmsg (Struct mmsg)) (ListOf cmsg (Struct mmsg))@ (note that the type
-- parameter for 'Struct' does not change). So, instead, we define a separate instance for
-- each possible parameter type:
instance GM.Mutable m mmsg cmsg => GM.Mutable m (ListOf mmsg Bool) (ListOf cmsg Bool) where
    thaw (ListOfBool msg) = ListOfBool <$> GM.thaw msg
    freeze (ListOfBool msg) = ListOfBool <$> GM.freeze msg
instance GM.Mutable m mmsg cmsg => GM.Mutable m (ListOf mmsg Word8) (ListOf cmsg Word8) where
    thaw (ListOfWord8 msg) = ListOfWord8 <$> GM.thaw msg
    freeze (ListOfWord8 msg) = ListOfWord8 <$> GM.freeze msg
instance GM.Mutable m mmsg cmsg => GM.Mutable m (ListOf mmsg Word16) (ListOf cmsg Word16) where
    thaw (ListOfWord16 msg) = ListOfWord16 <$> GM.thaw msg
    freeze (ListOfWord16 msg) = ListOfWord16 <$> GM.freeze msg
instance GM.Mutable m mmsg cmsg => GM.Mutable m (ListOf mmsg Word32) (ListOf cmsg Word32) where
    thaw (ListOfWord32 msg) = ListOfWord32 <$> GM.thaw msg
    freeze (ListOfWord32 msg) = ListOfWord32 <$> GM.freeze msg
instance GM.Mutable m mmsg cmsg => GM.Mutable m (ListOf mmsg Word64) (ListOf cmsg Word64) where
    thaw (ListOfWord64 msg) = ListOfWord64 <$> GM.thaw msg
    freeze (ListOfWord64 msg) = ListOfWord64 <$> GM.freeze msg
instance GM.Mutable m mmsg cmsg => GM.Mutable m (ListOf mmsg (Struct mmsg)) (ListOf cmsg (Struct cmsg)) where
    thaw (ListOfStruct ctag size) = do
        mtag <- GM.thaw ctag
        pure $ ListOfStruct mtag size
    freeze (ListOfStruct mtag size) = do
        ctag <- GM.freeze mtag
        pure $ ListOfStruct ctag size
instance GM.Mutable m mmsg cmsg => GM.Mutable m (ListOf mmsg (Maybe (Ptr mmsg))) (ListOf cmsg (Maybe (Ptr cmsg))) where
    thaw (ListOfPtr msg) = ListOfPtr <$> GM.thaw msg
    freeze (ListOfPtr msg) = ListOfPtr <$> GM.freeze msg
instance GM.Mutable m mmsg cmsg => GM.Mutable m (Struct mmsg) (Struct cmsg) where
    thaw (Struct cmsg addr dataSz ptrSz) = do
        mmsg <- GM.thaw cmsg
        pure $ Struct mmsg addr dataSz ptrSz
    freeze (Struct mmsg addr dataSz ptrSz) = do
        cmsg <- GM.freeze mmsg
        pure $ Struct cmsg addr dataSz ptrSz

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
    message (ListOfVoid msg _)   = msg
    message (ListOfStruct tag _) = message tag
    message (ListOfBool list)    = message list
    message (ListOfWord8 list)   = message list
    message (ListOfWord16 list)  = message list
    message (ListOfWord32 list)  = message list
    message (ListOfWord64 list)  = message list
    message (ListOfPtr list)     = message list

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
get :: (GM.Message m msg, ReadCtx m) => msg -> WordAddr -> m (Maybe (Ptr msg))
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
index :: (GM.Message m msg, ReadCtx m) => Int -> ListOf msg a -> m a
index i list = invoice 1 >> index' list
  where
    index' :: (GM.Message m msg, ReadCtx m) => ListOf msg a -> m a
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
    indexNList :: (GM.Message m msg, ReadCtx m, Integral a) => NormalList msg -> Int -> m a
    indexNList (NormalList msg addr@WordAt{..} len) eltsPerWord
        | i < len = do
            let wordIndex' = wordIndex + WordCount (i `div` eltsPerWord)
            word <- GM.getWord msg addr { wordIndex = wordIndex' }
            let shift = (i `mod` eltsPerWord) * (64 `div` eltsPerWord)
            pure $ fromIntegral $ word `shiftR` shift
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1 }

-- | Returns the length of a list
length :: ListOf msg a -> Int
length (ListOfVoid _ len)   = len
length (ListOfStruct _ len) = len
length (ListOfBool   nlist) = nLen nlist
length (ListOfWord8  nlist) = nLen nlist
length (ListOfWord16 nlist) = nLen nlist
length (ListOfWord32 nlist) = nLen nlist
length (ListOfWord64 nlist) = nLen nlist
length (ListOfPtr    nlist) = nLen nlist

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
getData :: (GM.Message m msg, ReadCtx m) => Int -> Struct msg -> m Word64
getData i struct
    | length (dataSection struct) <= i = 0 <$ invoice 1
    | otherwise = index i (dataSection struct)

-- | @'getPtr' i struct@ gets the @i@th word from the struct's pointer section,
-- returning Nothing if it is absent.
getPtr :: (GM.Message m msg, ReadCtx m) => Int -> Struct msg -> m (Maybe (Ptr msg))
getPtr i struct
    | length (ptrSection struct) <= i = Nothing <$ invoice 1
    | otherwise = index i (ptrSection struct)


-- | 'rawBytes' returns the raw bytes corresponding to the list.
rawBytes :: (GM.Message m msg, ReadCtx m) => ListOf msg Word8 -> m BS.ByteString
rawBytes (ListOfWord8 (NormalList msg WordAt{..} len)) = do
    invoice len
    bytes <- GM.getSegment msg segIndex >>= GM.toByteString
    let ByteCount byteOffset = wordsToBytes wordIndex
    pure $ BS.take len $ BS.drop byteOffset bytes


-- | Returns the root pointer of a message.
rootPtr :: (GM.Message m msg, ReadCtx m) => msg -> m (Struct msg)
rootPtr msg = do
    root <- get msg (WordAt 0 0)
    case root of
        Just (PtrStruct struct) -> pure struct
        _ -> throwM $ E.SchemaViolationError
                "Unexpected root type; expected struct."
