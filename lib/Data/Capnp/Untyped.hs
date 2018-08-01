{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-|
Module: Data.Capnp.Untyped
Description: Utilities for reading capnproto messages with no schema.

The types and functions in this module know about things like structs and
lists, but are not schema aware.

Each of the data types exported by this module is parametrized over a Message
type (see 'Data.Capnp.Message'), used as the underlying storage.
-}
module Data.Capnp.Untyped
    ( Ptr(..), List(..), Struct, ListOf
    , dataSection, ptrSection
    , getData, getPtr
    , setData, setPtr
    , copyStruct
    , get, index, length
    , setIndex
    , take
    , rootPtr
    , setRoot
    , rawBytes
    , ReadCtx
    , RWCtx
    , HasMessage(..), MessageDefault(..)
    , allocStruct
    , allocCompositeList
    , allocList0
    , allocList1
    , allocList8
    , allocList16
    , allocList32
    , allocList64
    , allocListPtr
    )
  where

import Prelude hiding (length, take)

import Data.Bits
import Data.Word

import Control.Monad             (forM_)
import Control.Monad.Catch       (MonadThrow(throwM))
import Data.Capnp.Address        (OffsetError(..), WordAddr(..), pointerFrom)
import Data.Capnp.Bits
    (ByteCount(..), Word1(..), WordCount(..), replaceBits, wordsToBytes)
import Data.Capnp.Pointer        (ElementSize(..))
import Data.Capnp.TraversalLimit (MonadLimit(invoice))

import qualified Data.ByteString    as BS
import qualified Data.Capnp.Errors  as E
import qualified Data.Capnp.Message as M
import qualified Data.Capnp.Pointer as P

-- | Type (constraint) synonym for the constraints needed for most read
-- operations.
type ReadCtx m msg = (M.Message m msg, MonadThrow m, MonadLimit m)

-- | Synonym for ReadCtx + WriteCtx
type RWCtx m s = (ReadCtx m (M.MutMsg s), M.WriteCtx m s)

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

instance M.Mutable m mmsg cmsg => M.Mutable m (Ptr mmsg) (Ptr cmsg) where
    thaw (PtrCap cmsg n) = do
        mmsg <- M.thaw cmsg
        pure $ PtrCap mmsg n
    thaw (PtrList l) = PtrList <$> M.thaw l
    thaw (PtrStruct s) = PtrStruct <$> M.thaw s
    freeze (PtrCap mmsg n) = do
        cmsg <- M.freeze mmsg
        pure $ PtrCap cmsg n
    freeze (PtrList l) = PtrList <$> M.freeze l
    freeze (PtrStruct s) = PtrStruct <$> M.freeze s
instance M.Mutable m mmsg cmsg => M.Mutable m (List mmsg) (List cmsg) where
    thaw (List0 l)      = List0 <$> M.thaw l
    thaw (List1 l)      = List1 <$> M.thaw l
    thaw (List8 l)      = List8 <$> M.thaw l
    thaw (List16 l)     = List16 <$> M.thaw l
    thaw (List32 l)     = List32 <$> M.thaw l
    thaw (List64 l)     = List64 <$> M.thaw l
    thaw (ListPtr l)    = ListPtr <$> M.thaw l
    thaw (ListStruct l) = ListStruct <$> M.thaw l
    freeze (List0 l)      = List0 <$> M.freeze l
    freeze (List1 l)      = List1 <$> M.freeze l
    freeze (List8 l)      = List8 <$> M.freeze l
    freeze (List16 l)     = List16 <$> M.freeze l
    freeze (List32 l)     = List32 <$> M.freeze l
    freeze (List64 l)     = List64 <$> M.freeze l
    freeze (ListPtr l)    = ListPtr <$> M.freeze l
    freeze (ListStruct l) = ListStruct <$> M.freeze l
instance M.Mutable m mmsg cmsg => M.Mutable m (NormalList mmsg) (NormalList cmsg) where
    thaw NormalList{..} = do
        mmsg <- M.thaw nMsg
        pure NormalList { nMsg = mmsg, .. }
    freeze NormalList{..} = do
        cmsg <- M.freeze nMsg
        pure NormalList { nMsg = cmsg, .. }
instance M.Mutable m mmsg cmsg => M.Mutable m (ListOf mmsg ()) (ListOf cmsg ()) where
    thaw (ListOfVoid cmsg n) = do
        mmsg <- M.thaw cmsg
        pure $ ListOfVoid mmsg n
    freeze (ListOfVoid mmsg n) = do
        cmsg <- M.freeze mmsg
        pure $ ListOfVoid cmsg n
-- Annoyingly, we can't just have an instance @Mutable m (ListOf mmsg a) (ListOf cmsg a)@,
-- because that would require that the implementation to be valid for e.g.
-- @Mutable m (ListOf mmsg (Struct mmsg)) (ListOf cmsg (Struct mmsg))@ (note that the type
-- parameter for 'Struct' does not change). So, instead, we define a separate instance for
-- each possible parameter type:
instance M.Mutable m mmsg cmsg => M.Mutable m (ListOf mmsg Bool) (ListOf cmsg Bool) where
    thaw (ListOfBool msg) = ListOfBool <$> M.thaw msg
    freeze (ListOfBool msg) = ListOfBool <$> M.freeze msg
instance M.Mutable m mmsg cmsg => M.Mutable m (ListOf mmsg Word8) (ListOf cmsg Word8) where
    thaw (ListOfWord8 msg) = ListOfWord8 <$> M.thaw msg
    freeze (ListOfWord8 msg) = ListOfWord8 <$> M.freeze msg
instance M.Mutable m mmsg cmsg => M.Mutable m (ListOf mmsg Word16) (ListOf cmsg Word16) where
    thaw (ListOfWord16 msg) = ListOfWord16 <$> M.thaw msg
    freeze (ListOfWord16 msg) = ListOfWord16 <$> M.freeze msg
instance M.Mutable m mmsg cmsg => M.Mutable m (ListOf mmsg Word32) (ListOf cmsg Word32) where
    thaw (ListOfWord32 msg) = ListOfWord32 <$> M.thaw msg
    freeze (ListOfWord32 msg) = ListOfWord32 <$> M.freeze msg
instance M.Mutable m mmsg cmsg => M.Mutable m (ListOf mmsg Word64) (ListOf cmsg Word64) where
    thaw (ListOfWord64 msg) = ListOfWord64 <$> M.thaw msg
    freeze (ListOfWord64 msg) = ListOfWord64 <$> M.freeze msg
instance M.Mutable m mmsg cmsg => M.Mutable m (ListOf mmsg (Struct mmsg)) (ListOf cmsg (Struct cmsg)) where
    thaw (ListOfStruct ctag size) = do
        mtag <- M.thaw ctag
        pure $ ListOfStruct mtag size
    freeze (ListOfStruct mtag size) = do
        ctag <- M.freeze mtag
        pure $ ListOfStruct ctag size
instance M.Mutable m mmsg cmsg => M.Mutable m (ListOf mmsg (Maybe (Ptr mmsg))) (ListOf cmsg (Maybe (Ptr cmsg))) where
    thaw (ListOfPtr msg) = ListOfPtr <$> M.thaw msg
    freeze (ListOfPtr msg) = ListOfPtr <$> M.freeze msg
instance M.Mutable m mmsg cmsg => M.Mutable m (Struct mmsg) (Struct cmsg) where
    thaw (Struct cmsg addr dataSz ptrSz) = do
        mmsg <- M.thaw cmsg
        pure $ Struct mmsg addr dataSz ptrSz
    freeze (Struct mmsg addr dataSz ptrSz) = do
        cmsg <- M.freeze mmsg
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
get :: ReadCtx m msg => msg -> WordAddr -> m (Maybe (Ptr msg))
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
    getWord msg addr = invoice 1 *> M.getWord msg addr
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

-- | Return the EltSpec needed for a pointer to the given list.
listEltSpec :: List msg -> P.EltSpec
listEltSpec (ListStruct list@(ListOfStruct (Struct msg _ dataSz ptrSz) _)) =
    P.EltComposite $ fromIntegral (length list) * (fromIntegral dataSz + fromIntegral ptrSz)
listEltSpec (List0 list)   = P.EltNormal Sz0 $ fromIntegral (length list)
listEltSpec (List1 list)   = P.EltNormal Sz1 $ fromIntegral (length list)
listEltSpec (List8 list)   = P.EltNormal Sz8 $ fromIntegral (length list)
listEltSpec (List16 list)  = P.EltNormal Sz16 $ fromIntegral (length list)
listEltSpec (List32 list)  = P.EltNormal Sz32 $ fromIntegral (length list)
listEltSpec (List64 list)  = P.EltNormal Sz64 $ fromIntegral (length list)
listEltSpec (ListPtr list) = P.EltNormal SzPtr $ fromIntegral (length list)

-- | Return the starting address of the list.
listAddr :: List msg -> WordAddr
listAddr (ListStruct (ListOfStruct (Struct _ addr _ _) _)) =
    -- addr is the address of the first element of the list, but
    -- composite lists start with a tag word:
    addr { wordIndex = wordIndex addr - 1 }
listAddr (List0 _) = WordAt { segIndex = 0, wordIndex = 1 }
listAddr (List1 (ListOfBool NormalList{nAddr})) = nAddr
listAddr (List8 (ListOfWord8 NormalList{nAddr})) = nAddr
listAddr (List16 (ListOfWord16 NormalList{nAddr})) = nAddr
listAddr (List32 (ListOfWord32 NormalList{nAddr})) = nAddr
listAddr (List64 (ListOfWord64 NormalList{nAddr})) = nAddr
listAddr (ListPtr (ListOfPtr NormalList{nAddr})) = nAddr

-- | Return the address of the pointer's target. It is illegal to call this on
-- a pointer which targets a capability.
ptrAddr :: Ptr msg -> WordAddr
ptrAddr (PtrCap _ _) = error "ptrAddr called on a capability pointer."
ptrAddr (PtrStruct (Struct _ addr _ _)) = addr
ptrAddr (PtrList list) = listAddr list

setIndex :: (ReadCtx m (M.MutMsg s), M.WriteCtx m s) => a -> Int -> ListOf (M.MutMsg s) a -> m ()
setIndex value i list | length list <= i =
    throwM E.BoundsError { E.index = i, E.maxIndex = length list }
setIndex value i list = case list of
    ListOfVoid _ _     -> pure ()
    ListOfBool nlist   -> setNIndex nlist 64 (Word1 value)
    ListOfWord8 nlist  -> setNIndex nlist 8 value
    ListOfWord16 nlist -> setNIndex nlist 4 value
    ListOfWord32 nlist -> setNIndex nlist 2 value
    ListOfWord64 nlist -> setNIndex nlist 1 value
    ListOfPtr nlist -> case value of
        Nothing                -> setNIndex nlist 1 (P.serializePtr Nothing)
        Just (PtrCap _ cap)    -> setNIndex nlist 1 (P.serializePtr (Just (P.CapPtr cap)))
        Just p@(PtrList ptrList)     ->
            setPtrIndex nlist p $ P.ListPtr 0 (listEltSpec ptrList)
        Just p@(PtrStruct (Struct _ addr dataSz ptrSz)) ->
            setPtrIndex nlist p $ P.StructPtr 0 dataSz ptrSz
    list@(ListOfStruct _ _) -> do
        dest <- index i list
        copyStruct dest value
  where
    setNIndex :: (ReadCtx m (M.MutMsg s), M.WriteCtx m s, Bounded a, Integral a) => NormalList (M.MutMsg s) -> Int -> a -> m ()
    setNIndex NormalList{nAddr=nAddr@WordAt{..},..} eltsPerWord value = do
        let wordAddr = nAddr { wordIndex = wordIndex + WordCount (i `div` eltsPerWord) }
        word <- M.getWord nMsg wordAddr
        let shift = (i `mod` eltsPerWord) * (64 `div` eltsPerWord)
        M.setWord nMsg wordAddr $ replaceBits value word shift
    setPtrIndex :: (ReadCtx m (M.MutMsg s), M.WriteCtx m s) => NormalList (M.MutMsg s) -> Ptr (M.MutMsg s) -> P.Ptr -> m ()
    setPtrIndex nlist@NormalList{..} absPtr relPtr =
        let srcAddr = nAddr { wordIndex = wordIndex nAddr + WordCount i }
        in case pointerFrom srcAddr (ptrAddr absPtr) relPtr of
            Left DifferentSegments -> error "TODO: handle setIndex when we need a far pointer."
            Left OutOfRange -> error "BUG: we should be screening messages to make this impossible."
            Right ptr ->
                setNIndex nlist 1 $ P.serializePtr (Just ptr)


-- | @'copyStruct' dest src@ copies the source struct to the destination struct.
copyStruct :: (ReadCtx m (M.MutMsg s), M.WriteCtx m s)
    => Struct (M.MutMsg s) -> Struct (M.MutMsg s) -> m ()
copyStruct dest src = do
    -- We copy both the data and pointer sections from src to dest,
    -- padding the tail of the destination section with zeros/null
    -- pointers as necessary. If the destination section is
    -- smaller than the source section, this will raise a BoundsError.
    --
    -- TODO: possible enhancement: allow the destination section to be
    -- smaller than the source section if and only if the tail of the
    -- source section is all zeros (default values).
    copySection (dataSection dest) (dataSection src) 0
    copySection (ptrSection  dest) (ptrSection  src) Nothing
  where
    copySection dest src pad = do
        -- Copy the source section to the destination section:
        forM_ [0..length src - 1] $ \i -> do
            value <- index i src
            setIndex value i dest
        -- Pad the remainder with zeros/default values:
        forM_ [length src..length dest - 1] $ \i ->
            setIndex pad i dest


-- | @index i list@ returns the ith element in @list@. Deducts 1 from the quota
index :: ReadCtx m msg => Int -> ListOf msg a -> m a
index i list = invoice 1 >> index' list
  where
    index' :: ReadCtx m msg => ListOf msg a -> m a
    index' (ListOfVoid _ len)
        | i < len = pure ()
        | otherwise = throwM E.BoundsError { E.index = i, E.maxIndex = len - 1 }
    index' (ListOfStruct (Struct msg addr@WordAt{..} dataSz ptrSz) len)
        | i < len = do
            let offset = WordCount $ i * (fromIntegral dataSz + fromIntegral ptrSz)
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
    indexNList :: (ReadCtx m msg, Integral a) => NormalList msg -> Int -> m a
    indexNList (NormalList msg addr@WordAt{..} len) eltsPerWord
        | i < len = do
            let wordIndex' = wordIndex + WordCount (i `div` eltsPerWord)
            word <- M.getWord msg addr { wordIndex = wordIndex' }
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

-- | Return a prefix of the list, of the given length.
take :: MonadThrow m => Int -> ListOf msg a -> m (ListOf msg a)
take count list
    | length list < count =
        throwM E.BoundsError { E.index = count, E.maxIndex = length list - 1 }
    | otherwise = pure $ go list
  where
    go (ListOfVoid msg _)   = ListOfVoid msg count
    go (ListOfStruct tag _) = ListOfStruct tag count
    go (ListOfBool nlist)   = ListOfBool $ nTake nlist
    go (ListOfWord8 nlist)  = ListOfWord8 $ nTake nlist
    go (ListOfWord16 nlist) = ListOfWord16 $ nTake nlist
    go (ListOfWord32 nlist) = ListOfWord32 $ nTake nlist
    go (ListOfWord64 nlist) = ListOfWord64 $ nTake nlist
    go (ListOfPtr nlist)    = ListOfPtr $ nTake nlist

    nTake :: NormalList msg -> NormalList msg
    nTake NormalList{..} = NormalList { nLen = count, .. }

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
getData :: ReadCtx m msg => Int -> Struct msg -> m Word64
getData i struct
    | length (dataSection struct) <= i = 0 <$ invoice 1
    | otherwise = index i (dataSection struct)

-- | @'getPtr' i struct@ gets the @i@th word from the struct's pointer section,
-- returning Nothing if it is absent.
getPtr :: ReadCtx m msg => Int -> Struct msg -> m (Maybe (Ptr msg))
getPtr i struct
    | length (ptrSection struct) <= i = Nothing <$ invoice 1
    | otherwise = index i (ptrSection struct)

-- | @'setData' value i struct@ sets the @i@th word in the struct's data section
-- to @value@.
setData :: (ReadCtx m (M.MutMsg s), M.WriteCtx m s)
    => Word64 -> Int -> Struct (M.MutMsg s) -> m ()
setData value i = setIndex value i . dataSection

-- | @'setData' value i struct@ sets the @i@th pointer in the struct's pointer
-- section to @value@.
setPtr :: (ReadCtx m (M.MutMsg s), M.WriteCtx m s) => Maybe (Ptr (M.MutMsg s)) -> Int -> Struct (M.MutMsg s) -> m ()
setPtr value i = setIndex value i . ptrSection

-- | 'rawBytes' returns the raw bytes corresponding to the list.
rawBytes :: ReadCtx m msg => ListOf msg Word8 -> m BS.ByteString
rawBytes (ListOfWord8 (NormalList msg WordAt{..} len)) = do
    invoice len
    bytes <- M.getSegment msg segIndex >>= M.toByteString
    let ByteCount byteOffset = wordsToBytes wordIndex
    pure $ BS.take len $ BS.drop byteOffset bytes


-- | Returns the root pointer of a message.
rootPtr :: ReadCtx m msg => msg -> m (Struct msg)
rootPtr msg = do
    root <- get msg (WordAt 0 0)
    case root of
        Just (PtrStruct struct) -> pure struct
        _ -> throwM $ E.SchemaViolationError
                "Unexpected root type; expected struct."


-- | Make the given struct the root object of its message.
setRoot :: M.WriteCtx m s => Struct (M.MutMsg s) -> m ()
setRoot (Struct msg addr dataSz ptrSz) =
    case pointerFrom (WordAt 0 0) addr (P.StructPtr 0 dataSz ptrSz) of
        Right ptr ->
            M.setWord msg (WordAt 0 0) (P.serializePtr $ Just ptr)
        Left DifferentSegments ->
            error $ "TODO: handle setting the root struct to something "
                ++ "outside the first segment."
        Left OutOfRange ->
            error "BUG(TODO): segment is too large to set the root pointer."

-- | Allocate a struct in the message.
allocStruct :: M.WriteCtx m s => M.MutMsg s -> Word16 -> Word16 -> m (Struct (M.MutMsg s))
allocStruct msg dataSz ptrSz = do
    let totalSz = fromIntegral dataSz + fromIntegral ptrSz
    addr <- M.alloc msg totalSz
    pure $ Struct msg addr dataSz ptrSz

-- | Allocate a composite list.
allocCompositeList
    :: M.WriteCtx m s
    => M.MutMsg s -- ^ The message to allocate in.
    -> Word16     -- ^ The size of the data sections
    -> Word16     -- ^ The size of the pointer sections
    -> Int        -- ^ The length of the list in elements.
    -> m (ListOf (M.MutMsg s) (Struct (M.MutMsg s)))
allocCompositeList msg dataSz ptrSz len = do
    let eltSize = fromIntegral dataSz + fromIntegral ptrSz
    addr <- M.alloc msg (WordCount $ len * eltSize + 1) -- + 1 for the tag word.
    M.setWord msg addr $ P.serializePtr $ Just $ P.StructPtr (fromIntegral len) dataSz ptrSz
    let firstStruct = Struct
            msg
            addr { wordIndex = wordIndex addr + 1 }
            dataSz
            ptrSz
    pure $ ListOfStruct firstStruct len

allocList0   :: M.WriteCtx m s => M.MutMsg s -> Int -> m (ListOf (M.MutMsg s) ())
allocList1   :: M.WriteCtx m s => M.MutMsg s -> Int -> m (ListOf (M.MutMsg s) Bool)
allocList8   :: M.WriteCtx m s => M.MutMsg s -> Int -> m (ListOf (M.MutMsg s) Word8)
allocList16  :: M.WriteCtx m s => M.MutMsg s -> Int -> m (ListOf (M.MutMsg s) Word16)
allocList32  :: M.WriteCtx m s => M.MutMsg s -> Int -> m (ListOf (M.MutMsg s) Word32)
allocList64  :: M.WriteCtx m s => M.MutMsg s -> Int -> m (ListOf (M.MutMsg s) Word64)
allocListPtr :: M.WriteCtx m s => M.MutMsg s -> Int -> m (ListOf (M.MutMsg s) (Maybe (Ptr (M.MutMsg s))))

allocList0   msg len = pure $ ListOfVoid msg len
allocList1   msg len = ListOfBool   <$> allocNormalList 64 msg len
allocList8   msg len = ListOfWord8  <$> allocNormalList 8 msg len
allocList16  msg len = ListOfWord16 <$> allocNormalList 4 msg len
allocList32  msg len = ListOfWord32 <$> allocNormalList 2 msg len
allocList64  msg len = ListOfWord64 <$> allocNormalList 1 msg len
allocListPtr msg len = ListOfPtr    <$> allocNormalList 1 msg len

-- | Allocate a NormalList
allocNormalList
    :: M.WriteCtx m s
    => Int        -- ^ The number of elements per 64-bit word
    -> M.MutMsg s -- ^ The message to allocate in
    -> Int        -- ^ The number of elements in the list.
    -> m (NormalList (M.MutMsg s))
allocNormalList eltsPerWord msg len = do
    -- round 'len' up to the nearest word boundary.
    let mask = eltsPerWord - 1
        wordCount = WordCount $ (len + mask) .&. complement mask
    addr <- M.alloc msg wordCount
    pure NormalList
        { nMsg = msg
        , nAddr = addr
        , nLen = len
        }
