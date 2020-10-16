-- | Capnproto message canonicalization, per:
--
-- https://capnproto.org/encoding.html#canonicalization
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Capnp.Canonicalize
    ( canonicalize
    ) where

-- Note [Allocation strategy]
--
-- The implementation makes use of knowledge of how we allocate values inside
-- a message; in particular, we assume objects are allocated sequentially,
-- and that if the first segment is big enough we will never allocate a second
-- segment.
--
-- If we ever make the allocator plugable, we will have to revisit this and
-- ensure that our assumptions still hold.

-- Note [Other assumptions]
--
-- This code relies on the fact that Capnp.Pointer.serializePointer does the
-- canonicalization of zero-sized struct pointers for us; see the comments there
-- for more details.

import Data.Word

import Data.Foldable    (for_)
import Data.Maybe       (isNothing)
import Data.Traversable (for)

import           Capnp.Bits    (WordCount)
import qualified Capnp.Message as M
import qualified Capnp.Untyped as U

-- | Return a canonicalized message with a copy of the given struct as its
-- root. returns a (message, segment) pair, where the segment is the first
-- and only segment of the returned message.
--
-- In addition to the usual reasons for failure when reading a message (traversal limit,
-- malformed messages), this can fail if the message does not fit in a single segment,
-- as the canonical form requires single-segment messages.
canonicalize :: (U.RWCtx m s, M.Message m msgIn) => U.Struct msgIn -> m (M.MutMsg s, M.Segment (M.MutMsg s))
canonicalize rootStructIn = do
    let msgIn = U.message rootStructIn
    -- Note [Allocation strategy]
    words <- totalWords msgIn
    msgOut <- M.newMessage $ Just words
    rootStructOut <- cloneCanonicalStruct rootStructIn msgOut
    U.setRoot rootStructOut
    segOut <- M.getSegment msgOut 0
    pure (msgOut, segOut)

totalWords :: U.ReadCtx m msg => msg -> m WordCount
totalWords msg = do
    -- Note [Allocation strategy]
    segCount <- M.numSegs msg
    sizes <- for [0..segCount - 1] $ \i -> do
        seg <- M.getSegment msg i
        M.numWords seg
    pure $ sum sizes

cloneCanonicalStruct :: (U.RWCtx m s, M.Message m msgIn) => U.Struct msgIn -> M.MutMsg s -> m (U.Struct (M.MutMsg s))
cloneCanonicalStruct structIn msgOut = do
    (nWords, nPtrs) <- findCanonicalSectionCounts structIn
    structOut <- U.allocStruct msgOut (fromIntegral nWords) (fromIntegral nPtrs)
    copyCanonicalStruct structIn structOut
    pure structOut

copyCanonicalStruct :: (U.RWCtx m s, M.Message m msgIn) => U.Struct msgIn -> U.Struct (M.MutMsg s) -> m ()
copyCanonicalStruct structIn structOut = do
    let nWords = fromIntegral $ U.structWordCount structOut
        nPtrs = fromIntegral $ U.structPtrCount structOut
    for_ [0..nWords - 1] $ \i -> do
        word <- U.getData i structIn
        U.setData word i structOut
    for_ [0..nPtrs - 1] $ \i -> do
        ptrIn <- U.getPtr i structIn
        ptrOut <- cloneCanonicalPtr ptrIn (U.message structOut)
        U.setPtr ptrOut i structOut

findCanonicalSectionCounts :: U.ReadCtx m msg => U.Struct msg -> m (Word16, Word16)
findCanonicalSectionCounts struct = do
    nWords <- canonicalSectionCount (== 0) (`U.getData` struct) (fromIntegral $ U.structWordCount struct)
    nPtrs <- canonicalSectionCount isNothing (`U.getPtr` struct) (fromIntegral $ U.structPtrCount struct)
    pure (nWords, nPtrs)

canonicalSectionCount :: Monad m => (a -> Bool) -> (Int -> m a) -> Int -> m Word16
canonicalSectionCount _ _ 0 = pure 0
canonicalSectionCount isDefault getIndex total = do
    value <- getIndex (total - 1)
    if isDefault value
        then canonicalSectionCount isDefault getIndex (total - 1)
        else pure $ fromIntegral total

cloneCanonicalPtr :: (U.RWCtx m s, M.Message m msgIn) => Maybe (U.Ptr msgIn) -> M.MutMsg s -> m (Maybe (U.Ptr (M.MutMsg s)))
cloneCanonicalPtr ptrIn msgOut =
    case ptrIn of
        Nothing ->
            pure Nothing
        Just (U.PtrCap cap) -> do
            client <- U.getClient cap
            Just . U.PtrCap <$> U.appendCap msgOut client
        Just (U.PtrStruct struct) ->
            Just . U.PtrStruct <$> cloneCanonicalStruct struct msgOut
        Just (U.PtrList list) ->
            Just . U.PtrList <$> cloneCanonicalList list msgOut

cloneCanonicalList :: (U.RWCtx m s, M.Message m msgIn) => U.List msgIn -> M.MutMsg s -> m (U.List (M.MutMsg s))
cloneCanonicalList listIn msgOut =
    case listIn of
        U.List0 l -> U.List0 <$> U.allocList0 msgOut (U.length l)
        U.List1 l -> U.List1 <$> (U.allocList1 msgOut (U.length l) >>= copyCanonicalDataList l)
        U.List8 l -> U.List8 <$> (U.allocList8 msgOut (U.length l) >>= copyCanonicalDataList l)
        U.List16 l -> U.List16 <$> (U.allocList16 msgOut (U.length l) >>= copyCanonicalDataList l)
        U.List32 l -> U.List32 <$> (U.allocList32 msgOut (U.length l) >>= copyCanonicalDataList l)
        U.List64 l -> U.List64 <$> (U.allocList64 msgOut (U.length l) >>= copyCanonicalDataList l)
        U.ListPtr l -> U.ListPtr <$> (U.allocListPtr msgOut (U.length l) >>= copyCanonicalPtrList l)
        U.ListStruct l -> U.ListStruct <$> cloneCanonicalStructList l msgOut

copyCanonicalDataList :: (U.RWCtx m s, M.Message m msgIn) => U.ListOf msgIn a -> U.ListOf (M.MutMsg s) a -> m (U.ListOf (M.MutMsg s) a)
copyCanonicalDataList listIn listOut = do
    for_ [0..U.length listIn - 1] $ \i -> do
        value <- U.index i listIn
        U.setIndex value i listOut
    pure listOut

copyCanonicalPtrList
    :: (U.RWCtx m s, M.Message m msgIn)
    => U.ListOf msgIn (Maybe (U.Ptr msgIn))
    -> U.ListOf (M.MutMsg s) (Maybe (U.Ptr (M.MutMsg s)))
    -> m (U.ListOf (M.MutMsg s) (Maybe (U.Ptr (M.MutMsg s))))
copyCanonicalPtrList listIn listOut = do
    for_ [0..U.length listIn - 1] $ \i -> do
        ptrIn <- U.index i listIn
        ptrOut <- cloneCanonicalPtr ptrIn (U.message listOut)
        U.setIndex ptrOut i listOut
    pure listOut

cloneCanonicalStructList
    :: (U.RWCtx m s, M.Message m msgIn)
    => U.ListOf msgIn (U.Struct msgIn)
    -> M.MutMsg s
    -> m (U.ListOf (M.MutMsg s) (U.Struct (M.MutMsg s)))
cloneCanonicalStructList listIn msgOut = do
    (nWords, nPtrs) <- findCanonicalListSectionCounts listIn
    listOut <- U.allocCompositeList msgOut nWords nPtrs (U.length listIn)
    copyCanonicalStructList listIn listOut
    pure listOut

copyCanonicalStructList
    :: (U.RWCtx m s, M.Message m msgIn)
    => U.ListOf msgIn (U.Struct msgIn)
    -> U.ListOf (M.MutMsg s) (U.Struct (M.MutMsg s))
    -> m ()
copyCanonicalStructList listIn listOut =
    for_ [0..U.length listIn - 1] $ \i -> do
        structIn <- U.index i listIn
        structOut <- U.index i listOut
        copyCanonicalStruct structIn structOut

findCanonicalListSectionCounts :: U.ReadCtx m msg => U.ListOf msg (U.Struct msg) -> m (Word16, Word16)
findCanonicalListSectionCounts list = go 0 0 0 where
    go i !nWords !nPtrs
        | i >= U.length list =
            pure (nWords, nPtrs)
        | otherwise = do
            struct <- U.index i list
            (nWords', nPtrs') <- findCanonicalSectionCounts struct
            go (i+1) (max nWords nWords') (max nPtrs nPtrs')
