-- | Capnproto message canonicalization, per:
--
-- https://capnproto.org/encoding.html#canonicalization
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
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
-- import qualified Language.Haskell.TH as TH

import           Capnp.Bits           (WordCount)
import           Capnp.Message        (Mutability(..))
import qualified Capnp.Message        as M
import           Capnp.TraversalLimit (LimitT)
import qualified Capnp.Untyped        as U
import           Control.Monad.ST     (RealWorld)
-- import           Internal.BuildPure   (PureBuilder)

-- | Return a canonicalized message with a copy of the given struct as its
-- root. returns a (message, segment) pair, where the segment is the first
-- and only segment of the returned message.
--
-- In addition to the usual reasons for failure when reading a message (traversal limit,
-- malformed messages), this can fail if the message does not fit in a single segment,
-- as the canonical form requires single-segment messages.
canonicalize
   :: (U.RWCtx m s, M.MonadReadMessage mutIn m)
   => U.Struct mutIn -> m (M.Message ('Mut s), M.Segment ('Mut s))
{-# SPECIALIZE canonicalize :: U.Struct 'Const -> LimitT IO (M.Message ('Mut RealWorld), M.Segment ('Mut RealWorld)) #-}
{-# SPECIALIZE canonicalize :: U.Struct ('Mut RealWorld) -> LimitT IO (M.Message ('Mut RealWorld), M.Segment ('Mut RealWorld)) #-}
canonicalize rootStructIn = do
    let msgIn = U.message @U.Struct rootStructIn
    -- Note [Allocation strategy]
    words <- totalWords msgIn
    msgOut <- M.newMessage $ Just words
    rootStructOut <- cloneCanonicalStruct rootStructIn msgOut
    U.setRoot rootStructOut
    segOut <- M.getSegment msgOut 0
    pure (msgOut, segOut)

totalWords :: U.ReadCtx m mut => M.Message mut -> m WordCount
totalWords msg = do
    -- Note [Allocation strategy]
    segCount <- M.numSegs msg
    sizes <- for [0..segCount - 1] $ \i -> do
        seg <- M.getSegment msg i
        M.numWords seg
    pure $ sum sizes

cloneCanonicalStruct
    :: (U.RWCtx m s, M.MonadReadMessage mutIn m)
    => U.Struct mutIn -> M.Message ('Mut s) -> m (U.Struct ('Mut s))
{-# SPECIALIZE cloneCanonicalStruct :: U.Struct 'Const -> M.Message ('Mut RealWorld) -> LimitT IO (U.Struct ('Mut RealWorld)) #-}
{-# SPECIALIZE cloneCanonicalStruct :: U.Struct ('Mut RealWorld) -> M.Message ('Mut RealWorld) -> LimitT IO (U.Struct ('Mut RealWorld)) #-}
cloneCanonicalStruct structIn msgOut = do
    (nWords, nPtrs) <- findCanonicalSectionCounts structIn
    structOut <- U.allocStruct msgOut (fromIntegral nWords) (fromIntegral nPtrs)
    copyCanonicalStruct structIn structOut
    pure structOut

copyCanonicalStruct
    :: (U.RWCtx m s, M.MonadReadMessage mutIn m)
    => U.Struct mutIn -> U.Struct ('Mut s) -> m ()
{-# SPECIALIZE copyCanonicalStruct :: U.Struct 'Const -> U.Struct ('Mut RealWorld) -> LimitT IO () #-}
{-# SPECIALIZE copyCanonicalStruct :: U.Struct ('Mut RealWorld) -> U.Struct ('Mut RealWorld) -> LimitT IO () #-}
copyCanonicalStruct structIn structOut = do
    let nWords = fromIntegral $ U.structWordCount structOut
        nPtrs = fromIntegral $ U.structPtrCount structOut
    for_ [0..nWords - 1] $ \i -> do
        word <- U.getData i structIn
        U.setData word i structOut
    for_ [0..nPtrs - 1] $ \i -> do
        ptrIn <- U.getPtr i structIn
        ptrOut <- cloneCanonicalPtr ptrIn (U.message @U.Struct structOut)
        U.setPtr ptrOut i structOut

findCanonicalSectionCounts :: U.ReadCtx m mut => U.Struct mut -> m (Word16, Word16)
{-# SPECIALIZE findCanonicalSectionCounts :: U.Struct 'Const -> LimitT IO (Word16, Word16) #-}
{-# SPECIALIZE findCanonicalSectionCounts :: U.Struct ('Mut RealWorld) -> LimitT IO (Word16, Word16) #-}
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

cloneCanonicalPtr
    :: (U.RWCtx m s, M.MonadReadMessage mutIn m)
    => Maybe (U.Ptr mutIn) -> M.Message ('Mut s) -> m (Maybe (U.Ptr ('Mut s)))
{-# SPECIALIZE cloneCanonicalPtr :: Maybe (U.Ptr 'Const) -> M.Message ('Mut RealWorld) -> LimitT IO (Maybe (U.Ptr ('Mut RealWorld))) #-}
{-# SPECIALIZE cloneCanonicalPtr :: Maybe (U.Ptr ('Mut RealWorld)) -> M.Message ('Mut RealWorld) -> LimitT IO (Maybe (U.Ptr ('Mut RealWorld))) #-}
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

cloneCanonicalList
    :: (U.RWCtx m s, M.MonadReadMessage mutIn m)
    => U.List mutIn -> M.Message ('Mut s) -> m (U.List ('Mut s))
{-# SPECIALIZE cloneCanonicalList :: U.List 'Const -> M.Message ('Mut RealWorld) -> LimitT IO (U.List ('Mut RealWorld)) #-}
{-# SPECIALIZE cloneCanonicalList :: U.List ('Mut RealWorld) -> M.Message ('Mut RealWorld) -> LimitT IO (U.List ('Mut RealWorld)) #-}
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

copyCanonicalDataList ::
    ( U.RWCtx m s
    , M.MonadReadMessage mutIn m
    , U.ListItem r
    , U.Unwrapped (U.Untyped r mutIn) ~ U.Unwrapped (U.Untyped r ('Mut s))
    )
    => U.ListOf r mutIn -> U.ListOf r ('Mut s) -> m (U.ListOf r ('Mut s))
{-
{-# SPECIALIZE copyCanonicalDataList ::
    ( U.ListItem r
    , U.Unwrapped (U.Untyped r 'Const) ~ U.Unwrapped (U.Untyped r ('Mut RealWorld))
    )
    => U.ListOf r 'Const
    -> U.ListOf r ('Mut RealWorld)
    -> LimitT IO (U.ListOf r ('Mut RealWorld))
    #-}
-}
{-# SPECIALIZE copyCanonicalDataList ::
    ( U.ListItem r
    )
    => U.ListOf r ('Mut RealWorld)
    -> U.ListOf r ('Mut RealWorld)
    -> LimitT IO (U.ListOf r ('Mut RealWorld))
    #-}
copyCanonicalDataList listIn listOut = do
    for_ [0..U.length listIn - 1] $ \i -> do
        value <- U.index i listIn
        U.setIndex value i listOut
    pure listOut

copyCanonicalPtrList
    :: (U.RWCtx m s, M.MonadReadMessage mutIn m)
    => U.ListOf ('U.Ptr 'Nothing) mutIn
    -> U.ListOf ('U.Ptr 'Nothing) ('Mut s)
    -> m (U.ListOf ('U.Ptr 'Nothing) ('Mut s))
{-# SPECIALIZE copyCanonicalPtrList
    :: U.ListOf ('U.Ptr 'Nothing) 'Const
    -> U.ListOf ('U.Ptr 'Nothing) ('Mut RealWorld)
    -> LimitT IO (U.ListOf ('U.Ptr 'Nothing) ('Mut RealWorld))
    #-}
{-# SPECIALIZE copyCanonicalPtrList
    :: U.ListOf ('U.Ptr 'Nothing) ('Mut RealWorld)
    -> U.ListOf ('U.Ptr 'Nothing) ('Mut RealWorld)
    -> LimitT IO (U.ListOf ('U.Ptr 'Nothing) ('Mut RealWorld))
    #-}
copyCanonicalPtrList listIn listOut = do
    for_ [0..U.length listIn - 1] $ \i -> do
        ptrIn <- U.index i listIn
        ptrOut <- cloneCanonicalPtr ptrIn (U.message @(U.ListOf ('U.Ptr 'Nothing)) listOut)
        U.setIndex ptrOut i listOut
    pure listOut

cloneCanonicalStructList
    :: (U.RWCtx m s, M.MonadReadMessage mutIn m)
    => U.ListOf ('U.Ptr ('Just 'U.Struct)) mutIn
    -> M.Message ('Mut s)
    -> m (U.ListOf ('U.Ptr ('Just 'U.Struct)) ('Mut s))
{-# SPECIALIZE cloneCanonicalStructList
    :: U.ListOf ('U.Ptr ('Just ('U.Struct))) 'Const
    -> M.Message ('Mut RealWorld)
    -> LimitT IO (U.ListOf ('U.Ptr ('Just 'U.Struct)) ('Mut RealWorld))
    #-}
{-# SPECIALIZE cloneCanonicalStructList
    :: U.ListOf ('U.Ptr ('Just 'U.Struct)) ('Mut RealWorld)
    -> M.Message ('Mut RealWorld)
    -> LimitT IO (U.ListOf ('U.Ptr ('Just 'U.Struct)) ('Mut RealWorld))
    #-}
cloneCanonicalStructList listIn msgOut = do
    (nWords, nPtrs) <- findCanonicalListSectionCounts listIn
    listOut <- U.allocCompositeList msgOut nWords nPtrs (U.length listIn)
    copyCanonicalStructList listIn listOut
    pure listOut

copyCanonicalStructList
    :: (U.RWCtx m s, M.MonadReadMessage mutIn m)
    => U.ListOf ('U.Ptr ('Just 'U.Struct)) mutIn
    -> U.ListOf ('U.Ptr ('Just 'U.Struct)) ('Mut s)
    -> m ()
{-# SPECIALIZE copyCanonicalStructList
    :: U.ListOf ('U.Ptr ('Just 'U.Struct)) 'Const
    -> U.ListOf ('U.Ptr ('Just 'U.Struct)) ('Mut RealWorld)
    -> LimitT IO ()
    #-}
{-# SPECIALIZE copyCanonicalStructList
    :: U.ListOf ('U.Ptr ('Just 'U.Struct)) ('Mut RealWorld)
    -> U.ListOf ('U.Ptr ('Just 'U.Struct)) ('Mut RealWorld)
    -> LimitT IO ()
    #-}
copyCanonicalStructList listIn listOut =
    for_ [0..U.length listIn - 1] $ \i -> do
        structIn <- U.index i listIn
        structOut <- U.index i listOut
        copyCanonicalStruct structIn structOut

findCanonicalListSectionCounts
    :: U.ReadCtx m mut
    => U.ListOf ('U.Ptr ('Just 'U.Struct)) mut -> m (Word16, Word16)
{-# SPECIALIZE findCanonicalListSectionCounts
    :: U.ListOf ('U.Ptr ('Just 'U.Struct)) 'Const -> LimitT IO (Word16, Word16)
    #-}
{-# SPECIALIZE findCanonicalListSectionCounts
    :: U.ListOf ('U.Ptr ('Just 'U.Struct)) ('Mut RealWorld) -> LimitT IO (Word16, Word16)
    #-}
findCanonicalListSectionCounts list = go 0 0 0 where
    go i !nWords !nPtrs
        | i >= U.length list =
            pure (nWords, nPtrs)
        | otherwise = do
            struct <- U.index i list
            (nWords', nPtrs') <- findCanonicalSectionCounts struct
            go (i+1) (max nWords nWords') (max nPtrs nPtrs')


{-
do
    -- Generate specializations for various functions above.
    --
    -- TODO: Figure out why this version doesn't seem to be taking
    -- effect; having the explicit SPECIALIZE pragmas written out
    -- literally results in a 15-20% speedup vs. trying to generate
    -- them with template haskell. But ew.
    --
    -- TODO(cleanup): find some way to group the signature & the specialization together
    -- without duplicating everything.
    let specializations :: [(TH.Name, [TH.TypeQ])]
        specializations =
            [ ( 'copyCanonicalStructList
              , each $ \mutIn mutOut m ->
                    [t| U.ListOf $mutIn (U.Struct $mutIn)
                        -> U.ListOf $mutOut (U.Struct $mutOut) -> $m ()
                    |]
              )
            , ( 'copyCanonicalDataList
              , each $ \mutIn mutOut m -> do
                    a <- pure . TH.VarT <$> TH.newName "a"
                    [t| U.ListOf $mutIn $a -> U.ListOf $mutOut $a -> $m (U.ListOf $mutOut $a) |]
              )
            , ( 'copyCanonicalPtrList
              , each $ \mutIn mutOut m ->
                    [t| U.ListOf $mutIn (Maybe (U.Ptr $mutIn))
                        -> U.ListOf $mutOut (Maybe (U.Ptr $mutOut))
                        -> $m (U.ListOf $mutOut (Maybe (U.Ptr $mutOut)))
                    |]
              )
            , ( 'cloneCanonicalPtr
              , each $ \mutIn mutOut m ->
                    [t| Maybe (U.Ptr $mutIn) -> M.Message $mutOut -> $m (Maybe (U.Ptr $mutOut)) |]
              )
            , ( 'cloneCanonicalList
              , each $ \mutIn mutOut m ->
                    [t| U.List $mutIn -> M.Message $mutOut -> $m (U.List $mutOut) |]
              )
            , ( 'cloneCanonicalStruct
              , each $ \mutIn mutOut m ->
                    [t| U.Struct $mutIn -> M.Message $mutOut -> $m (U.Struct $mutOut) |]
              )
            , ( 'copyCanonicalStruct
              , each $ \mutIn mutOut m ->
                    [t| U.Struct $mutIn -> U.Struct $mutOut -> $m () |]
              )
            ]
        each f = do
            let s = pure $ TH.VarT (TH.mkName "s")
            (m, s) <- [ ( [t| LimitT IO |], [t| RealWorld |] )
                      , ( [t| PureBuilder $s |], s )
                      ]
            mutIn <- [ [t| 'Const |], [t| 'Mut $s |] ]
            pure $ f mutIn [t| 'Mut $s |] m
{-
            map
                (\t -> f t [t| 'Mut RealWorld |] [t| LimitT IO |])
                [ [t| 'Const |], [t| 'Mut RealWorld |] ]
-}
    concat <$> for specializations (\(name, types) -> do
        for types $ \mkType -> do
            t <- mkType
            pure $ TH.PragmaD $ TH.SpecialiseP name t Nothing TH.AllPhases)
-}
