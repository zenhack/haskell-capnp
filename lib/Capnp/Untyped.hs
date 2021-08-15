{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-error=deprecations #-}
{-|
Module: Capnp.Untyped
Description: Utilities for reading capnproto messages with no schema.

The types and functions in this module know about things like structs and
lists, but are not schema aware.

Each of the data types exported by this module is parametrized over the
mutability of the message it contains (see "Capnp.Message").
-}
module Capnp.Untyped
    (
    -- * Type-level descriptions of wire representations.
      Repr(..)
    , PtrRepr(..)
    , ListRepr(..)
    , NormalListRepr(..)
    , DataSz(..)

    -- * Mapping representations to value types.
    , Untyped
    , UntypedData
    , UntypedPtr
    , UntypedSomePtr
    , UntypedList
    , UntypedSomeList
    -- ** TODO
    , IgnoreMut(..)
    , MaybePtr(..)
    , Unwrapped

    -- * Relating the representations of lists & their elements.
    , Element(..)
    , ListItem(..)
    , ElemRepr
    , ListReprFor

    -- * Working with pointers
    , IsPtrRepr(..)
    , IsListPtrRepr(..)

    -- * Allocating values
    , Allocate(..)
    , AllocateNormalList(..)

    , Ptr(..), List(..), Struct, ListOf, Cap
    , structByteCount
    , structWordCount
    , structPtrCount
    , structListByteCount
    , structListWordCount
    , structListPtrCount
    , getData, getPtr
    , setData, setPtr
    , copyStruct
    , copyPtr
    , copyList
    , copyCap
    , copyListOf
    , getClient
    , get, index
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
    , appendCap

    , TraverseMsg(..)
    )
  where

import Prelude hiding (length, take)

import Data.Bits
import Data.Word

import Control.Exception.Safe    (impureThrow)
import Control.Monad             (forM_, unless)
import Control.Monad.Catch       (MonadCatch, MonadThrow(throwM))
import Control.Monad.Catch.Pure  (CatchT(runCatchT))
import Control.Monad.Primitive   (PrimMonad(..))
import Control.Monad.ST          (RealWorld)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Kind                 (Type)

import qualified Data.ByteString     as BS
import qualified Language.Haskell.TH as TH

import Capnp.Address        (OffsetError(..), WordAddr(..), pointerFrom)
import Capnp.Bits
    ( BitCount(..)
    , ByteCount(..)
    , Word1(..)
    , WordCount(..)
    , bitsToBytesCeil
    , bytesToWordsCeil
    , replaceBits
    , wordsToBytes
    )
import Capnp.Mutability     (MaybeMutable(..), Mutability(..))
import Capnp.TraversalLimit (LimitT, MonadLimit(invoice))

import qualified Capnp.Errors  as E
import qualified Capnp.Message as M
import qualified Capnp.Pointer as P

-------------------------------------------------------------------------------
-- Untyped refernces to values in a message.
-------------------------------------------------------------------------------

-- | A an absolute pointer to a value (of arbitrary type) in a message.
-- Note that there is no variant for far pointers, which don't make sense
-- with absolute addressing.
data Ptr mut
    = PtrCap (Cap mut)
    | PtrList (List mut)
    | PtrStruct (Struct mut)

-- | A list of values (of arbitrary type) in a message.
data List mut
    = List0 (ListOf ('Data 'Sz0) mut)
    | List1 (ListOf ('Data 'Sz1) mut)
    | List8 (ListOf ('Data 'Sz8) mut)
    | List16 (ListOf ('Data 'Sz16) mut)
    | List32 (ListOf ('Data 'Sz32) mut)
    | List64 (ListOf ('Data 'Sz64) mut)
    | ListPtr (ListOf ('Ptr 'Nothing) mut)
    | ListStruct (ListOf ('Ptr ('Just 'Struct)) mut)

-- | A "normal" (non-composite) list.
data NormalList mut = NormalList
    { nPtr :: {-# UNPACK #-} !(M.WordPtr mut)
    , nLen :: !Int
    }

data StructList mut = StructList
    { slFirst :: Struct mut
    -- ^ First element. data/ptr sizes are the same for
    -- all elements.
    , slLen   :: !Int
    -- ^ Number of elements
    }

-- | A list of values with representation 'r' in a message.
newtype ListOf r mut = ListOf (ListRepOf r mut)

type family ListRepOf (r :: Repr) :: Mutability -> * where
    ListRepOf ('Ptr ('Just 'Struct)) = StructList
    ListRepOf r = NormalList

-- | @'ListItem' r@ indicates that @r@ is a representation for elements of some list
-- type. Not every representation is covered; instances exist only for @r@ where
-- @'ElemRepr' ('ListReprFor' r) ~ r@.
class Element r => ListItem (r :: Repr) where
    -- | Returns the length of a list
    length :: ListOf r mut -> Int

    -- underlying implementations of index, setIndex and take, but
    -- without bounds checking. Don't call these directly.
    unsafeIndex :: ReadCtx m mut => Int -> ListOf r mut -> m (Unwrapped (Untyped r mut))
    unsafeSetIndex
        :: (RWCtx m s, a ~ Unwrapped (Untyped r ('Mut s)))
        => a -> Int -> ListOf r ('Mut s) -> m ()
    unsafeTake :: Int -> ListOf r mut -> ListOf r mut

    checkListOf :: ReadCtx m mut => ListOf r mut -> m ()

    default length :: (ListRepOf r ~ NormalList) => ListOf r mut -> Int
    length (ListOf nlist) = nLen nlist
    {-# INLINE length #-}

    default unsafeIndex ::
        forall m mut.
        ( ReadCtx m mut
        , Integral (Unwrapped (Untyped r mut))
        , ListRepOf r ~ NormalList
        , FiniteBits (Unwrapped (Untyped r mut))
        ) => Int -> ListOf r mut -> m (Unwrapped (Untyped r mut))
    unsafeIndex i (ListOf nlist) =
        unsafeIndexBits @(Unwrapped (Untyped r mut)) i nlist
    {-# INLINE unsafeIndex #-}

    default unsafeSetIndex ::
        forall m s a.
        ( RWCtx m s
        , a ~ Unwrapped (Untyped r ('Mut s))
        , ListRepOf r ~ NormalList
        , Integral a
        , Bounded a
        , FiniteBits a
        ) => a -> Int -> ListOf r ('Mut s) -> m ()
    unsafeSetIndex value i (ListOf nlist) =
        unsafeSetIndexBits @(Unwrapped (Untyped r ('Mut s))) value i nlist
    {-# INLINE unsafeSetIndex #-}

    default unsafeTake :: ListRepOf r ~ NormalList => Int -> ListOf r mut -> ListOf r mut
    unsafeTake count (ListOf NormalList{..}) = ListOf NormalList{ nLen = count, .. }
    {-# INLINE unsafeTake #-}

    default checkListOf ::
        forall m mut.
        ( ReadCtx m mut
        , ListRepOf r ~ NormalList
        , FiniteBits (Untyped r mut)
        ) => ListOf r mut -> m ()
    checkListOf (ListOf l) = checkNormalList
        l
        (fromIntegral $ finiteBitSize (undefined :: Untyped r mut))
    {-# INLINE checkListOf #-}

unsafeIndexBits
    :: forall a m mut.
    ( ReadCtx m mut
    , FiniteBits a
    , Integral a
    ) => Int -> NormalList mut -> m a
{-# INLINE unsafeIndexBits #-}
unsafeIndexBits i nlist =
    indexNList @a i nlist (64 `div` finiteBitSize (undefined :: a))

unsafeSetIndexBits
    :: forall a m s.
    ( RWCtx m s
    , Bounded a
    , FiniteBits a
    , Integral a
    ) => a -> Int -> NormalList ('Mut s) -> m ()
{-# INLINE unsafeSetIndexBits #-}
unsafeSetIndexBits value i nlist =
    setNIndex @a i nlist (64 `div` finiteBitSize value) value

indexNList
    :: forall a m mut. (ReadCtx m mut, Integral a)
    => Int -> NormalList mut -> Int -> m a
{-# INLINE indexNList #-}
indexNList i (NormalList M.WordPtr{pSegment, pAddr=WordAt{..}} _) eltsPerWord = do
    let wordIndex' = wordIndex + WordCount (i `div` eltsPerWord)
    word <- M.read pSegment wordIndex'
    let shift = (i `mod` eltsPerWord) * (64 `div` eltsPerWord)
    pure $ fromIntegral $ word `shiftR` shift

setNIndex
    :: forall a m s. (RWCtx m s, Bounded a, Integral a)
    => Int -> NormalList ('Mut s) -> Int -> a -> m ()
{-# INLINE setNIndex #-}
setNIndex i NormalList{nPtr=M.WordPtr{pSegment, pAddr=WordAt{wordIndex}}} eltsPerWord value = do
    let eltWordIndex = wordIndex + WordCount (i `div` eltsPerWord)
    word <- M.read pSegment eltWordIndex
    let shift = (i `mod` eltsPerWord) * (64 `div` eltsPerWord)
    M.write pSegment eltWordIndex $ replaceBits value word shift

setPtrIndex :: RWCtx m s => Int -> NormalList ('Mut s) -> Ptr ('Mut s) -> P.Ptr -> m ()
{-# INLINE setPtrIndex #-}
setPtrIndex i NormalList{nPtr=nPtr@M.WordPtr{pAddr=addr@WordAt{wordIndex}}} absPtr relPtr =
    let srcPtr = nPtr { M.pAddr = addr { wordIndex = wordIndex + WordCount i } }
    in setPointerTo srcPtr (ptrAddr absPtr) relPtr

instance ListItem ('Ptr ('Just 'Struct)) where
    length (ListOf (StructList _ len)) = len
    {-# INLINE length #-}
    unsafeIndex i (ListOf (StructList (StructAt ptr@M.WordPtr{pAddr=addr@WordAt{..}} dataSz ptrSz) _)) = do
        let offset = WordCount $ i * (fromIntegral dataSz + fromIntegral ptrSz)
        let addr' = addr { wordIndex = wordIndex + offset }
        return $ StructAt ptr { M.pAddr = addr' } dataSz ptrSz
    {-# INLINE unsafeIndex #-}
    unsafeSetIndex value i list = do
        dest <- unsafeIndex i list
        copyStruct dest value
    unsafeTake count (ListOf (StructList s _)) = ListOf (StructList s count)
    {-# INLINE unsafeTake #-}

    checkListOf (ListOf (StructList s@(StructAt ptr _ _) len)) =
        checkPtrOffset ptr (fromIntegral len * structSize s)
    {-# INLINE checkListOf #-}

instance ListItem ('Data 'Sz0)  where
    unsafeIndex _ _ = pure ()
    {-# INLINE unsafeIndex #-}
    unsafeSetIndex _ _ _ = pure ()
    {-# INLINE unsafeSetIndex #-}
    checkListOf _ = pure ()
    {-# INLINE checkListOf #-}

instance ListItem ('Data 'Sz1) where
    unsafeIndex i (ListOf nlist) = do
        Word1 val <- unsafeIndexBits @Word1 i nlist
        pure val
    {-# INLINE unsafeIndex #-}
    unsafeSetIndex value i (ListOf nlist) =
        unsafeSetIndexBits @Word1 (Word1 value) i nlist
    {-# INLINE unsafeSetIndex #-}
    checkListOf (ListOf l) = checkNormalList l 1
    {-# INLINE checkListOf #-}

instance ListItem ('Data 'Sz8)
instance ListItem ('Data 'Sz16)
instance ListItem ('Data 'Sz32)
instance ListItem ('Data 'Sz64)

instance ListItem ('Ptr 'Nothing) where
    unsafeIndex i (ListOf (NormalList ptr@M.WordPtr{pAddr=addr@WordAt{..}} _)) =
        get ptr { M.pAddr = addr { wordIndex = wordIndex + WordCount i } }
    {-# INLINE unsafeIndex #-}
    unsafeSetIndex value i list@(ListOf nlist) = case value of
        Just p | message @Ptr p /= message @(ListOf ('Ptr 'Nothing)) list -> do
            newPtr <- copyPtr (message @(ListOf ('Ptr 'Nothing)) list) value
            unsafeSetIndex newPtr i list
        Nothing ->
            setNIndex i nlist 1 (P.serializePtr Nothing)
        Just (PtrCap (CapAt _ cap)) ->
            setNIndex i nlist 1 (P.serializePtr (Just (P.CapPtr cap)))
        Just p@(PtrList ptrList) ->
            setPtrIndex i nlist p $ P.ListPtr 0 (listEltSpec ptrList)
        Just p@(PtrStruct (StructAt _ dataSz ptrSz)) ->
            setPtrIndex i nlist p $ P.StructPtr 0 dataSz ptrSz
    {-# INLINABLE unsafeSetIndex #-}

    checkListOf (ListOf l) = checkNormalList l 64
    {-# INLINE checkListOf #-}

-- | A Capability in a message.
data Cap mut = CapAt (M.Message mut) !Word32

-- | A struct value in a message.
data Struct mut
    = StructAt
        {-# UNPACK #-} !(M.WordPtr mut) -- Start of struct
        !Word16 -- Data section size.
        !Word16 -- Pointer section size.

-- | Type (constraint) synonym for the constraints needed for most read
-- operations.
type ReadCtx m mut = (M.MonadReadMessage mut m, MonadThrow m, MonadLimit m)

-- | Synonym for ReadCtx + WriteCtx
type RWCtx m s = (ReadCtx m ('Mut s), M.WriteCtx m s)

-- | A 'Repr' describes a wire representation for a value. This is
-- mostly used at the type level (using DataKinds); types are
-- parametrized over representations.
data Repr
    = Ptr (Maybe PtrRepr)
    -- ^ Pointer type. 'Nothing' indicates an AnyPointer, 'Just' describes
    -- a more specific pointer type.
    | Data DataSz
    -- ^ Non-pointer type.
    deriving(Show)

-- | Information about the representation of a pointer type
data PtrRepr
    = Cap
    -- ^ Capability pointer.
    | List (Maybe ListRepr)
    -- ^ List pointer. 'Nothing' describes an AnyList, 'Just' describes
    -- more specific list types.
    | Struct
    -- ^ A struct (or group).
    deriving(Show)

-- | Information about the representation of a list type.
data ListRepr where
    -- | A "normal" list
    ListNormal :: NormalListRepr -> ListRepr
    ListComposite :: ListRepr
    deriving(Show)

-- | Information about the representation of a normal (non-composite) list.
data NormalListRepr where
    NormalListData :: DataSz -> NormalListRepr
    NormalListPtr :: NormalListRepr
    deriving(Show)

-- | The size of a non-pointer type. @SzN@ represents an @N@-bit value.
data DataSz = Sz0 | Sz1 | Sz8 | Sz16 | Sz32 | Sz64
    deriving(Show)

newtype IgnoreMut a (mut :: Mutability) = IgnoreMut a
    deriving(Show, Read, Eq, Ord, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits)
newtype MaybePtr (mut :: Mutability) = MaybePtr (Maybe (Ptr mut))

type family Unwrapped a where
    Unwrapped (IgnoreMut a mut) = a
    Unwrapped (MaybePtr mut) = Maybe (Ptr mut)
    Unwrapped a = a

-- | @Untyped r mut@ is an untyped value with representation @r@ stored in
-- a message with mutability @mut@.
type family Untyped (r :: Repr) :: Mutability -> Type where
    Untyped ('Data sz) = IgnoreMut (UntypedData sz)
    Untyped ('Ptr ptr) = UntypedPtr ptr

-- | @UntypedData sz@ is an untyped value with size @sz@.
type family UntypedData (sz :: DataSz) :: Type where
    UntypedData 'Sz0 = ()
    UntypedData 'Sz1 = Bool
    UntypedData 'Sz8 = Word8
    UntypedData 'Sz16 = Word16
    UntypedData 'Sz32 = Word32
    UntypedData 'Sz64 = Word64

-- | Like 'Untyped', but for pointers only.
type family UntypedPtr (r :: Maybe PtrRepr) :: Mutability -> Type where
    UntypedPtr 'Nothing = MaybePtr
    UntypedPtr ('Just r) = UntypedSomePtr r

-- | Like 'UntypedPtr', but doesn't allow AnyPointers.
type family UntypedSomePtr (r :: PtrRepr) :: Mutability -> Type where
    UntypedSomePtr 'Struct = Struct
    UntypedSomePtr 'Cap = Cap
    UntypedSomePtr ('List r) = UntypedList r

-- | Like 'Untyped', but for lists only.
type family UntypedList (r :: Maybe ListRepr) :: Mutability ->  Type where
    UntypedList 'Nothing = List
    UntypedList ('Just r) = UntypedSomeList r

-- | Like 'UntypedList', but doesn't allow AnyLists.
type family UntypedSomeList (r :: ListRepr) :: Mutability -> Type where
    UntypedSomeList r = ListOf (ElemRepr r)

-- | @ElemRepr r@ is the representation of elements of lists with
-- representation @r@.
type family ElemRepr (rl :: ListRepr) :: Repr where
    ElemRepr 'ListComposite = 'Ptr ('Just 'Struct)
    ElemRepr ('ListNormal 'NormalListPtr) = 'Ptr 'Nothing
    ElemRepr ('ListNormal ('NormalListData sz)) = 'Data sz

-- | @ListReprFor e@ is the representation of lists with elements
-- whose representation is @e@.
type family ListReprFor (e :: Repr) :: ListRepr where
    ListReprFor ('Data sz) = 'ListNormal ('NormalListData sz)
    ListReprFor ('Ptr ('Just 'Struct)) = 'ListComposite
    ListReprFor ('Ptr a) = 'ListNormal 'NormalListPtr

-- | 'Element' supports converting between values of representation
-- @'ElemRepr' ('ListReprFor' r)@ and values of representation @r@.
--
-- At a glance, you might expect this to just be a no-op, but it is actually
-- *not* always the case that @'ElemRepr' ('ListReprFor' r) ~ r@; in the
-- case of pointer types, @'ListReprFor' r@ can contain arbitrary pointers,
-- so information is lost, and it is possible for the list to contain pointers
-- of the incorrect type. In this case, 'fromElement' will throw an error.
--
-- 'toElement' is more trivial.
class Element (r :: Repr) where
    fromElement
        :: forall m mut. ReadCtx m mut
        => M.Message mut
        -> Unwrapped (Untyped (ElemRepr (ListReprFor r)) mut)
        -> m (Unwrapped (Untyped r mut))
    toElement :: Unwrapped (Untyped r mut) -> Unwrapped (Untyped (ElemRepr (ListReprFor r)) mut)

-- | Operations on types with pointer representations.
class IsPtrRepr (r :: Maybe PtrRepr) where
    toPtr :: Unwrapped (Untyped ('Ptr r) mut) -> Maybe (Ptr mut)
    -- ^ Convert an untyped value of this representation to an AnyPointer.
    fromPtr :: ReadCtx m mut => M.Message mut -> Maybe (Ptr mut) -> m (Unwrapped (Untyped ('Ptr r) mut))
    -- ^ Extract a value with this representation from an AnyPointer, failing
    -- if the pointer is the wrong type for this representation.

-- | Operations on types with list representations.
class IsListPtrRepr (r :: ListRepr) where
    rToList :: UntypedSomeList r mut -> List mut
    -- ^ Convert an untyped value of this representation to an AnyList.
    rFromList :: ReadCtx m mut => List mut -> m (UntypedSomeList r mut)
    -- ^ Extract a value with this representation from an AnyList, failing
    -- if the list is the wrong type for this representation.
    rFromListMsg :: ReadCtx m mut => M.Message mut -> m (UntypedSomeList r mut)
    -- ^ Create a zero-length value with this representation, living in the
    -- provided message.

-- helper function for throwing SchemaViolationError "expected ..."
expected :: MonadThrow m => String -> m a
expected msg = throwM $ E.SchemaViolationError $ "expected " ++ msg


-------------------------------------------------------------------------------
-- 'Element' instances
-------------------------------------------------------------------------------

instance Element ('Data sz) where
    fromElement _ = pure
    toElement = id
    {-# INLINE fromElement #-}
    {-# INLINE toElement #-}
instance Element ('Ptr ('Just 'Struct)) where
    fromElement _ = pure
    toElement = id
    {-# INLINE fromElement #-}
    {-# INLINE toElement #-}
instance Element ('Ptr 'Nothing) where
    fromElement _ = pure
    toElement = id
    {-# INLINE fromElement #-}
    {-# INLINE toElement #-}
instance Element ('Ptr ('Just 'Cap)) where
    fromElement = fromPtr @('Just 'Cap)
    toElement = Just . PtrCap
    {-# INLINE fromElement #-}
    {-# INLINE toElement #-}
instance IsPtrRepr ('Just ('List a)) => Element ('Ptr ('Just ('List a))) where
    fromElement = fromPtr @('Just ('List a))
    toElement = toPtr @('Just ('List a))
    {-# INLINE fromElement #-}
    {-# INLINE toElement #-}

-------------------------------------------------------------------------------
-- 'IsPtrRepr' instances
-------------------------------------------------------------------------------

instance IsPtrRepr 'Nothing where
    toPtr p = p
    fromPtr _ = pure
    {-# INLINE toPtr #-}
    {-# INLINE fromPtr #-}
instance IsPtrRepr ('Just 'Struct) where
    toPtr s = Just (PtrStruct s)
    fromPtr msg Nothing            = messageDefault @Struct msg
    fromPtr _ (Just (PtrStruct s)) = pure s
    fromPtr _ _                    = expected "pointer to struct"
    {-# INLINE toPtr #-}
    {-# INLINE fromPtr #-}
instance IsPtrRepr ('Just 'Cap) where
    toPtr c = Just (PtrCap c)
    fromPtr _ Nothing           = expected "pointer to capability"
    fromPtr _ (Just (PtrCap c)) = pure c
    fromPtr _ _                 = expected "pointer to capability"
    {-# INLINE toPtr #-}
    {-# INLINE fromPtr #-}
instance IsPtrRepr ('Just ('List 'Nothing)) where
    toPtr l = Just (PtrList l)
    fromPtr _ Nothing            = expected "pointer to list"
    fromPtr _ (Just (PtrList l)) = pure l
    fromPtr _ (Just _)           = expected "pointer to list"
    {-# INLINE toPtr #-}
    {-# INLINE fromPtr #-}
instance IsListPtrRepr r => IsPtrRepr ('Just ('List ('Just r))) where
    toPtr l = Just (PtrList (rToList @r l))
    fromPtr msg Nothing          = rFromListMsg @r msg
    fromPtr _ (Just (PtrList l)) = rFromList @r l
    fromPtr _ (Just _)           = expected "pointer to list"
    {-# INLINE toPtr #-}
    {-# INLINE fromPtr #-}

-- | N.B. this should mostly be considered an implementation detail, but
-- it is exposed because it is used by generated code.
--
-- 'TraverseMsg' is similar to 'Traversable' from the prelude, but
-- the intent is that rather than conceptually being a "container",
-- the instance is a value backed by a message, and the point of the
-- type class is to be able to apply transformations to the underlying
-- message.
--
-- We don't just use 'Traversable' for this for two reasons:
--
-- 1. While algebraically it makes sense, it would be very unintuitive to
--    e.g. have the 'Traversable' instance for 'List' not traverse over the
--    *elements* of the list.
-- 2. For the instance for WordPtr, we actually need a stronger constraint than
--    Applicative in order for the implementation to type check. A previous
--    version of the library *did* have @tMsg :: Applicative m => ...@, but
--    performance considerations eventually forced us to open up the hood a
--    bit.
class TraverseMsg f where
    tMsg :: TraverseMsgCtx m mutA mutB => (M.Message mutA -> m (M.Message mutB)) -> f mutA -> m (f mutB)

type TraverseMsgCtx m mutA mutB =
    ( MonadThrow m
    , M.MonadReadMessage mutA m
    , M.MonadReadMessage mutB m
    )

instance TraverseMsg M.WordPtr where
    tMsg f M.WordPtr{pMessage, pAddr=pAddr@WordAt{segIndex}} = do
        msg' <- f pMessage
        seg' <- M.getSegment msg' segIndex
        pure M.WordPtr
            { pMessage = msg'
            , pSegment = seg'
            , pAddr
            }

instance TraverseMsg Ptr where
    tMsg f = \case
        PtrCap cap ->
            PtrCap <$> tMsg f cap
        PtrList l ->
            PtrList <$> tMsg f l
        PtrStruct s ->
            PtrStruct <$> tMsg f s

instance TraverseMsg Cap where
    tMsg f (CapAt msg n) = CapAt <$> f msg <*> pure n

instance TraverseMsg Struct where
    tMsg f (StructAt ptr dataSz ptrSz) = StructAt
        <$> tMsg f ptr
        <*> pure dataSz
        <*> pure ptrSz

instance TraverseMsg List where
    tMsg f = \case
        List0      l -> List0      <$> tMsg f l
        List1      l -> List1      <$> tMsg f l
        List8      l -> List8      <$> tMsg f l
        List16     l -> List16     <$> tMsg f l
        List32     l -> List32     <$> tMsg f l
        List64     l -> List64     <$> tMsg f l
        ListPtr    l -> ListPtr    <$> tMsg f l
        ListStruct l -> ListStruct <$> tMsg f l

instance TraverseMsg (ListRepOf r) => TraverseMsg (ListOf r) where
    tMsg f (ListOf l) = ListOf <$> tMsg f l

instance TraverseMsg NormalList where
    tMsg f NormalList{..} = do
        ptr <- tMsg f nPtr
        pure NormalList { nPtr = ptr, .. }

instance TraverseMsg StructList where
    tMsg f StructList{..} = do
        s <- tMsg f slFirst
        pure StructList { slFirst = s, .. }

-------------------------------------------------------------------------------

-- | Types whose storage is owned by a message..
class HasMessage (f :: Mutability -> *) where
    -- | Get the message in which the value is stored.
    message :: Unwrapped (f mut) -> M.Message mut

-- | Types which have a "default" value, but require a message
-- to construct it.
--
-- The default is usually conceptually zero-size. This is mostly useful
-- for generated code, so that it can use standard decoding techniques
-- on default values.
class HasMessage f => MessageDefault f where
    messageDefault :: ReadCtx m mut => M.Message mut -> m (Unwrapped (f mut))

instance HasMessage M.WordPtr where
    message M.WordPtr{pMessage} = pMessage

instance HasMessage Ptr where
    message (PtrCap cap)       = message @Cap cap
    message (PtrList list)     = message @List list
    message (PtrStruct struct) = message @Struct struct

instance HasMessage Cap where
    message (CapAt msg _) = msg

instance HasMessage Struct where
    message (StructAt ptr _ _) = message @M.WordPtr ptr

instance MessageDefault Struct where
    messageDefault msg = do
        pSegment <- M.getSegment msg 0
        pure $ StructAt M.WordPtr{pMessage = msg, pSegment, pAddr = WordAt 0 0} 0 0

instance HasMessage List where
    message (List0 list)      = message @(ListOf ('Data 'Sz0)) list
    message (List1 list)      = message @(ListOf ('Data 'Sz1)) list
    message (List8 list)      = message @(ListOf ('Data 'Sz8)) list
    message (List16 list)     = message @(ListOf ('Data 'Sz16)) list
    message (List32 list)     = message @(ListOf ('Data 'Sz32)) list
    message (List64 list)     = message @(ListOf ('Data 'Sz64)) list
    message (ListPtr list)    = message @(ListOf ('Ptr 'Nothing)) list
    message (ListStruct list) = message @(ListOf ('Ptr ('Just 'Struct))) list

instance HasMessage (ListOf ('Ptr ('Just 'Struct))) where
    message (ListOf list)    = message @StructList list

instance MessageDefault (ListOf ('Ptr ('Just 'Struct))) where
    messageDefault msg = ListOf <$> messageDefault @StructList msg

instance {-# OVERLAPS #-} ListRepOf r ~ NormalList => HasMessage (ListOf r) where
    message (ListOf list)    = message @NormalList list

instance {-# OVERLAPS #-} ListRepOf r ~ NormalList => MessageDefault (ListOf r) where
    messageDefault msg = ListOf <$> messageDefault @NormalList msg

instance HasMessage NormalList where
    message = M.pMessage . nPtr

instance MessageDefault NormalList where
    messageDefault msg = do
        pSegment <- M.getSegment msg 0
        pure NormalList
            { nPtr = M.WordPtr { pMessage = msg, pSegment, pAddr = WordAt 0 0 }
            , nLen = 0
            }

instance HasMessage StructList where
    message (StructList s _) = message @Struct s

instance MessageDefault StructList where
    messageDefault msg = StructList
        <$> messageDefault @Struct msg
        <*> pure 0

-- | Extract a client (indepedent of the messsage) from the capability.
getClient :: ReadCtx m mut => Cap mut -> m M.Client
{-# INLINABLE getClient #-}
getClient (CapAt msg idx) = M.getCap msg (fromIntegral idx)

-- | @get ptr@ returns the Ptr stored at @ptr@.
-- Deducts 1 from the quota for each word read (which may be multiple in the
-- case of far pointers).
get :: ReadCtx m mut => M.WordPtr mut -> m (Maybe (Ptr mut))
{-# INLINABLE get #-}
{-# SPECIALIZE get :: M.WordPtr ('Mut RealWorld) -> LimitT IO (Maybe (Ptr ('Mut RealWorld))) #-}
get ptr@M.WordPtr{pMessage, pAddr} = do
    word <- getWord ptr
    case P.parsePtr word of
        Nothing -> return Nothing
        Just p -> case p of
            P.CapPtr cap -> return $ Just $ PtrCap (CapAt pMessage cap)
            P.StructPtr off dataSz ptrSz -> return $ Just $ PtrStruct $
                StructAt ptr { M.pAddr = resolveOffset pAddr off } dataSz ptrSz
            P.ListPtr off eltSpec -> Just <$>
                getList ptr { M.pAddr = resolveOffset pAddr off } eltSpec
            P.FarPtr twoWords offset segment -> do
                landingSegment <- M.getSegment pMessage (fromIntegral segment)
                let addr' = WordAt { wordIndex = fromIntegral offset
                                   , segIndex = fromIntegral segment
                                   }
                let landingPtr = M.WordPtr
                        { pMessage
                        , pSegment = landingSegment
                        , pAddr = addr'
                        }
                if not twoWords
                    then do
                        -- XXX: invoice so we don't open ourselves up to DoS
                        -- in the case of a chain of far pointers -- but a
                        -- better solution would be to just reject after the
                        -- first chain since this isn't actually legal. TODO
                        -- refactor (and then get rid of the MonadLimit
                        -- constraint).
                        invoice 1
                        get landingPtr
                    else do
                        landingPad <- getWord landingPtr
                        case P.parsePtr landingPad of
                            Just (P.FarPtr False off seg) -> do
                                let segIndex = fromIntegral seg
                                finalSegment <- M.getSegment pMessage segIndex
                                tagWord <- getWord M.WordPtr
                                    { pMessage
                                    , pSegment = landingSegment
                                    , M.pAddr = addr' { wordIndex = wordIndex addr' + 1 }
                                    }
                                let finalPtr = M.WordPtr
                                        { pMessage
                                        , pSegment = finalSegment
                                        , pAddr = WordAt
                                            { wordIndex = fromIntegral off
                                            , segIndex
                                            }
                                        }
                                case P.parsePtr tagWord of
                                    Just (P.StructPtr 0 dataSz ptrSz) ->
                                        return $ Just $ PtrStruct $
                                            StructAt finalPtr dataSz ptrSz
                                    Just (P.ListPtr 0 eltSpec) ->
                                        Just <$> getList finalPtr eltSpec
                                    -- TODO: I'm not sure whether far pointers to caps are
                                    -- legal; it's clear how they would work, but I don't
                                    -- see a use, and the spec is unclear. Should check
                                    -- how the reference implementation does this, copy
                                    -- that, and submit a patch to the spec.
                                    Just (P.CapPtr cap) ->
                                        return $ Just $ PtrCap (CapAt pMessage cap)
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
    getWord M.WordPtr{pSegment, pAddr=WordAt{wordIndex}} =
        M.read pSegment wordIndex
    resolveOffset addr@WordAt{..} off =
        addr { wordIndex = wordIndex + fromIntegral off + 1 }
    getList ptr@M.WordPtr{pAddr=addr@WordAt{wordIndex}} eltSpec = PtrList <$>
        case eltSpec of
            P.EltNormal sz len -> pure $ case sz of
                P.Sz0   -> List0   (ListOf nlist)
                P.Sz1   -> List1   (ListOf nlist)
                P.Sz8   -> List8   (ListOf nlist)
                P.Sz16  -> List16  (ListOf nlist)
                P.Sz32  -> List32  (ListOf nlist)
                P.Sz64  -> List64  (ListOf nlist)
                P.SzPtr -> ListPtr (ListOf nlist)
              where
                nlist = NormalList ptr (fromIntegral len)
            P.EltComposite _ -> do
                tagWord <- getWord ptr
                case P.parsePtr' tagWord of
                    P.StructPtr numElts dataSz ptrSz ->
                        pure $ ListStruct $ ListOf $ StructList
                            (StructAt
                                ptr { M.pAddr = addr { wordIndex = wordIndex + 1 } }
                                dataSz
                                ptrSz)
                            (fromIntegral numElts)
                    tag -> throwM $ E.InvalidDataError $
                        "Composite list tag was not a struct-" ++
                        "formatted word: " ++ show tag

-- | Return the EltSpec needed for a pointer to the given list.
listEltSpec :: List msg -> P.EltSpec
listEltSpec (ListStruct list@(ListOf (StructList (StructAt _ dataSz ptrSz) _))) =
    P.EltComposite $ fromIntegral (length list) * (fromIntegral dataSz + fromIntegral ptrSz)
listEltSpec (List0 list)   = P.EltNormal P.Sz0 $ fromIntegral (length list)
listEltSpec (List1 list)   = P.EltNormal P.Sz1 $ fromIntegral (length list)
listEltSpec (List8 list)   = P.EltNormal P.Sz8 $ fromIntegral (length list)
listEltSpec (List16 list)  = P.EltNormal P.Sz16 $ fromIntegral (length list)
listEltSpec (List32 list)  = P.EltNormal P.Sz32 $ fromIntegral (length list)
listEltSpec (List64 list)  = P.EltNormal P.Sz64 $ fromIntegral (length list)
listEltSpec (ListPtr list) = P.EltNormal P.SzPtr $ fromIntegral (length list)

-- | Return the starting address of the list.
listAddr :: List msg -> WordAddr
listAddr (ListStruct (ListOf (StructList (StructAt M.WordPtr{pAddr} _ _) _))) =
    -- pAddr is the address of the first element of the list, but
    -- composite lists start with a tag word:
    pAddr { wordIndex = wordIndex pAddr - 1 }
listAddr (List0 (ListOf NormalList{nPtr=M.WordPtr{pAddr}})) = pAddr
listAddr (List1 (ListOf NormalList{nPtr=M.WordPtr{pAddr}})) = pAddr
listAddr (List8 (ListOf NormalList{nPtr=M.WordPtr{pAddr}})) = pAddr
listAddr (List16 (ListOf NormalList{nPtr=M.WordPtr{pAddr}})) = pAddr
listAddr (List32 (ListOf NormalList{nPtr=M.WordPtr{pAddr}})) = pAddr
listAddr (List64 (ListOf NormalList{nPtr=M.WordPtr{pAddr}})) = pAddr
listAddr (ListPtr (ListOf NormalList{nPtr=M.WordPtr{pAddr}})) = pAddr

-- | Return the address of the pointer's target. It is illegal to call this on
-- a pointer which targets a capability.
ptrAddr :: Ptr msg -> WordAddr
ptrAddr (PtrCap _) = error "ptrAddr called on a capability pointer."
ptrAddr (PtrStruct (StructAt M.WordPtr{pAddr}_ _)) = pAddr
ptrAddr (PtrList list) = listAddr list

-- | @'setIndex value i list@ Set the @i@th element of @list@ to @value@.
setIndex
    :: (RWCtx m s, ListItem r)
    => Unwrapped (Untyped r ('Mut s)) -> Int -> ListOf r ('Mut s) -> m ()
{-# INLINE setIndex #-}
{-# SPECIALIZE setIndex
    :: ListItem r
    => Unwrapped (Untyped r ('Mut RealWorld)) -> Int -> ListOf r ('Mut RealWorld) -> LimitT IO () #-}
setIndex _ i list | i < 0 || length list <= i =
    throwM E.BoundsError { E.index = i, E.maxIndex = length list }
setIndex value i list = unsafeSetIndex value i list

-- | @'setPointerTo' msg srcLoc dstAddr relPtr@ sets the word at @srcLoc@ in @msg@ to a
-- pointer like @relPtr@, but pointing to @dstAddr@. @relPtr@ should not be a far pointer.
-- If the two addresses are in different segments, a landing pad will be allocated and
-- @srcLoc@ will contain a far pointer.
setPointerTo :: M.WriteCtx m s => M.WordPtr ('Mut s) -> WordAddr -> P.Ptr -> m ()
{-# INLINABLE setPointerTo #-}
{-# SPECIALIZE setPointerTo :: M.WordPtr ('Mut RealWorld) -> WordAddr -> P.Ptr -> LimitT IO () #-}
setPointerTo
        M.WordPtr
            { pMessage = msg
            , pSegment=srcSegment
            , pAddr=srcAddr@WordAt{wordIndex=srcWordIndex}
            }
        dstAddr
        relPtr
    | P.StructPtr _ 0 0 <- relPtr =
        -- We special case zero-sized structs, since (1) we don't have to
        -- really point at the correct offset, since they can "fit" anywhere,
        -- and (2) they cause problems with double-far pointers, where part
        -- of the landing pad needs to have a zero offset, but that makes it
        -- look like a null pointer... so we just avoid that case by cutting
        -- it off here.
        M.write srcSegment srcWordIndex $
            P.serializePtr $ Just $ P.StructPtr (-1) 0 0
    | otherwise = case pointerFrom srcAddr dstAddr relPtr of
        Right absPtr ->
            M.write srcSegment srcWordIndex $ P.serializePtr $ Just absPtr
        Left OutOfRange ->
            error "BUG: segment is too large to set the pointer."
        Left DifferentSegments -> do
            -- We need a far pointer; allocate a landing pad in the target segment,
            -- set it to point to the final destination, an then set the source pointer
            -- pointer to point to the landing pad.
            let WordAt{segIndex} = dstAddr
            M.allocInSeg msg segIndex 1 >>= \case
                Just M.WordPtr{pSegment=landingPadSegment, pAddr=landingPadAddr} ->
                    case pointerFrom landingPadAddr dstAddr relPtr of
                        Right landingPad -> do
                            let WordAt{segIndex,wordIndex} = landingPadAddr
                            M.write landingPadSegment wordIndex (P.serializePtr $ Just landingPad)
                            M.write srcSegment srcWordIndex $
                                P.serializePtr $ Just $ P.FarPtr False (fromIntegral wordIndex) (fromIntegral segIndex)
                        Left DifferentSegments ->
                            error "BUG: allocated a landing pad in the wrong segment!"
                        Left OutOfRange ->
                            error "BUG: segment is too large to set the pointer."
                Nothing -> do
                    -- The target segment is full. We need to do a double-far pointer.
                    -- First allocate the 2-word landing pad, wherever it will fit:
                    M.WordPtr
                        { pSegment = landingPadSegment
                        , pAddr = WordAt
                            { wordIndex = landingPadOffset
                            , segIndex = landingPadSegIndex
                            }
                        } <- M.alloc msg 2
                    -- Next, point the source pointer at the landing pad:
                    M.write srcSegment srcWordIndex $
                        P.serializePtr $ Just $ P.FarPtr True
                            (fromIntegral landingPadOffset)
                            (fromIntegral landingPadSegIndex)
                    -- Finally, fill in the landing pad itself.
                    --
                    -- The first word is a far pointer whose offset is the
                    -- starting address of our target object:
                    M.write landingPadSegment landingPadOffset $
                        let WordAt{wordIndex, segIndex} = dstAddr in
                        P.serializePtr $ Just $ P.FarPtr False
                            (fromIntegral wordIndex)
                            (fromIntegral segIndex)
                    -- The second word is a pointer of the right "shape"
                    -- for the target, but with a zero offset:
                    M.write landingPadSegment (landingPadOffset + 1) $
                        P.serializePtr $ Just $ case relPtr of
                            P.StructPtr _ nWords nPtrs -> P.StructPtr 0 nWords nPtrs
                            P.ListPtr _ eltSpec -> P.ListPtr 0 eltSpec
                            _ -> relPtr

-- | Make a copy of a capability inside the target message.
copyCap :: RWCtx m s => M.Message ('Mut s) -> Cap ('Mut s) -> m (Cap ('Mut s))
{-# INLINABLE copyCap #-}
copyCap dest cap = getClient cap >>= appendCap dest

-- | Make a copy of the value at the pointer, in the target message.
copyPtr :: RWCtx m s => M.Message ('Mut s) -> Maybe (Ptr ('Mut s)) -> m (Maybe (Ptr ('Mut s)))
{-# INLINABLE copyPtr #-}
{-# SPECIALIZE copyPtr :: M.Message ('Mut RealWorld) -> Maybe (Ptr ('Mut RealWorld)) -> LimitT IO (Maybe (Ptr ('Mut RealWorld))) #-}
copyPtr _ Nothing                = pure Nothing
copyPtr dest (Just (PtrCap cap))    = Just . PtrCap <$> copyCap dest cap
copyPtr dest (Just (PtrList src))   = Just . PtrList <$> copyList dest src
copyPtr dest (Just (PtrStruct src)) = Just . PtrStruct <$> do
    destStruct <- allocStruct
            dest
            (fromIntegral $ structWordCount src)
            (fromIntegral $ structPtrCount src)
    copyStruct destStruct src
    pure destStruct

-- | Make a copy of the list, in the target message.
copyList :: RWCtx m s => M.Message ('Mut s) -> List ('Mut s) -> m (List ('Mut s))
{-# INLINABLE copyList #-}
{-# SPECIALIZE copyList :: M.Message ('Mut RealWorld) -> List ('Mut RealWorld) -> LimitT IO (List ('Mut RealWorld)) #-}
copyList dest src = case src of
    List0 src      -> List0 <$> allocList0 dest (length src)
    List1 src      -> List1 <$> copyNewListOf dest src allocList1
    List8 src      -> List8 <$> copyNewListOf dest src allocList8
    List16 src     -> List16 <$> copyNewListOf dest src allocList16
    List32 src     -> List32 <$> copyNewListOf dest src allocList32
    List64 src     -> List64 <$> copyNewListOf dest src allocList64
    ListPtr src    -> ListPtr <$> copyNewListOf dest src allocListPtr
    ListStruct src -> ListStruct <$> do
        destList <- allocCompositeList
            dest
            (fromIntegral $ structListWordCount src)
            (structListPtrCount src)
            (length src)
        copyListOf destList src
        pure destList

copyNewListOf
    :: (ListItem r, RWCtx m s)
    => M.Message ('Mut s)
    -> ListOf r ('Mut s)
    -> (M.Message ('Mut s) -> Int -> m (ListOf r ('Mut s)))
    -> m (ListOf r ('Mut s))
{-# INLINE copyNewListOf #-}
copyNewListOf destMsg src new = do
    dest <- new destMsg (length src)
    copyListOf dest src
    pure dest


-- | Make a copy of the list, in the target message.
copyListOf
    :: (ListItem r, RWCtx m s)
    => ListOf r ('Mut s) -> ListOf r ('Mut s) -> m ()
{-# INLINE copyListOf #-}
copyListOf dest src =
    forM_ [0..length src - 1] $ \i -> do
        value <- index i src
        setIndex value i dest

-- | @'copyStruct' dest src@ copies the source struct to the destination struct.
copyStruct :: RWCtx m s => Struct ('Mut s) -> Struct ('Mut s) -> m ()
{-# INLINABLE copyStruct #-}
{-# SPECIALIZE copyStruct :: Struct ('Mut RealWorld) -> Struct ('Mut RealWorld) -> LimitT IO () #-}
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
        copyListOf dest src
        -- Pad the remainder with zeros/default values:
        forM_ [length src..length dest - 1] $ \i ->
            setIndex pad i dest


-- | @index i list@ returns the ith element in @list@. Deducts 1 from the quota
index :: (ReadCtx m mut, ListItem r) => Int -> ListOf r mut -> m (Unwrapped (Untyped r mut))
{-# INLINE index #-}
{-# SPECIALIZE index :: ListItem r => Int -> ListOf r 'Const -> LimitT IO (Unwrapped (Untyped r 'Const)) #-}
{-# SPECIALIZE index :: ListItem r => Int -> ListOf r ('Mut RealWorld) -> LimitT IO (Unwrapped (Untyped r ('Mut RealWorld))) #-}
index i list
    | i < 0 || i >= length list =
        throwM E.BoundsError { E.index = i, E.maxIndex = length list - 1 }
    | otherwise = unsafeIndex i list

-- | Return a prefix of the list, of the given length.
{-# INLINABLE take #-}
take count list
    | length list < count =
        throwM E.BoundsError { E.index = count, E.maxIndex = length list - 1 }
    | otherwise = pure $ unsafeTake count list

-- | The data section of a struct, as a list of Word64
dataSection :: Struct mut -> ListOf ('Data 'Sz64) mut
{-# INLINE dataSection #-}
dataSection (StructAt ptr dataSz _) =
    ListOf $ NormalList ptr (fromIntegral dataSz)

-- | The pointer section of a struct, as a list of Ptr
ptrSection :: Struct mut -> ListOf ('Ptr 'Nothing) mut
{-# INLINE ptrSection #-}
ptrSection (StructAt ptr@M.WordPtr{pAddr=addr@WordAt{wordIndex}} dataSz ptrSz) =
    ListOf $ NormalList
        { nPtr = ptr { M.pAddr = addr { wordIndex = wordIndex + fromIntegral dataSz } }
        , nLen = fromIntegral ptrSz
        }

-- | Get the size (in words) of a struct's data section.
structWordCount :: Struct mut -> WordCount
structWordCount (StructAt _ptr dataSz _ptrSz) = fromIntegral dataSz

-- | Get the size (in bytes) of a struct's data section.
structByteCount :: Struct mut -> ByteCount
structByteCount = wordsToBytes . structWordCount

-- | Get the size of a struct's pointer section.
structPtrCount  :: Struct mut -> Word16
structPtrCount (StructAt _ptr _dataSz ptrSz) = ptrSz

-- | Get the size (in words) of the data sections in a struct list.
structListWordCount :: ListOf ('Ptr ('Just 'Struct)) mut -> WordCount
structListWordCount (ListOf (StructList s _)) = structWordCount s

-- | Get the size (in words) of the data sections in a struct list.
structListByteCount :: ListOf ('Ptr ('Just 'Struct)) mut -> ByteCount
structListByteCount (ListOf (StructList s _)) = structByteCount s

-- | Get the size of the pointer sections in a struct list.
structListPtrCount  :: ListOf ('Ptr ('Just 'Struct)) mut -> Word16
structListPtrCount  (ListOf (StructList s _)) = structPtrCount s

-- | @'getData' i struct@ gets the @i@th word from the struct's data section,
-- returning 0 if it is absent.
getData :: ReadCtx m msg => Int -> Struct msg -> m Word64
{-# INLINE getData #-}
getData i struct
    | fromIntegral (structWordCount struct) <= i = pure 0
    | otherwise = index i (dataSection struct)

-- | @'getPtr' i struct@ gets the @i@th word from the struct's pointer section,
-- returning Nothing if it is absent.
getPtr :: ReadCtx m msg => Int -> Struct msg -> m (Maybe (Ptr msg))
{-# INLINE getPtr #-}
getPtr i struct
    | fromIntegral (structPtrCount struct) <= i = do
        invoice 1
        pure Nothing
    | otherwise = do
        ptr <- index i (ptrSection struct)
        checkPtr ptr
        invoicePtr ptr
        pure ptr

checkPtr :: ReadCtx m mut => Maybe (Ptr mut) -> m ()
{-# INLINABLE checkPtr #-}
checkPtr Nothing              = pure ()
checkPtr (Just (PtrCap c))    = checkCap c
checkPtr (Just (PtrList l))   = checkList l
checkPtr (Just (PtrStruct s)) = checkStruct s

checkCap :: ReadCtx m mut => Cap mut -> m ()
{-# INLINABLE checkCap #-}
checkCap (CapAt _ _ ) = pure ()
    -- No need to do anything here; an out of bounds index is just treated
    -- as null.

checkList :: ReadCtx m mut => List mut -> m ()
{-# INLINABLE checkList #-}
checkList (List0 l)      = checkListOf @('Data 'Sz0) l
checkList (List1 l)      = checkListOf @('Data 'Sz1) l
checkList (List8 l)      = checkListOf @('Data 'Sz8) l
checkList (List16 l)     = checkListOf @('Data 'Sz16) l
checkList (List32 l)     = checkListOf @('Data 'Sz32) l
checkList (List64 l)     = checkListOf @('Data 'Sz64) l
checkList (ListPtr l)    = checkListOf @('Ptr 'Nothing) l
checkList (ListStruct l) = checkListOf @('Ptr ('Just 'Struct)) l

checkNormalList :: ReadCtx m mut => NormalList mut -> BitCount -> m ()
{-# INLINABLE checkNormalList #-}
checkNormalList NormalList{nPtr, nLen} eltSize =
    let nBits = fromIntegral nLen * eltSize
        nWords = bytesToWordsCeil $ bitsToBytesCeil nBits
    in
    checkPtrOffset nPtr nWords

checkStruct :: ReadCtx m mut => Struct mut -> m ()
{-# INLINABLE checkStruct #-}
checkStruct s@(StructAt ptr _ _) =
    checkPtrOffset ptr (structSize s)

checkPtrOffset :: ReadCtx m mut => M.WordPtr mut -> WordCount -> m ()
{-# INLINABLE checkPtrOffset #-}
checkPtrOffset M.WordPtr{pSegment, pAddr=WordAt{wordIndex}} size = do
    segWords <- M.numWords pSegment
    let maxIndex = fromIntegral segWords - 1
    unless (wordIndex >= 0) $
        throwM E.BoundsError { index = fromIntegral wordIndex, maxIndex }
    unless (wordIndex + size <= segWords) $
        throwM E.BoundsError
            { index = fromIntegral (wordIndex + size) - 1
            , maxIndex
            }

structSize :: Struct mut -> WordCount
structSize s = structWordCount s + fromIntegral (structPtrCount s)

-- | Invoice the traversal limit for all data reachable via the pointer
-- directly, i.e. without following further pointers.
--
-- The minimum possible cost is 1, and for lists will always be proportional
-- to the length of the list, even if the size of the elements is zero.
invoicePtr :: MonadLimit m => Maybe (Ptr mut) -> m ()
{-# INLINABLE invoicePtr #-}
{-# SPECIALIZE invoicePtr :: Maybe (Ptr ('Mut RealWorld)) -> LimitT IO () #-}
invoicePtr p = invoice $! ptrInvoiceSize p

ptrInvoiceSize :: Maybe (Ptr mut) -> WordCount
{-# INLINABLE ptrInvoiceSize #-}
ptrInvoiceSize = \case
    Nothing            -> 1
    Just (PtrCap _)    -> 1
    Just (PtrStruct s) -> structInvoiceSize s
    Just (PtrList l)   -> listInvoiceSize l
listInvoiceSize :: List mut -> WordCount
{-# INLINABLE listInvoiceSize #-}
listInvoiceSize l = max 1 $! case l of
    List0 l   -> fromIntegral $! length l
    List1 l   -> fromIntegral $! length l `div` 64
    List8 l   -> fromIntegral $! length l `div`  8
    List16 l  -> fromIntegral $! length l `div`  4
    List32 l  -> fromIntegral $! length l `div`  2
    List64 l  -> fromIntegral $! length l
    ListPtr l -> fromIntegral $! length l
    ListStruct (ListOf (StructList s len)) ->
        structInvoiceSize s * fromIntegral len
structInvoiceSize :: Struct mut -> WordCount
{-# INLINABLE structInvoiceSize #-}
structInvoiceSize (StructAt _ dataSz ptrSz) =
    max 1 (fromIntegral dataSz + fromIntegral ptrSz)

-- | @'setData' value i struct@ sets the @i@th word in the struct's data section
-- to @value@.
{-# INLINE setData #-}
setData :: (ReadCtx m ('Mut s), M.WriteCtx m s)
    => Word64 -> Int -> Struct ('Mut s) -> m ()
setData value i = setIndex value i . dataSection

-- | @'setData' value i struct@ sets the @i@th pointer in the struct's pointer
-- section to @value@.
setPtr :: (ReadCtx m ('Mut s), M.WriteCtx m s) => Maybe (Ptr ('Mut s)) -> Int -> Struct ('Mut s) -> m ()
{-# INLINE setPtr #-}
setPtr value i = setIndex value i . ptrSection

-- | 'rawBytes' returns the raw bytes corresponding to the list.
rawBytes :: ReadCtx m 'Const => ListOf ('Data 'Sz8) 'Const -> m BS.ByteString
{-# INLINABLE rawBytes #-}
-- TODO: we can get away with a more lax context than ReadCtx, maybe even make
-- this non-monadic.
rawBytes (ListOf (NormalList M.WordPtr{pSegment, pAddr=WordAt{wordIndex}} len)) = do
    let bytes = M.toByteString pSegment
    let ByteCount byteOffset = wordsToBytes wordIndex
    pure $ BS.take len $ BS.drop byteOffset bytes


-- | Returns the root pointer of a message.
rootPtr :: ReadCtx m mut => M.Message mut -> m (Struct mut)
{-# INLINABLE rootPtr #-}
rootPtr msg = do
    seg <- M.getSegment msg 0
    root <- get M.WordPtr
        { pMessage = msg
        , pSegment = seg
        , pAddr = WordAt 0 0
        }
    checkPtr root
    invoicePtr root
    case root of
        Just (PtrStruct struct) -> pure struct
        Nothing -> messageDefault @Struct msg
        _ -> throwM $ E.SchemaViolationError
                "Unexpected root type; expected struct."


-- | Make the given struct the root object of its message.
setRoot :: M.WriteCtx m s => Struct ('Mut s) -> m ()
{-# INLINABLE setRoot #-}
setRoot (StructAt M.WordPtr{pMessage, pAddr=addr} dataSz ptrSz) = do
    pSegment <- M.getSegment pMessage 0
    let rootPtr = M.WordPtr{pMessage, pSegment, pAddr = WordAt 0 0}
    setPointerTo rootPtr addr (P.StructPtr 0 dataSz ptrSz)


-- | An instace of @'Allocate'@ specifies how to allocate a value with a given representation.
-- This only makes sense for pointers of course, so it is defined on PtrRepr. Of the well-kinded
-- types, only @'List 'Nothing@ is missing an instance.
class Allocate (r :: PtrRepr) where
    -- | Extra information needed to allocate a value:
    --
    -- * For structs, the sizes of the sections.
    -- * For capabilities, the client to attach to the messages.
    -- * For lists, the length, and for composite lists, the struct sizes as well.
    type AllocHint r

    -- | Allocate a value of the given type.
    alloc :: RWCtx m s => M.Message ('Mut s) -> AllocHint r -> m (Unwrapped (UntypedSomePtr r ('Mut s)))

instance Allocate 'Struct where
    type AllocHint 'Struct = (Word16, Word16)
    alloc msg = uncurry (allocStruct msg)
instance Allocate 'Cap where
    type AllocHint 'Cap = M.Client
    alloc = appendCap
instance Allocate ('List ('Just 'ListComposite)) where
    type AllocHint ('List ('Just 'ListComposite)) = (Int, AllocHint 'Struct)
    alloc msg (len, (nWords, nPtrs)) = allocCompositeList msg nWords nPtrs len
instance AllocateNormalList r => Allocate ('List ('Just ('ListNormal r))) where
    type AllocHint ('List ('Just ('ListNormal r))) = Int
    alloc = allocNormalList @r

-- | Like 'Allocate', but specialized to normal (non-composite) lists.
--
-- Instead of an 'AllocHint' type family, the hint is always an 'Int',
-- which is the number of elements.
class AllocateNormalList (r :: NormalListRepr) where
    allocNormalList
        :: RWCtx m s
        => M.Message ('Mut s) -> Int -> m (UntypedSomeList ('ListNormal r) ('Mut s))

instance AllocateNormalList ('NormalListData 'Sz0) where allocNormalList = allocList0
instance AllocateNormalList ('NormalListData 'Sz1) where allocNormalList = allocList1
instance AllocateNormalList ('NormalListData 'Sz8) where allocNormalList = allocList8
instance AllocateNormalList ('NormalListData 'Sz16) where allocNormalList = allocList16
instance AllocateNormalList ('NormalListData 'Sz32) where allocNormalList = allocList32
instance AllocateNormalList ('NormalListData 'Sz64) where allocNormalList = allocList64
instance AllocateNormalList 'NormalListPtr where allocNormalList = allocListPtr

-- | Allocate a struct in the message.
allocStruct :: M.WriteCtx m s => M.Message ('Mut s) -> Word16 -> Word16 -> m (Struct ('Mut s))
{-# INLINABLE allocStruct #-}
allocStruct msg dataSz ptrSz = do
    let totalSz = fromIntegral dataSz + fromIntegral ptrSz
    ptr <- M.alloc msg totalSz
    pure $ StructAt ptr dataSz ptrSz

-- | Allocate a composite list.
allocCompositeList
    :: M.WriteCtx m s
    => M.Message ('Mut s) -- ^ The message to allocate in.
    -> Word16     -- ^ The size of the data section
    -> Word16     -- ^ The size of the pointer section
    -> Int        -- ^ The length of the list in elements.
    -> m (ListOf ('Ptr ('Just 'Struct)) ('Mut s))
{-# INLINABLE allocCompositeList #-}
allocCompositeList msg dataSz ptrSz len = do
    let eltSize = fromIntegral dataSz + fromIntegral ptrSz
    ptr@M.WordPtr{pSegment, pAddr=addr@WordAt{wordIndex}}
        <- M.alloc msg (WordCount $ len * eltSize + 1) -- + 1 for the tag word.
    M.write pSegment wordIndex $ P.serializePtr' $ P.StructPtr (fromIntegral len) dataSz ptrSz
    let firstStruct = StructAt
            ptr { M.pAddr = addr { wordIndex = wordIndex + 1 } }
            dataSz
            ptrSz
    pure $ ListOf $ StructList firstStruct len

-- | Allocate a list of capnproto @Void@ values.
allocList0   :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (ListOf ('Data 'Sz0) ('Mut s))
{-# INLINABLE allocList0 #-}

-- | Allocate a list of booleans
allocList1   :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (ListOf ('Data 'Sz1) ('Mut s))
{-# INLINABLE allocList1 #-}

-- | Allocate a list of 8-bit values.
allocList8   :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (ListOf ('Data 'Sz8) ('Mut s))
{-# INLINABLE allocList8 #-}

-- | Allocate a list of 16-bit values.
allocList16  :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (ListOf ('Data 'Sz16) ('Mut s))
{-# INLINABLE allocList16 #-}

-- | Allocate a list of 32-bit values.
allocList32  :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (ListOf ('Data 'Sz32) ('Mut s))
{-# INLINABLE allocList32 #-}

-- | Allocate a list of 64-bit words.
allocList64  :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (ListOf ('Data 'Sz64) ('Mut s))
{-# INLINABLE allocList64 #-}

-- | Allocate a list of pointers.
allocListPtr :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (ListOf ('Ptr 'Nothing) ('Mut s))
{-# INLINABLE allocListPtr #-}

allocList0   msg len = ListOf <$> allocNormalList' 0  msg len
allocList1   msg len = ListOf <$> allocNormalList' 1  msg len
allocList8   msg len = ListOf <$> allocNormalList' 8  msg len
allocList16  msg len = ListOf <$> allocNormalList' 16 msg len
allocList32  msg len = ListOf <$> allocNormalList' 32 msg len
allocList64  msg len = ListOf <$> allocNormalList' 64 msg len
allocListPtr msg len = ListOf <$> allocNormalList' 64 msg len

-- | Allocate a NormalList
allocNormalList'
    :: M.WriteCtx m s
    => Int                  -- ^ The number bits per element
    -> M.Message ('Mut s) -- ^ The message to allocate in
    -> Int                  -- ^ The number of elements in the list.
    -> m (NormalList ('Mut s))
{-# INLINABLE allocNormalList' #-}
allocNormalList' bitsPerElt msg len = do
    -- round 'len' up to the nearest word boundary.
    let totalBits = BitCount (len * bitsPerElt)
        totalWords = bytesToWordsCeil $ bitsToBytesCeil totalBits
    ptr <- M.alloc msg totalWords
    pure NormalList { nPtr = ptr, nLen = len }

appendCap :: M.WriteCtx m s => M.Message ('Mut s) -> M.Client -> m (Cap ('Mut s))
{-# INLINABLE appendCap #-}
appendCap msg client = do
    i <- M.appendCap msg client
    pure $ CapAt msg (fromIntegral i)

instance MaybeMutable (ListRepOf r) => MaybeMutable (ListOf r) where
    thaw         (ListOf l) = ListOf <$> thaw l
    freeze       (ListOf l) = ListOf <$> freeze l
    unsafeThaw   (ListOf l) = ListOf <$> unsafeThaw l
    unsafeFreeze (ListOf l) = ListOf <$> unsafeFreeze l


-------------------------------------------------------------------------------
-- Helpers generated MaybeMutable instances
-------------------------------------------------------------------------------

-- trivial wrapaper around CatchT, so we can add a PrimMonad instance.
newtype CatchTWrap m a = CatchTWrap { runCatchTWrap :: CatchT m a }
    deriving(Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch)

instance PrimMonad m => PrimMonad (CatchTWrap m) where
    type PrimState (CatchTWrap m) = PrimState m
    primitive = lift . primitive

-- | @runCatchImpure m@ runs @m@, and if it throws, raises the
-- exception with 'impureThrow'.
runCatchImpure :: Monad m => CatchTWrap m a -> m a
{-# INLINABLE runCatchImpure #-}
runCatchImpure m = do
    res <- runCatchT $ runCatchTWrap m
    pure $ case res of
        Left e  -> impureThrow e
        Right v -> v

-------------------------------------------------------------------------------
-- Generated MaybeMutable instances
-------------------------------------------------------------------------------

do
    let mkWrappedInstance name =
            let f = pure $ TH.ConT name in
            [d|instance MaybeMutable $f where
                thaw         = runCatchImpure . tMsg thaw
                freeze       = runCatchImpure . tMsg freeze
                unsafeThaw   = runCatchImpure . tMsg unsafeThaw
                unsafeFreeze = runCatchImpure . tMsg unsafeFreeze
            |]
    concat <$> traverse mkWrappedInstance
        [ ''Ptr
        , ''List
        , ''NormalList
        , ''Struct
        ]

do
    let mkIsListPtrRepr (r, listC, str) =
            [d| instance IsListPtrRepr $r where
                    rToList = $(pure $ TH.ConE listC)
                    rFromList $(pure $ TH.ConP listC [TH.VarP (TH.mkName "l")]) = pure l
                    rFromList _ = expected $(pure $ TH.LitE $ TH.StringL $ "pointer to " ++ str)
                    rFromListMsg = messageDefault @(Untyped ('Ptr ('Just ('List ('Just $r)))))
            |]
    concat <$> traverse mkIsListPtrRepr
        [ ( [t| 'ListNormal ('NormalListData 'Sz0) |]
          , 'List0
          , "List(Void)"
          )
        , ( [t| 'ListNormal ('NormalListData 'Sz1) |]
          , 'List1
          , "List(Bool)"
          )
        , ( [t| 'ListNormal ('NormalListData 'Sz8) |]
          , 'List8
          , "List(UInt8)"
          )
        , ( [t| 'ListNormal ('NormalListData 'Sz16) |]
          , 'List16
          , "List(UInt16)"
          )
        , ( [t| 'ListNormal ('NormalListData 'Sz32) |]
          , 'List32
          , "List(UInt32)"
          )
        , ( [t| 'ListNormal ('NormalListData 'Sz64) |]
          , 'List64
          , "List(UInt64)"
          )
        , ( [t| 'ListNormal 'NormalListPtr |]
          , 'ListPtr
          , "List(AnyPointer)"
          )
        , ( [t| 'ListComposite |]
          , 'ListStruct
          , "composite list"
          )
        ]
