{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-|
Module: Capnp.Untyped
Description: Utilities for reading capnproto messages with no schema.

The types and functions in this module know about things like structs and
lists, but are not schema aware.

Each of the data types exported by this module is parametrized over the
mutability of the message it contains (see "Capnp.Message").
-}
module Capnp.Untyped
    ( Ptr(..), List(..), Struct, ListOf, Cap
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
import Capnp.Message        (Mutability(..))
import Capnp.Pointer        (ElementSize(..))
import Capnp.TraversalLimit (LimitT, MonadLimit(invoice))
import Data.Mutable         (Thaw(..))

import qualified Capnp.Errors     as E
import qualified Capnp.Message    as M
import qualified Capnp.Mutability as Mut
import qualified Capnp.Pointer    as P

-- | Type (constraint) synonym for the constraints needed for most read
-- operations.
type ReadCtx m mut = (M.MonadReadMessage mut m, MonadThrow m, MonadLimit m)

-- | Synonym for ReadCtx + WriteCtx
type RWCtx m s = (ReadCtx m ('Mut s), M.WriteCtx m s)

-- | A an absolute pointer to a value (of arbitrary type) in a message.
-- Note that there is no variant for far pointers, which don't make sense
-- with absolute addressing.
data Ptr mut
    = PtrCap (Cap mut)
    | PtrList (List mut)
    | PtrStruct (Struct mut)

-- | A list of values (of arbitrary type) in a message.
data List mut
    = List0 (ListOf mut ())
    | List1 (ListOf mut Bool)
    | List8 (ListOf mut Word8)
    | List16 (ListOf mut Word16)
    | List32 (ListOf mut Word32)
    | List64 (ListOf mut Word64)
    | ListPtr (ListOf mut (Maybe (Ptr mut)))
    | ListStruct (ListOf mut (Struct mut))

-- | A "normal" (non-composite) list.
data NormalList mut = NormalList
    { nPtr :: !(M.WordPtr mut)
    , nLen :: !Int
    }

-- | A list of values of type 'a' in a message.
data ListOf mut a where
    ListOfStruct
        :: Struct mut -- First element. data/ptr sizes are the same for
                      -- all elements.
        -> !Int       -- Number of elements
        -> ListOf mut (Struct mut)
    ListOfVoid   :: !(NormalList mut) -> ListOf mut ()
    ListOfBool   :: !(NormalList mut) -> ListOf mut Bool
    ListOfWord8  :: !(NormalList mut) -> ListOf mut Word8
    ListOfWord16 :: !(NormalList mut) -> ListOf mut Word16
    ListOfWord32 :: !(NormalList mut) -> ListOf mut Word32
    ListOfWord64 :: !(NormalList mut) -> ListOf mut Word64
    ListOfPtr    :: !(NormalList mut) -> ListOf mut (Maybe (Ptr mut))

-- | A Capability in a message.
data Cap mut = Cap (M.Message mut) !Word32

-- | A struct value in a message.
data Struct mut
    = Struct
        !(M.WordPtr mut) -- Start of struct
        !Word16 -- Data section size.
        !Word16 -- Pointer section size.

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
    tMsg f (Cap msg n) = Cap <$> f msg <*> pure n

instance TraverseMsg Struct where
    tMsg f (Struct ptr dataSz ptrSz) = Struct
        <$> tMsg f ptr
        <*> pure dataSz
        <*> pure ptrSz

instance TraverseMsg List where
    tMsg f = \case
        List0      l -> List0      . unflip  <$> tMsg f (FlipList  l)
        List1      l -> List1      . unflip  <$> tMsg f (FlipList  l)
        List8      l -> List8      . unflip  <$> tMsg f (FlipList  l)
        List16     l -> List16     . unflip  <$> tMsg f (FlipList  l)
        List32     l -> List32     . unflip  <$> tMsg f (FlipList  l)
        List64     l -> List64     . unflip  <$> tMsg f (FlipList  l)
        ListPtr    l -> ListPtr    . unflipP <$> tMsg f (FlipListP l)
        ListStruct l -> ListStruct . unflipS <$> tMsg f (FlipListS l)

instance TraverseMsg NormalList where
    tMsg f NormalList{..} = do
        ptr <- tMsg f nPtr
        pure NormalList { nPtr = ptr, .. }

-------------------------------------------------------------------------------
-- newtype wrappers for the purpose of implementing 'TraverseMsg'; these adjust
-- the shape of 'ListOf' so that we can define an instance. We need a couple
-- different wrappers depending on the shape of the element type.
-------------------------------------------------------------------------------

-- 'FlipList' wraps a @ListOf msg a@ where 'a' is of kind @*@.
newtype FlipList  a msg = FlipList  { unflip  :: ListOf msg a                 }

-- 'FlipListS' wraps a @ListOf msg (Struct msg)@. We can't use 'FlipList' for
-- our instances, because we need both instances of the 'msg' parameter to stay
-- equal.
newtype FlipListS   msg = FlipListS { unflipS :: ListOf msg (Struct msg)      }

-- 'FlipListP' wraps a @ListOf msg (Maybe (Ptr msg))@. Pointers can't use
-- 'FlipList' for the same reason as structs.
newtype FlipListP   msg = FlipListP { unflipP :: ListOf msg (Maybe (Ptr msg)) }

-------------------------------------------------------------------------------
-- 'TraverseMsg' instances for 'FlipList'
-------------------------------------------------------------------------------

instance TraverseMsg (FlipList ()) where
    tMsg f (FlipList (ListOfVoid   nlist)) = FlipList . ListOfVoid <$> tMsg f nlist

instance TraverseMsg (FlipList Bool) where
    tMsg f (FlipList (ListOfBool   nlist)) = FlipList . ListOfBool   <$> tMsg f nlist

instance TraverseMsg (FlipList Word8) where
    tMsg f (FlipList (ListOfWord8  nlist)) = FlipList . ListOfWord8  <$> tMsg f nlist

instance TraverseMsg (FlipList Word16) where
    tMsg f (FlipList (ListOfWord16 nlist)) = FlipList . ListOfWord16 <$> tMsg f nlist

instance TraverseMsg (FlipList Word32) where
    tMsg f (FlipList (ListOfWord32 nlist)) = FlipList . ListOfWord32 <$> tMsg f nlist

instance TraverseMsg (FlipList Word64) where
    tMsg f (FlipList (ListOfWord64 nlist)) = FlipList . ListOfWord64 <$> tMsg f nlist

-------------------------------------------------------------------------------
-- 'TraverseMsg' instances for struct and pointer lists.
-------------------------------------------------------------------------------

instance TraverseMsg FlipListP where
    tMsg f (FlipListP (ListOfPtr nlist))   = FlipListP . ListOfPtr   <$> tMsg f nlist

instance TraverseMsg FlipListS where
    tMsg f (FlipListS (ListOfStruct tag size)) =
        FlipListS <$> (ListOfStruct <$> tMsg f tag <*> pure size)

-- helpers for applying tMsg to a @ListOf@.
tFlip  :: (TraverseMsg (FlipList a), TraverseMsgCtx m mutA mutB)
    => (M.Message mutA -> m (M.Message mutB)) -> ListOf mutA a -> m (ListOf mutB a)
tFlipS :: TraverseMsgCtx m mutA mutB => (M.Message mutA -> m (M.Message mutB)) -> ListOf mutA (Struct mutA) -> m (ListOf mutB (Struct mutB ))
tFlipP :: TraverseMsgCtx m mutA mutB => (M.Message mutA -> m (M.Message mutB)) -> ListOf mutA (Maybe (Ptr mutA)) -> m (ListOf mutB (Maybe (Ptr mutB)))
tFlip  f list  = unflip  <$> tMsg f (FlipList  list)
tFlipS f list  = unflipS <$> tMsg f (FlipListS list)
tFlipP f list  = unflipP <$> tMsg f (FlipListP list)

-------------------------------------------------------------------------------
-- Boilerplate 'Thaw' instances.
--
-- These all just call the underlying methods on the message, using 'TraverseMsg'.
-------------------------------------------------------------------------------

instance Thaw a => Thaw (Maybe a) where
    type Mutable s (Maybe a) = Maybe (Mutable s a)

    thaw         = traverse thaw
    freeze       = traverse freeze
    unsafeThaw   = traverse unsafeThaw
    unsafeFreeze = traverse unsafeFreeze

do
    let mkWrappedInstance name =
            let f = pure $ TH.ConT name in
            [d|instance Thaw ($f 'Const) where
                type Mutable s ($f 'Const) = $f ('Mut s)

                thaw         = runCatchImpure . tMsg thaw
                freeze       = runCatchImpure . tMsg freeze
                unsafeThaw   = runCatchImpure . tMsg unsafeThaw
                unsafeFreeze = runCatchImpure . tMsg unsafeFreeze

               instance Mut.MaybeMutable $f where
                thaw         = runCatchImpure . tMsg thaw
                freeze       = runCatchImpure . tMsg freeze
                unsafeThaw   = runCatchImpure . tMsg unsafeThaw
                unsafeFreeze = runCatchImpure . tMsg unsafeFreeze
            |]
        mkListOfInstance t =
            [d|instance Thaw (ListOf 'Const $t) where
                type Mutable s (ListOf 'Const $t) = ListOf ('Mut s) $t

                thaw         = runCatchImpure . tFlip thaw
                freeze       = runCatchImpure . tFlip freeze
                unsafeThaw   = runCatchImpure . tFlip unsafeThaw
                unsafeFreeze = runCatchImpure . tFlip unsafeFreeze
            |]
    xs <- traverse mkWrappedInstance
        [ ''Ptr
        , ''List
        , ''NormalList
        , ''Struct
        ]
    ys <- traverse mkListOfInstance
        [ [t|()|]
        , [t|Bool|]
        , [t|Word8|]
        , [t|Word16|]
        , [t|Word32|]
        , [t|Word64|]
        ]
    pure $ concat $ xs ++ ys

instance Thaw (ListOf 'Const (Struct 'Const)) where
    type Mutable s (ListOf 'Const (Struct 'Const)) =
        ListOf ('Mut s) (Struct ('Mut s))

    thaw         = runCatchImpure . tFlipS thaw
    freeze       = runCatchImpure . tFlipS freeze
    unsafeThaw   = runCatchImpure . tFlipS unsafeThaw
    unsafeFreeze = runCatchImpure . tFlipS unsafeFreeze

instance Thaw (ListOf 'Const (Maybe (Ptr 'Const))) where
    type Mutable s (ListOf 'Const (Maybe (Ptr 'Const))) =
        ListOf ('Mut s) (Maybe (Ptr ('Mut s)))

    thaw         = runCatchImpure . tFlipP thaw
    freeze       = runCatchImpure . tFlipP freeze
    unsafeThaw   = runCatchImpure . tFlipP unsafeThaw
    unsafeFreeze = runCatchImpure . tFlipP unsafeFreeze

-------------------------------------------------------------------------------
-- Helpers for the above boilerplate Thaw instances
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
runCatchImpure m = do
    res <- runCatchT $ runCatchTWrap m
    pure $ case res of
        Left e  -> impureThrow e
        Right v -> v

-------------------------------------------------------------------------------

-- | Types @a@ whose storage is owned by a message..
class HasMessage a mut | a -> mut where
    -- | Get the message in which the @a@ is stored.
    message :: a -> M.Message mut

-- | Types which have a "default" value, but require a message
-- to construct it.
--
-- The default is usually conceptually zero-size. This is mostly useful
-- for generated code, so that it can use standard decoding techniques
-- on default values.
class HasMessage a mut => MessageDefault a mut where
    messageDefault :: ReadCtx m mut => M.Message mut -> m a

instance HasMessage (M.WordPtr mut) mut where
    message M.WordPtr{pMessage} = pMessage

instance HasMessage (Ptr mut) mut where
    message (PtrCap cap)       = message cap
    message (PtrList list)     = message list
    message (PtrStruct struct) = message struct

instance HasMessage (Cap mut) mut where
    message (Cap msg _) = msg

instance HasMessage (Struct mut) mut where
    message (Struct ptr _ _) = message ptr

instance MessageDefault (Struct mut) mut where
    messageDefault msg = do
        pSegment <- M.getSegment msg 0
        pure $ Struct M.WordPtr{pMessage = msg, pSegment, pAddr = WordAt 0 0} 0 0

instance HasMessage (List mut) mut where
    message (List0 list)      = message list
    message (List1 list)      = message list
    message (List8 list)      = message list
    message (List16 list)     = message list
    message (List32 list)     = message list
    message (List64 list)     = message list
    message (ListPtr list)    = message list
    message (ListStruct list) = message list

instance HasMessage (ListOf mut a) mut where
    message (ListOfStruct tag _) = message tag
    message (ListOfVoid list)    = message list
    message (ListOfBool list)    = message list
    message (ListOfWord8 list)   = message list
    message (ListOfWord16 list)  = message list
    message (ListOfWord32 list)  = message list
    message (ListOfWord64 list)  = message list
    message (ListOfPtr list)     = message list

instance MessageDefault (ListOf mut ()) mut where
    messageDefault msg = ListOfVoid <$> messageDefault msg
instance MessageDefault (ListOf mut (Struct mut)) mut where
    messageDefault msg = flip ListOfStruct 0 <$> messageDefault msg
instance MessageDefault (ListOf mut Bool) mut where
    messageDefault msg = ListOfBool <$> messageDefault msg
instance MessageDefault (ListOf mut Word8) mut where
    messageDefault msg = ListOfWord8 <$> messageDefault msg
instance MessageDefault (ListOf mut Word16) mut where
    messageDefault msg = ListOfWord16 <$> messageDefault msg
instance MessageDefault (ListOf mut Word32) mut where
    messageDefault msg = ListOfWord32 <$> messageDefault msg
instance MessageDefault (ListOf mut Word64) mut where
    messageDefault msg = ListOfWord64 <$> messageDefault msg
instance MessageDefault (ListOf mut (Maybe (Ptr mut))) mut where
    messageDefault msg = ListOfPtr <$> messageDefault msg

instance HasMessage (NormalList mut) mut where
    message = M.pMessage . nPtr

instance MessageDefault (NormalList mut) mut where
    messageDefault msg = do
        pSegment <- M.getSegment msg 0
        pure NormalList
            { nPtr = M.WordPtr { pMessage = msg, pSegment, pAddr = WordAt 0 0 }
            , nLen = 0
            }

-- | Extract a client (indepedent of the messsage) from the capability.
getClient :: ReadCtx m mut => Cap mut -> m M.Client
getClient (Cap msg idx) = M.getCap msg (fromIntegral idx)

-- | @get ptr@ returns the Ptr stored at @ptr@.
-- Deducts 1 from the quota for each word read (which may be multiple in the
-- case of far pointers).
get :: ReadCtx m mut => M.WordPtr mut -> m (Maybe (Ptr mut))
{-# SPECIALIZE get :: M.WordPtr ('Mut RealWorld) -> LimitT IO (Maybe (Ptr ('Mut RealWorld))) #-}
get ptr@M.WordPtr{pMessage, pAddr} = do
    word <- getWord ptr
    case P.parsePtr word of
        Nothing -> return Nothing
        Just p -> case p of
            P.CapPtr cap -> return $ Just $ PtrCap (Cap pMessage cap)
            P.StructPtr off dataSz ptrSz -> return $ Just $ PtrStruct $
                Struct ptr { M.pAddr = resolveOffset pAddr off } dataSz ptrSz
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
                                            Struct finalPtr dataSz ptrSz
                                    Just (P.ListPtr 0 eltSpec) ->
                                        Just <$> getList finalPtr eltSpec
                                    -- TODO: I'm not sure whether far pointers to caps are
                                    -- legal; it's clear how they would work, but I don't
                                    -- see a use, and the spec is unclear. Should check
                                    -- how the reference implementation does this, copy
                                    -- that, and submit a patch to the spec.
                                    Just (P.CapPtr cap) ->
                                        return $ Just $ PtrCap (Cap pMessage cap)
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
                Sz0   -> List0  (ListOfVoid    nlist)
                Sz1   -> List1  (ListOfBool    nlist)
                Sz8   -> List8  (ListOfWord8   nlist)
                Sz16  -> List16 (ListOfWord16  nlist)
                Sz32  -> List32 (ListOfWord32  nlist)
                Sz64  -> List64 (ListOfWord64  nlist)
                SzPtr -> ListPtr (ListOfPtr nlist)
              where
                nlist = NormalList ptr (fromIntegral len)
            P.EltComposite _ -> do
                tagWord <- getWord ptr
                case P.parsePtr' tagWord of
                    P.StructPtr numElts dataSz ptrSz ->
                        pure $ ListStruct $ ListOfStruct
                            (Struct ptr { M.pAddr = addr { wordIndex = wordIndex + 1 } }
                                    dataSz
                                    ptrSz)
                            (fromIntegral numElts)
                    tag -> throwM $ E.InvalidDataError $
                        "Composite list tag was not a struct-" ++
                        "formatted word: " ++ show tag

-- | Return the EltSpec needed for a pointer to the given list.
listEltSpec :: List msg -> P.EltSpec
listEltSpec (ListStruct list@(ListOfStruct (Struct _ dataSz ptrSz) _)) =
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
listAddr (ListStruct (ListOfStruct (Struct M.WordPtr{pAddr} _ _) _)) =
    -- pAddr is the address of the first element of the list, but
    -- composite lists start with a tag word:
    pAddr { wordIndex = wordIndex pAddr - 1 }
listAddr (List0 (ListOfVoid NormalList{nPtr=M.WordPtr{pAddr}})) = pAddr
listAddr (List1 (ListOfBool NormalList{nPtr=M.WordPtr{pAddr}})) = pAddr
listAddr (List8 (ListOfWord8 NormalList{nPtr=M.WordPtr{pAddr}})) = pAddr
listAddr (List16 (ListOfWord16 NormalList{nPtr=M.WordPtr{pAddr}})) = pAddr
listAddr (List32 (ListOfWord32 NormalList{nPtr=M.WordPtr{pAddr}})) = pAddr
listAddr (List64 (ListOfWord64 NormalList{nPtr=M.WordPtr{pAddr}})) = pAddr
listAddr (ListPtr (ListOfPtr NormalList{nPtr=M.WordPtr{pAddr}})) = pAddr

-- | Return the address of the pointer's target. It is illegal to call this on
-- a pointer which targets a capability.
ptrAddr :: Ptr msg -> WordAddr
ptrAddr (PtrCap _) = error "ptrAddr called on a capability pointer."
ptrAddr (PtrStruct (Struct M.WordPtr{pAddr}_ _)) = pAddr
ptrAddr (PtrList list) = listAddr list

-- | @'setIndex value i list@ Set the @i@th element of @list@ to @value@.
setIndex :: RWCtx m s => a -> Int -> ListOf ('Mut s) a -> m ()
{-# SPECIALIZE setIndex :: a -> Int -> ListOf ('Mut RealWorld) a -> LimitT IO () #-}
setIndex _ i list | i < 0 || length list <= i =
    throwM E.BoundsError { E.index = i, E.maxIndex = length list }
setIndex value i list = case list of
    ListOfVoid _       -> pure ()
    ListOfBool nlist   -> setNIndex nlist 64 (Word1 value)
    ListOfWord8 nlist  -> setNIndex nlist 8 value
    ListOfWord16 nlist -> setNIndex nlist 4 value
    ListOfWord32 nlist -> setNIndex nlist 2 value
    ListOfWord64 nlist -> setNIndex nlist 1 value
    ListOfPtr nlist -> case value of
        Just p | message p /= message list -> do
            newPtr <- copyPtr (message list) value
            setIndex newPtr i list
        Nothing                -> setNIndex nlist 1 (P.serializePtr Nothing)
        Just (PtrCap (Cap _ cap))    -> setNIndex nlist 1 (P.serializePtr (Just (P.CapPtr cap)))
        Just p@(PtrList ptrList)     ->
            setPtrIndex nlist p $ P.ListPtr 0 (listEltSpec ptrList)
        Just p@(PtrStruct (Struct _ dataSz ptrSz)) ->
            setPtrIndex nlist p $ P.StructPtr 0 dataSz ptrSz
    list@(ListOfStruct _ _) -> do
        dest <- index i list
        copyStruct dest value
  where
    setNIndex :: (RWCtx m s, Bounded a, Integral a) => NormalList ('Mut s) -> Int -> a -> m ()
    setNIndex NormalList{nPtr=M.WordPtr{pSegment, pAddr=WordAt{wordIndex}}} eltsPerWord value = do
        let eltWordIndex = wordIndex + WordCount (i `div` eltsPerWord)
        word <- M.read pSegment eltWordIndex
        let shift = (i `mod` eltsPerWord) * (64 `div` eltsPerWord)
        M.write pSegment eltWordIndex $ replaceBits value word shift
    setPtrIndex :: RWCtx m s => NormalList ('Mut s) -> Ptr ('Mut s) -> P.Ptr -> m ()
    setPtrIndex NormalList{nPtr=nPtr@M.WordPtr{pAddr=addr@WordAt{wordIndex}}} absPtr relPtr =
        let srcPtr = nPtr { M.pAddr = addr { wordIndex = wordIndex + WordCount i } }
        in setPointerTo srcPtr (ptrAddr absPtr) relPtr

-- | @'setPointerTo' msg srcLoc dstAddr relPtr@ sets the word at @srcLoc@ in @msg@ to a
-- pointer like @relPtr@, but pointing to @dstAddr@. @relPtr@ should not be a far pointer.
-- If the two addresses are in different segments, a landing pad will be allocated and
-- @srcLoc@ will contain a far pointer.
setPointerTo :: M.WriteCtx m s => M.WordPtr ('Mut s) -> WordAddr -> P.Ptr -> m ()
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
copyCap dest cap = getClient cap >>= appendCap dest

-- | Make a copy of the value at the pointer, in the target message.
copyPtr :: RWCtx m s => M.Message ('Mut s) -> Maybe (Ptr ('Mut s)) -> m (Maybe (Ptr ('Mut s)))
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
    :: RWCtx m s
    => M.Message ('Mut s)
    -> ListOf ('Mut s) a
    -> (M.Message ('Mut s) -> Int -> m (ListOf ('Mut s) a))
    -> m (ListOf ('Mut s) a)
{-# INLINE copyNewListOf #-}
copyNewListOf destMsg src new = do
    dest <- new destMsg (length src)
    copyListOf dest src
    pure dest


-- | Make a copy of the list, in the target message.
copyListOf :: RWCtx m s => ListOf ('Mut s) a -> ListOf ('Mut s) a -> m ()
{-# SPECIALIZE copyListOf :: ListOf ('Mut RealWorld) a -> ListOf ('Mut RealWorld) a -> LimitT IO () #-}
copyListOf dest src =
    forM_ [0..length src - 1] $ \i -> do
        value <- index i src
        setIndex value i dest

-- | @'copyStruct' dest src@ copies the source struct to the destination struct.
copyStruct :: RWCtx m s => Struct ('Mut s) -> Struct ('Mut s) -> m ()
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
index :: ReadCtx m mut => Int -> ListOf mut a -> m a
{-# SPECIALIZE index :: Int -> ListOf 'Const a -> LimitT IO a #-}
{-# SPECIALIZE index :: Int -> ListOf ('Mut RealWorld) a -> LimitT IO a #-}
index i list
    | i < 0 || i >= length list =
        throwM E.BoundsError { E.index = i, E.maxIndex = length list - 1 }
    | otherwise = index' list
  where
    index' :: ReadCtx m mut => ListOf mut a -> m a
    index' (ListOfVoid _) = pure ()
    index' (ListOfStruct (Struct ptr@M.WordPtr{pAddr=addr@WordAt{..}} dataSz ptrSz) _) = do
        let offset = WordCount $ i * (fromIntegral dataSz + fromIntegral ptrSz)
        let addr' = addr { wordIndex = wordIndex + offset }
        return $ Struct ptr { M.pAddr = addr' } dataSz ptrSz
    index' (ListOfBool   nlist) = do
        Word1 val <- indexNList nlist 64
        pure val
    index' (ListOfWord8  nlist) = indexNList nlist 8
    index' (ListOfWord16 nlist) = indexNList nlist 4
    index' (ListOfWord32 nlist) = indexNList nlist 2
    index' (ListOfWord64 (NormalList M.WordPtr{pSegment, pAddr=WordAt{wordIndex}} _)) =
        M.read pSegment $ wordIndex + WordCount i
    index' (ListOfPtr (NormalList ptr@M.WordPtr{pAddr=addr@WordAt{..}} _)) =
        get ptr { M.pAddr = addr { wordIndex = wordIndex + WordCount i } }
    indexNList :: (ReadCtx m mut, Integral a) => NormalList mut -> Int -> m a
    indexNList (NormalList M.WordPtr{pSegment, pAddr=WordAt{..}} _) eltsPerWord = do
        let wordIndex' = wordIndex + WordCount (i `div` eltsPerWord)
        word <- M.read pSegment wordIndex'
        let shift = (i `mod` eltsPerWord) * (64 `div` eltsPerWord)
        pure $ fromIntegral $ word `shiftR` shift

-- | Returns the length of a list
length :: ListOf msg a -> Int
length (ListOfStruct _ len) = len
length (ListOfVoid   nlist) = nLen nlist
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
    go (ListOfStruct tag _) = ListOfStruct tag count
    go (ListOfVoid nlist)   = ListOfVoid $ nTake nlist
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
{-# INLINE dataSection #-}
dataSection (Struct ptr dataSz _) =
    ListOfWord64 $ NormalList ptr (fromIntegral dataSz)

-- | The pointer section of a struct, as a list of Ptr
ptrSection :: Struct msg -> ListOf msg (Maybe (Ptr msg))
{-# INLINE ptrSection #-}
ptrSection (Struct ptr@M.WordPtr{pAddr=addr@WordAt{wordIndex}} dataSz ptrSz) =
    ListOfPtr $ NormalList
        { nPtr = ptr { M.pAddr = addr { wordIndex = wordIndex + fromIntegral dataSz } }
        , nLen = fromIntegral ptrSz
        }

-- | Get the size (in words) of a struct's data section.
structWordCount :: Struct msg -> WordCount
structWordCount (Struct _ptr dataSz _ptrSz) = fromIntegral dataSz

-- | Get the size (in bytes) of a struct's data section.
structByteCount :: Struct msg -> ByteCount
structByteCount = wordsToBytes . structWordCount

-- | Get the size of a struct's pointer section.
structPtrCount  :: Struct msg -> Word16
structPtrCount (Struct _ptr _dataSz ptrSz) = ptrSz

-- | Get the size (in words) of the data sections in a struct list.
structListWordCount :: ListOf msg (Struct msg) -> WordCount
structListWordCount (ListOfStruct s _) = structWordCount s

-- | Get the size (in words) of the data sections in a struct list.
structListByteCount :: ListOf msg (Struct msg) -> ByteCount
structListByteCount (ListOfStruct s _) = structByteCount s

-- | Get the size of the pointer sections in a struct list.
structListPtrCount  :: ListOf msg (Struct msg) -> Word16
structListPtrCount  (ListOfStruct s _) = structPtrCount s

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
checkPtr Nothing              = pure ()
checkPtr (Just (PtrCap c))    = checkCap c
checkPtr (Just (PtrList l))   = checkList l
checkPtr (Just (PtrStruct s)) = checkStruct s

checkCap :: ReadCtx m mut => Cap mut -> m ()
checkCap (Cap _ _ ) = pure ()
    -- No need to do anything here; an out of bounds index is just treated
    -- as null.

checkList :: ReadCtx m mut => List mut -> m ()
checkList (List0 l)      = checkListOf l
checkList (List1 l)      = checkListOf l
checkList (List8 l)      = checkListOf l
checkList (List16 l)     = checkListOf l
checkList (List32 l)     = checkListOf l
checkList (List64 l)     = checkListOf l
checkList (ListPtr l)    = checkListOf l
checkList (ListStruct l) = checkListOf l

checkListOf :: ReadCtx m mut => ListOf mut a -> m ()
checkListOf (ListOfStruct s@(Struct ptr _ _) len) =
    checkPtrOffset ptr (fromIntegral len * structSize s)
checkListOf (ListOfVoid _) = pure ()
checkListOf (ListOfBool l) = checkNormalList l 1
checkListOf (ListOfWord8 l) = checkNormalList l 8
checkListOf (ListOfWord16 l) = checkNormalList l 16
checkListOf (ListOfWord32 l) = checkNormalList l 32
checkListOf (ListOfWord64 l) = checkNormalList l 64
checkListOf (ListOfPtr l) = checkNormalList l 64

checkNormalList :: ReadCtx m mut => NormalList mut -> BitCount -> m ()
checkNormalList NormalList{nPtr, nLen} eltSize =
    let nBits = fromIntegral nLen * eltSize
        nWords = bytesToWordsCeil $ bitsToBytesCeil nBits
    in
    checkPtrOffset nPtr nWords

checkStruct :: ReadCtx m mut => Struct mut -> m ()
checkStruct s@(Struct ptr _ _) =
    checkPtrOffset ptr (structSize s)

checkPtrOffset :: ReadCtx m mut => M.WordPtr mut -> WordCount -> m ()
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
{-# SPECIALIZE invoicePtr :: Maybe (Ptr ('Mut RealWorld)) -> LimitT IO () #-}
invoicePtr p = invoice $! ptrInvoiceSize p

ptrInvoiceSize :: Maybe (Ptr mut) -> WordCount
ptrInvoiceSize = \case
    Nothing            -> 1
    Just (PtrCap _)    -> 1
    Just (PtrStruct s) -> structInvoiceSize s
    Just (PtrList l)   -> listInvoiceSize l
listInvoiceSize :: List mut -> WordCount
listInvoiceSize l = max 1 $! case l of
    List0 l   -> fromIntegral $! length l
    List1 l   -> fromIntegral $! length l `div` 64
    List8 l   -> fromIntegral $! length l `div`  8
    List16 l  -> fromIntegral $! length l `div`  4
    List32 l  -> fromIntegral $! length l `div`  2
    List64 l  -> fromIntegral $! length l
    ListPtr l -> fromIntegral $! length l
    ListStruct (ListOfStruct s len) ->
        structInvoiceSize s * fromIntegral len
structInvoiceSize :: Struct mut -> WordCount
structInvoiceSize (Struct _ dataSz ptrSz) =
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
rawBytes :: ReadCtx m 'Const => ListOf 'Const Word8 -> m BS.ByteString
-- TODO: we can get away with a more lax context than ReadCtx, maybe even make
-- this non-monadic.
rawBytes (ListOfWord8 (NormalList M.WordPtr{pSegment, pAddr=WordAt{wordIndex}} len)) = do
    let bytes = M.toByteString pSegment
    let ByteCount byteOffset = wordsToBytes wordIndex
    pure $ BS.take len $ BS.drop byteOffset bytes


-- | Returns the root pointer of a message.
rootPtr :: ReadCtx m mut => M.Message mut -> m (Struct mut)
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
        Nothing -> messageDefault msg
        _ -> throwM $ E.SchemaViolationError
                "Unexpected root type; expected struct."


-- | Make the given struct the root object of its message.
setRoot :: M.WriteCtx m s => Struct ('Mut s) -> m ()
setRoot (Struct M.WordPtr{pMessage, pAddr=addr} dataSz ptrSz) = do
    pSegment <- M.getSegment pMessage 0
    let rootPtr = M.WordPtr{pMessage, pSegment, pAddr = WordAt 0 0}
    setPointerTo rootPtr addr (P.StructPtr 0 dataSz ptrSz)

-- | Allocate a struct in the message.
allocStruct :: M.WriteCtx m s => M.Message ('Mut s) -> Word16 -> Word16 -> m (Struct ('Mut s))
allocStruct msg dataSz ptrSz = do
    let totalSz = fromIntegral dataSz + fromIntegral ptrSz
    ptr <- M.alloc msg totalSz
    pure $ Struct ptr dataSz ptrSz

-- | Allocate a composite list.
allocCompositeList
    :: M.WriteCtx m s
    => M.Message ('Mut s) -- ^ The message to allocate in.
    -> Word16     -- ^ The size of the data section
    -> Word16     -- ^ The size of the pointer section
    -> Int        -- ^ The length of the list in elements.
    -> m (ListOf ('Mut s) (Struct ('Mut s)))
allocCompositeList msg dataSz ptrSz len = do
    let eltSize = fromIntegral dataSz + fromIntegral ptrSz
    ptr@M.WordPtr{pSegment, pAddr=addr@WordAt{wordIndex}}
        <- M.alloc msg (WordCount $ len * eltSize + 1) -- + 1 for the tag word.
    M.write pSegment wordIndex $ P.serializePtr' $ P.StructPtr (fromIntegral len) dataSz ptrSz
    let firstStruct = Struct
            ptr { M.pAddr = addr { wordIndex = wordIndex + 1 } }
            dataSz
            ptrSz
    pure $ ListOfStruct firstStruct len

-- | Allocate a list of capnproto @Void@ values.
allocList0   :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (ListOf ('Mut s) ())

-- | Allocate a list of booleans
allocList1   :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (ListOf ('Mut s) Bool)

-- | Allocate a list of 8-bit values.
allocList8   :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (ListOf ('Mut s) Word8)

-- | Allocate a list of 16-bit values.
allocList16  :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (ListOf ('Mut s) Word16)

-- | Allocate a list of 32-bit values.
allocList32  :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (ListOf ('Mut s) Word32)

-- | Allocate a list of 64-bit words.
allocList64  :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (ListOf ('Mut s) Word64)

-- | Allocate a list of pointers.
allocListPtr :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (ListOf ('Mut s) (Maybe (Ptr ('Mut s))))

allocList0   msg len = ListOfVoid   <$> allocNormalList 0  msg len
allocList1   msg len = ListOfBool   <$> allocNormalList 1  msg len
allocList8   msg len = ListOfWord8  <$> allocNormalList 8  msg len
allocList16  msg len = ListOfWord16 <$> allocNormalList 16 msg len
allocList32  msg len = ListOfWord32 <$> allocNormalList 32 msg len
allocList64  msg len = ListOfWord64 <$> allocNormalList 64 msg len
allocListPtr msg len = ListOfPtr    <$> allocNormalList 64 msg len

-- | Allocate a NormalList
allocNormalList
    :: M.WriteCtx m s
    => Int                  -- ^ The number bits per element
    -> M.Message ('Mut s) -- ^ The message to allocate in
    -> Int                  -- ^ The number of elements in the list.
    -> m (NormalList ('Mut s))
allocNormalList bitsPerElt msg len = do
    -- round 'len' up to the nearest word boundary.
    let totalBits = BitCount (len * bitsPerElt)
        totalWords = bytesToWordsCeil $ bitsToBytesCeil totalBits
    ptr <- M.alloc msg totalWords
    pure NormalList { nPtr = ptr, nLen = len }

appendCap :: M.WriteCtx m s => M.Message ('Mut s) -> M.Client -> m (Cap ('Mut s))
appendCap msg client = do
    i <- M.appendCap msg client
    pure $ Cap msg (fromIntegral i)
