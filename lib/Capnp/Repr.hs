{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- | Module: Capnp.Repr
-- Description: Type-level plumbing for wire-representations.
--
-- This module provides facilities for working with the wire
-- representations of capnproto objects at the type level. The most
-- central part of this module is the 'Repr' type.
--
-- Recommended reading: https://capnproto.org/encoding.html
module Capnp.Repr
    (
    -- * Type-level descriptions of wire representations.
      Repr(..)
    , PtrRepr(..)
    , ListRepr(..)
    , NormalListRepr(..)
    , DataSz(..)

    -- * Mapping representations to value types from "Capnp.Untyped"
    , Untyped
    , UntypedData
    , UntypedPtr
    , UntypedSomePtr
    , UntypedList
    , UntypedSomeList

    -- * Mapping types to their wire representations.
    , ReprFor
    , PtrReprFor

    -- * Relating the representations of lists & their elements.
    , ElemRepr
    , ListReprFor
    , Element(..)

    -- * Working with wire-encoded values
    , Raw(..)

    -- * Working with lists
    , List
    , length
    , index
    , setIndex

    -- * Working with pointers
    , IsPtrRepr(..)
    , IsListPtrRepr(..)

    -- * Allocating values
    , Allocate(..)

    -- * Shorthands for types
    , IsStruct
    , IsCap
    , IsPtr
    ) where

import Prelude hiding (length)

import qualified Capnp.Errors         as E
import           Capnp.Message        (Mutability(..))
import qualified Capnp.Message        as M
import           Capnp.TraversalLimit (evalLimitT)
import qualified Capnp.Untyped        as U
import           Control.Monad.Catch  (MonadThrow(..))
import           Data.Default         (Default(..))
import           Data.Int
import           Data.Kind            (Type)
import           Data.Maybe           (fromJust)
import           Data.Word
import           GHC.Generics         (Generic)
import qualified Language.Haskell.TH  as TH

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
    ListData :: DataSz -> NormalListRepr
    ListPtr :: NormalListRepr
    deriving(Show)

-- | The size of a non-pointer type. @SzN@ represents an @N@-bit value.
data DataSz = Sz0 | Sz1 | Sz8 | Sz16 | Sz32 | Sz64
    deriving(Show)

-- | @Untyped mut r@ is an untyped value with representation @r@ stored in
-- a message with mutability @mut@.
type family Untyped (mut :: Mutability) (r :: Repr) :: Type where
    Untyped mut ('Data sz) = UntypedData sz
    Untyped mut ('Ptr ptr) = UntypedPtr mut ptr

-- | @UntypedData sz@ is an untyped value with size @sz@.
type family UntypedData (sz :: DataSz) :: Type where
    UntypedData 'Sz0 = ()
    UntypedData 'Sz1 = Bool
    UntypedData 'Sz8 = Word8
    UntypedData 'Sz16 = Word16
    UntypedData 'Sz32 = Word32
    UntypedData 'Sz64 = Word64

-- | Like 'Untyped', but for pointers only.
type family UntypedPtr (mut :: Mutability) (r :: Maybe PtrRepr) :: Type where
    UntypedPtr mut 'Nothing = Maybe (U.Ptr mut)
    UntypedPtr mut ('Just r) = UntypedSomePtr mut r

-- | Like 'UntypedPtr', but doesn't allow AnyPointers.
type family UntypedSomePtr (mut :: Mutability) (r :: PtrRepr) :: Type where
    UntypedSomePtr mut 'Struct = U.Struct mut
    UntypedSomePtr mut 'Cap = U.Cap mut
    UntypedSomePtr mut ('List r) = UntypedList mut r

-- | Like 'Untyped', but for lists only.
type family UntypedList (mut :: Mutability) (r :: Maybe ListRepr) :: Type where
    UntypedList mut 'Nothing = U.List mut
    UntypedList mut ('Just r) = UntypedSomeList mut r

-- | Like 'UntypedList', but doesn't allow AnyLists.
type family UntypedSomeList (mut :: Mutability) (r :: ListRepr) :: Type where
    UntypedSomeList mut r = U.ListOf mut (Untyped mut (ElemRepr r))


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
    alloc :: U.RWCtx m s => M.Message ('Mut s) -> AllocHint r -> m (UntypedSomePtr ('Mut s) r)

-- | @'ReprFor' a@ denotes the Cap'n Proto wire represent of the type @a@.
type family ReprFor (a :: Type) :: Repr

type instance ReprFor () = 'Data 'Sz0
type instance ReprFor Bool = 'Data 'Sz1
type instance ReprFor Word8 = 'Data 'Sz8
type instance ReprFor Word16 = 'Data 'Sz16
type instance ReprFor Word32 = 'Data 'Sz32
type instance ReprFor Word64 = 'Data 'Sz64
type instance ReprFor Int8 = 'Data 'Sz8
type instance ReprFor Int16 = 'Data 'Sz16
type instance ReprFor Int32 = 'Data 'Sz32
type instance ReprFor Int64 = 'Data 'Sz64
type instance ReprFor Float = 'Data 'Sz32
type instance ReprFor Double = 'Data 'Sz64

type instance ReprFor (U.ListOf mut a) = ReprFor (List a)
type instance ReprFor (U.Struct mut) = 'Ptr ('Just 'Struct)
type instance ReprFor (U.Cap mut) = 'Ptr ('Just 'Cap)
type instance ReprFor (U.Ptr mut) = 'Ptr 'Nothing
type instance ReprFor (U.List mut) = 'Ptr ('Just ('List 'Nothing))

type instance ReprFor (List a) = 'Ptr ('Just ('List ('Just (ListReprFor (ReprFor a)))))

-- | @PtrReprFor r@ extracts the pointer represnetation in r; undefined if
-- r is not a pointer representation.
type family PtrReprFor (r :: Repr) :: Maybe PtrRepr where
    PtrReprFor ('Ptr pr) = pr

-- | @ElemRepr r@ is the representation of elements of lists with
-- representation @r@.
type family ElemRepr (rl :: ListRepr) :: Repr where
    ElemRepr 'ListComposite = 'Ptr ('Just 'Struct)
    ElemRepr ('ListNormal 'ListPtr) = 'Ptr 'Nothing
    ElemRepr ('ListNormal ('ListData sz)) = 'Data sz

-- | @ListReprFor e@ is the representation of lists with elements
-- whose representation is @e@.
type family ListReprFor (e :: Repr) :: ListRepr where
    ListReprFor ('Data sz) = 'ListNormal ('ListData sz)
    ListReprFor ('Ptr ('Just 'Struct)) = 'ListComposite
    ListReprFor ('Ptr a) = 'ListNormal 'ListPtr

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
        :: forall m mut. U.ReadCtx m mut
        => M.Message mut
        -> Untyped mut (ElemRepr (ListReprFor r))
        -> m (Untyped mut r)
    toElement :: Untyped mut r -> Untyped mut (ElemRepr (ListReprFor r))

instance Element ('Data sz) where
    fromElement _ = pure
    toElement = id
instance Element ('Ptr ('Just 'Struct)) where
    fromElement _ = pure
    toElement = id
instance Element ('Ptr 'Nothing) where
    fromElement _ = pure
    toElement = id
instance Element ('Ptr ('Just 'Cap)) where
    fromElement = fromPtr @('Just 'Cap)
    toElement = Just . U.PtrCap
instance IsPtrRepr ('Just ('List a)) => Element ('Ptr ('Just ('List a))) where
    fromElement = fromPtr @('Just ('List a))
    toElement = toPtr @('Just ('List a))

-- | A @'Raw' mut a@ is an @a@ embedded in a capnproto message with mutability
-- @mut@.
newtype Raw (a :: Type ) (mut :: Mutability)
    = Raw { fromRaw :: Untyped mut (ReprFor a) }

deriving instance Show (Untyped mut (ReprFor a)) => Show (Raw a mut)
deriving instance Read (Untyped mut (ReprFor a)) => Read (Raw a mut)
deriving instance Eq (Untyped mut (ReprFor a)) => Eq (Raw a mut)
deriving instance Generic (Untyped mut (ReprFor a)) => Generic (Raw a mut)

-- | A phantom type denoting capnproto lists of type @a@.
data List a

-- | Get the length of a capnproto list.
length :: Raw (List a) mut -> Int
length (Raw l) = U.length l

-- | @'index' i list@ gets the @i@th element of the list.
index :: forall a m mut.
    ( U.ReadCtx m mut
    , Element (ReprFor a)
    ) => Int -> Raw (List a) mut -> m (Raw a mut)
index i (Raw l) =
    Raw <$> (U.index i l >>= fromElement @(ReprFor a) @m @mut (U.message l))

-- | @'setIndex' value i list@ sets the @i@th element of @list@ to @value@.
setIndex :: forall a m s.
    ( U.RWCtx m s
    , Element (ReprFor a)
    ) => Raw a ('Mut s) -> Int -> Raw (List a) ('Mut s) -> m ()
setIndex (Raw v) i (Raw l) = U.setIndex (toElement @(ReprFor a) @('Mut s) v) i l

instance U.HasMessage (Untyped mut (ReprFor a)) mut => U.HasMessage (Raw a mut) mut where
    message (Raw r) = U.message r
instance U.MessageDefault (Untyped mut (ReprFor a)) mut => U.MessageDefault (Raw a mut) mut where
    messageDefault msg = Raw <$> U.messageDefault msg

instance U.MessageDefault (Raw a 'Const) 'Const => Default (Raw a 'Const) where
    def = fromJust $ evalLimitT maxBound $ U.messageDefault M.empty

-- | Operations on types with pointer representations.
class IsPtrRepr (r :: Maybe PtrRepr) where
    toPtr :: Untyped mut ('Ptr r) -> Maybe (U.Ptr mut)
    -- ^ Convert an untyped value of this representation to an AnyPointer.
    fromPtr :: U.ReadCtx m mut => M.Message mut -> Maybe (U.Ptr mut) -> m (Untyped mut ('Ptr r))
    -- ^ Extract a value with this representation from an AnyPointer, failing
    -- if the pointer is the wrong type for this representation.

instance IsPtrRepr 'Nothing where
    toPtr p = p
    fromPtr _ p = pure p

instance IsPtrRepr ('Just 'Struct) where
    toPtr s = Just (U.PtrStruct s)
    fromPtr msg Nothing              = U.messageDefault msg
    fromPtr _ (Just (U.PtrStruct s)) = pure s
    fromPtr _ _                      = expected "pointer to struct"
instance IsPtrRepr ('Just 'Cap) where
    toPtr c = Just (U.PtrCap c)
    fromPtr _ Nothing             = expected "pointer to capability"
    fromPtr _ (Just (U.PtrCap c)) = pure c
    fromPtr _ _                   = expected "pointer to capability"
instance IsPtrRepr ('Just ('List 'Nothing)) where
    toPtr l = Just (U.PtrList l)
    fromPtr _ Nothing              = expected "pointer to list"
    fromPtr _ (Just (U.PtrList l)) = pure l
    fromPtr _ (Just _)             = expected "pointer to list"
instance IsListPtrRepr r => IsPtrRepr ('Just ('List ('Just r))) where
    toPtr l = Just (U.PtrList (rToList @r l))
    fromPtr msg Nothing            = rFromListMsg @r msg
    fromPtr _ (Just (U.PtrList l)) = rFromList @r l
    fromPtr _ (Just _)             = expected "pointer to list"

-- | Operations on types with list representations.
class IsListPtrRepr (r :: ListRepr) where
    rToList :: UntypedSomeList mut r -> U.List mut
    -- ^ Convert an untyped value of this representation to an AnyList.
    rFromList :: U.ReadCtx m mut => U.List mut -> m (UntypedSomeList mut r)
    -- ^ Extract a value with this representation from an AnyList, failing
    -- if the list is the wrong type for this representation.
    rFromListMsg :: U.ReadCtx m mut => M.Message mut -> m (UntypedSomeList mut r)
    -- ^ Create a zero-length value with this representation, living in the
    -- provided message.

-- helper function for throwing SchemaViolationError "expected ..."
expected :: MonadThrow m => String -> m a
expected msg = throwM $ E.SchemaViolationError $ "expected " ++ msg

do
    let mkIsListPtrRepr (r, listC, str) =
            [d| instance IsListPtrRepr $r where
                    rToList = $(pure $ TH.ConE listC)
                    rFromList $(pure $ TH.ConP listC [TH.VarP (TH.mkName "l")]) = pure l
                    rFromList _ = expected $(pure $ TH.LitE $ TH.StringL $ "pointer to " ++ str)
                    rFromListMsg = U.messageDefault
            |]
    concat <$> traverse mkIsListPtrRepr
        [ ( [t| 'ListNormal ('ListData 'Sz0) |]
          , 'U.List0
          , "List(Void)"
          )
        , ( [t| 'ListNormal ('ListData 'Sz1) |]
          , 'U.List1
          , "List(Bool)"
          )
        , ( [t| 'ListNormal ('ListData 'Sz8) |]
          , 'U.List8
          , "List(UInt8)"
          )
        , ( [t| 'ListNormal ('ListData 'Sz16) |]
          , 'U.List16
          , "List(UInt16)"
          )
        , ( [t| 'ListNormal ('ListData 'Sz32) |]
          , 'U.List32
          , "List(UInt32)"
          )
        , ( [t| 'ListNormal ('ListData 'Sz64) |]
          , 'U.List64
          , "List(UInt64)"
          )
        , ( [t| 'ListNormal 'ListPtr |]
          , 'U.ListPtr
          , "List(AnyPointer)"
          )
        , ( [t| 'ListComposite |]
          , 'U.ListStruct
          , "composite list"
          )
        ]

instance Allocate 'Struct where
    type AllocHint 'Struct = (Word16, Word16)
    alloc msg = uncurry (U.allocStruct msg)
instance Allocate 'Cap where
    type AllocHint 'Cap = M.Client
    alloc = U.appendCap
instance Allocate ('List ('Just 'ListComposite)) where
    type AllocHint ('List ('Just 'ListComposite)) = (Int, AllocHint 'Struct)
    alloc msg (len, (nWords, nPtrs)) = U.allocCompositeList msg nWords nPtrs len
instance AllocateNormalList r => Allocate ('List ('Just ('ListNormal r))) where
    type AllocHint ('List ('Just ('ListNormal r))) = Int
    alloc = allocNormalList @r

class AllocateNormalList (r :: NormalListRepr) where
    allocNormalList :: U.RWCtx m s => M.Message ('Mut s) -> Int -> m (UntypedSomeList ('Mut s) ('ListNormal r))
instance AllocateNormalList ('ListData 'Sz0) where allocNormalList = U.allocList0
instance AllocateNormalList ('ListData 'Sz1) where allocNormalList = U.allocList1
instance AllocateNormalList ('ListData 'Sz8) where allocNormalList = U.allocList8
instance AllocateNormalList ('ListData 'Sz16) where allocNormalList = U.allocList16
instance AllocateNormalList ('ListData 'Sz32) where allocNormalList = U.allocList32
instance AllocateNormalList ('ListData 'Sz64) where allocNormalList = U.allocList64
instance AllocateNormalList 'ListPtr where allocNormalList = U.allocListPtr


-- | Constraint that @a@ is a struct type.
type IsStruct a = ReprFor a ~ 'Ptr ('Just 'Struct)

-- | Constraint that @a@ is a capability type.
type IsCap a = ReprFor a ~ 'Ptr ('Just 'Cap)

-- | Constraint that @a@ is a pointer type.
type IsPtr a =
    ( ReprFor a ~ 'Ptr (PtrReprFor (ReprFor a))
    , IsPtrRepr (PtrReprFor (ReprFor a))
    )
