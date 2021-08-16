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
    , Element(..)
    , ElemRepr
    , ListReprFor

    -- * Working with pointers
    , IsPtrRepr(..)
    , IsListPtrRepr(..)

    -- * Working with wire-encoded values
    , Raw(..)

    -- * Working with lists
    , List
    , length
    , index
    , setIndex

    -- * Allocating values
    , Allocate(..)

    -- * Shorthands for types
    , IsStruct
    , IsCap
    , IsPtr
    ) where

import Prelude hiding (length)

import qualified Capnp.Message           as M
import           Capnp.Mutability        (MaybeMutable(..), Mutability(..))
import           Capnp.TraversalLimit    (evalLimitT)
import           Capnp.Untyped
    ( Allocate(..)
    , DataSz(..)
    , ElemRepr
    , Element(..)
    , IsListPtrRepr(..)
    , IsPtrRepr(..)
    , ListRepr(..)
    , ListReprFor
    , MaybePtr(..)
    , NormalListRepr(..)
    , PtrRepr(..)
    , Repr(..)
    , Untyped
    , UntypedData
    , UntypedList
    , UntypedPtr
    , UntypedSomeList
    , UntypedSomePtr
    , Unwrapped
    )
import qualified Capnp.Untyped           as U
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Default            (Default(..))
import           Data.Int
import           Data.Kind               (Type)
import           Data.Maybe              (fromJust)
import           Data.Traversable        (for)
import           Data.Word
import           GHC.Generics            (Generic)

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

type instance ReprFor (U.Struct mut) = 'Ptr ('Just 'Struct)
type instance ReprFor (U.Cap mut) = 'Ptr ('Just 'Cap)
type instance ReprFor (U.Ptr mut) = 'Ptr 'Nothing
type instance ReprFor (U.List mut) = 'Ptr ('Just ('List 'Nothing))
type instance ReprFor (U.ListOf r mut) = 'Ptr ('Just ('List ('Just (ListReprFor r))))

type instance ReprFor (List a) = 'Ptr ('Just ('List ('Just (ListReprFor (ReprFor a)))))

-- | @PtrReprFor r@ extracts the pointer represnetation in r; undefined if
-- r is not a pointer representation.
type family PtrReprFor (r :: Repr) :: Maybe PtrRepr where
    PtrReprFor ('Ptr pr) = pr

-- | A @'Raw' mut a@ is an @a@ embedded in a capnproto message with mutability
-- @mut@.
newtype Raw (a :: Type ) (mut :: Mutability)
    = Raw { fromRaw :: U.Unwrapped (Untyped (ReprFor a) mut) }

deriving instance Show (U.Unwrapped (Untyped (ReprFor a) mut)) => Show (Raw a mut)
deriving instance Read (U.Unwrapped (Untyped (ReprFor a) mut)) => Read (Raw a mut)
deriving instance Eq (U.Unwrapped (Untyped (ReprFor a) mut)) => Eq (Raw a mut)
deriving instance Generic (U.Unwrapped (Untyped (ReprFor a) mut)) => Generic (Raw a mut)

-- | A phantom type denoting capnproto lists of type @a@.
data List a

type ListElem a =
    ( U.Element (ReprFor a)
    , U.ListItem (ElemRepr (ListReprFor (ReprFor a)))
    )

-- | Get the length of a capnproto list.
length :: ListElem a => Raw (List a) mut -> Int
{-# INLINE length #-}
length (Raw l) = U.length l

-- | @'index' i list@ gets the @i@th element of the list.
index :: forall a m mut.
    ( U.ReadCtx m mut
    , U.HasMessage (U.ListOf (ElemRepr (ListReprFor (ReprFor a))))
    , ListElem a
    ) => Int -> Raw (List a) mut -> m (Raw a mut)
{-# INLINE index #-}
index i (Raw l) = Raw <$> do
    elt <- U.index i l
    fromElement
        @(ReprFor a)
        @m
        @mut
        (U.message @(U.ListOf (ElemRepr (ListReprFor (ReprFor a)))) l)
        elt

-- | @'setIndex' value i list@ sets the @i@th element of @list@ to @value@.
setIndex :: forall a m s.
    ( U.RWCtx m s
    , U.ListItem (ElemRepr (ListReprFor (ReprFor a)))
    , U.Element (ReprFor a)
    ) => Raw a ('Mut s) -> Int -> Raw (List a) ('Mut s) -> m ()
{-# INLINE setIndex #-}
setIndex (Raw v) i (Raw l) = U.setIndex (toElement @(ReprFor a) @('Mut s) v) i l

instance U.HasMessage (Untyped (ReprFor a)) => U.HasMessage (Raw a) where
    message (Raw r) = U.message @(Untyped (ReprFor a)) r
instance U.MessageDefault (Untyped (ReprFor a)) => U.MessageDefault (Raw a) where
    messageDefault msg = Raw <$> U.messageDefault @(Untyped (ReprFor a)) msg

instance U.MessageDefault (Raw a) => Default (Raw a 'Const) where
    def = fromJust $ evalLimitT maxBound $ U.messageDefault @(Raw a) M.empty

instance ReprMaybeMutable (ReprFor a) => MaybeMutable (Raw a) where
    thaw (Raw v) = Raw <$> rThaw @(ReprFor a) v
    freeze (Raw v) = Raw <$> rFreeze @(ReprFor a) v
    unsafeThaw (Raw v) = Raw <$> rUnsafeThaw @(ReprFor a) v
    unsafeFreeze (Raw v) = Raw <$> rUnsafeFreeze @(ReprFor a) v
    {-# INLINE thaw #-}
    {-# INLINE freeze #-}
    {-# INLINE unsafeThaw #-}
    {-# INLINE unsafeFreeze #-}

-- | Like MaybeMutable, but defined on the repr. Helper for implementing
-- MaybeMutable (Raw a)
class ReprMaybeMutable (r :: Repr) where
    rThaw         :: (PrimMonad m, PrimState m ~ s) => Unwrapped (Untyped r 'Const) -> m (Unwrapped (Untyped r ('Mut s)))
    rUnsafeThaw   :: (PrimMonad m, PrimState m ~ s) => Unwrapped (Untyped r 'Const) -> m (Unwrapped (Untyped r ('Mut s)))
    rFreeze       :: (PrimMonad m, PrimState m ~ s) => Unwrapped (Untyped r ('Mut s)) -> m (Unwrapped (Untyped r 'Const))
    rUnsafeFreeze :: (PrimMonad m, PrimState m ~ s) => Unwrapped (Untyped r ('Mut s)) -> m (Unwrapped (Untyped r 'Const))

instance ReprMaybeMutable ('Ptr 'Nothing) where
    rThaw p = do
        MaybePtr p' <- thaw (MaybePtr p)
        pure p'
    rFreeze p = do
        MaybePtr p' <- freeze (MaybePtr p)
        pure p'
    rUnsafeThaw p = do
        MaybePtr p' <- unsafeThaw (MaybePtr p)
        pure p'
    rUnsafeFreeze p = do
        MaybePtr p' <- unsafeFreeze (MaybePtr p)
        pure p'

do
    let types =
            [ [t|'Just 'Struct|]
            , [t|'Just 'Cap|]
            , [t|'Just ('List 'Nothing)|]
            , [t|'Just ('List ('Just 'ListComposite))|]
            , [t|'Just ('List ('Just ('ListNormal 'NormalListPtr)))|]
            ]
    concat <$> for types (\t -> do
        [d|instance ReprMaybeMutable ('Ptr $t) where
            rThaw = thaw
            rFreeze = freeze
            rUnsafeThaw = thaw
            rUnsafeFreeze = freeze
            |])

instance ReprMaybeMutable ('Ptr ('Just ('List ('Just ('ListNormal ('NormalListData sz)))))) where
    rThaw = thaw
    rFreeze = freeze
    rUnsafeThaw = thaw
    rUnsafeFreeze = freeze

instance ReprMaybeMutable ('Data sz) where
    rThaw = pure
    rFreeze = pure
    rUnsafeThaw = pure
    rUnsafeFreeze = pure

-- | Constraint that @a@ is a struct type.
type IsStruct a = ReprFor a ~ 'Ptr ('Just 'Struct)

-- | Constraint that @a@ is a capability type.
type IsCap a = ReprFor a ~ 'Ptr ('Just 'Cap)

-- | Constraint that @a@ is a pointer type.
type IsPtr a =
    ( ReprFor a ~ 'Ptr (PtrReprFor (ReprFor a))
    , IsPtrRepr (PtrReprFor (ReprFor a))
    )
