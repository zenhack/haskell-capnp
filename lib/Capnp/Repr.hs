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

import           Capnp.Message        (Mutability(..))
import qualified Capnp.Message        as M
import           Capnp.TraversalLimit (evalLimitT)
import           Capnp.Untyped
    ( Allocate(..)
    , DataSz(..)
    , ElemRepr
    , Element(..)
    , IsListPtrRepr(..)
    , IsPtrRepr(..)
    , ListRepr(..)
    , ListReprFor
    , NormalListRepr(..)
    , PtrRepr(..)
    , Repr(..)
    , Untyped
    , UntypedData
    , UntypedList
    , UntypedPtr
    , UntypedSomeList
    , UntypedSomePtr
    )
import qualified Capnp.Untyped        as U
import           Data.Default         (Default(..))
import           Data.Int
import           Data.Kind            (Type)
import           Data.Maybe           (fromJust)
import           Data.Word
import           GHC.Generics         (Generic)

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

-- | A @'Raw' mut a@ is an @a@ embedded in a capnproto message with mutability
-- @mut@.
newtype Raw (mut :: Mutability) (a :: Type)
    = Raw { fromRaw :: Untyped mut (ReprFor a) }

deriving instance Show (Untyped mut (ReprFor a)) => Show (Raw mut a)
deriving instance Read (Untyped mut (ReprFor a)) => Read (Raw mut a)
deriving instance Eq (Untyped mut (ReprFor a)) => Eq (Raw mut a)
deriving instance Generic (Untyped mut (ReprFor a)) => Generic (Raw mut a)

-- | A phantom type denoting capnproto lists of type @a@.
data List a

-- | Get the length of a capnproto list.
length :: Raw mut (List a) -> Int
length (Raw l) = U.length l

-- | @'index' i list@ gets the @i@th element of the list.
index :: forall a m mut.
    ( U.ReadCtx m mut
    , Element (ReprFor a)
    ) => Int -> Raw mut (List a) -> m (Raw mut a)
index i (Raw l) =
    Raw <$> (U.index i l >>= fromElement @(ReprFor a) @m @mut (U.message l))

-- | @'setIndex' value i list@ sets the @i@th element of @list@ to @value@.
setIndex :: forall a m s.
    ( U.RWCtx m s
    , Element (ReprFor a)
    ) => Raw ('Mut s) a -> Int -> Raw ('Mut s) (List a) -> m ()
setIndex (Raw v) i (Raw l) = U.setIndex (toElement @(ReprFor a) @('Mut s) v) i l

instance U.HasMessage (Untyped mut (ReprFor a)) mut => U.HasMessage (Raw mut a) mut where
    message (Raw r) = U.message r
instance U.MessageDefault (Untyped mut (ReprFor a)) mut => U.MessageDefault (Raw mut a) mut where
    messageDefault msg = Raw <$> U.messageDefault msg

instance U.MessageDefault (Raw 'Const a) 'Const => Default (Raw 'Const a) where
    def = fromJust $ evalLimitT maxBound $ U.messageDefault M.empty


-- | Constraint that @a@ is a struct type.
type IsStruct a = ReprFor a ~ 'Ptr ('Just 'Struct)

-- | Constraint that @a@ is a capability type.
type IsCap a = ReprFor a ~ 'Ptr ('Just 'Cap)

-- | Constraint that @a@ is a pointer type.
type IsPtr a =
    ( ReprFor a ~ 'Ptr (PtrReprFor (ReprFor a))
    , IsPtrRepr (PtrReprFor (ReprFor a))
    )
