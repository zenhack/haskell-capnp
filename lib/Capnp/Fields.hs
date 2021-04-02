{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
-- | Module: Capnp.Fields
-- Description: Support for working with struct fields
module Capnp.Fields
    ( HasField
    , Field(..)
    , FieldLoc(..)
    , DataFieldLoc(..)
    , FieldKind(..)
    , HasUnion(..)
    , Variant(..)
    , HasVariant
    ) where

import Capnp.Bits
import Data.Word
import GHC.OverloadedLabels (IsLabel)

import qualified Capnp.Classes as C
import qualified Capnp.Message as M
import qualified Capnp.Repr    as R
import qualified Capnp.Untyped as U

data FieldKind = Slot | Group

-- | @'Field' a b@ is a first-class representation of a field of type @b@ within
-- an @a@, where @a@ must be a struct type.
newtype Field (k :: FieldKind) a b = Field (FieldLoc k (R.ReprFor b))

-- | The location of a field within a message.
data FieldLoc (k :: FieldKind) (r :: R.Repr) where
    GroupField :: FieldLoc 'Group ('R.Ptr ('Just 'R.Struct))
    PtrField :: R.IsPtrRepr a => Word16 -> FieldLoc 'Slot ('R.Ptr a)
    DataField :: C.IsWord (R.UntypedData a) => DataFieldLoc a -> FieldLoc 'Slot ('R.Data a)
    VoidField :: FieldLoc 'Slot ('R.Data 'R.Sz0)

-- | The location of a data (non-pointer) field.
data DataFieldLoc (sz :: R.DataSz) = DataFieldLoc
    { shift        :: !BitCount
    , index        :: !Word16
    , mask         :: !Word64
    , defaultValue :: !Word64
    }

class R.IsStruct a => HasUnion a where
    unionField :: Field 'Slot a Word16
    data RawWhich (mut :: M.Mutability) a
    internalWhich :: U.ReadCtx m mut => Word16 -> R.Raw mut a -> m (RawWhich mut a)

data Variant (k :: FieldKind) a b = Variant
    { field    :: !(Field k a b)
    , tagValue :: !Word16
    }

-- | An instance @'HasField' name k a b@ indicates that the struct type @a@
-- has a field named @name@ with type @b@ (with @k@ being the 'FieldKind' for
-- the field). The generated code includes instances of this for each field
-- in the schema.
class
    ( R.IsStruct a
    , IsLabel name (Field k a b)
    ) => HasField name k a b | a name -> k b


class
    ( HasUnion a
    , IsLabel name (Variant k a b)
    ) => HasVariant name k a b | a name -> k b
