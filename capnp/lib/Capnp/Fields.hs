{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module: Capnp.Fields
-- Description: Support for working with struct fields
module Capnp.Fields
  ( HasField (..),
    Field (..),
    FieldLoc (..),
    DataFieldLoc (..),
    FieldKind (..),
    HasUnion (..),
    Variant (..),
    HasVariant (..),
  )
where

import Capnp.Bits
import qualified Capnp.Classes as C
import qualified Capnp.Message as M
import qualified Capnp.Repr as R
import qualified Capnp.Untyped as U
import Data.Word
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (Symbol)

-- | What sort of field is this? This corresponds to the slot/group variants
-- in the @Field@ type in schema.capnp. Mostly used at the type level with
-- the @DataKinds@ extension.
--
-- (Note that this has nothing to do with kinds in the usual type system sense
-- of the word).
data FieldKind
  = -- | The field is a normal slot; it can be read and written as an
    -- individual value.
    Slot
  | -- | The field is a group. Since this shares space with its parent struct
    -- access patterns are a bit different.
    Group
  deriving (Show, Read, Eq)

-- | @'Field' k a b@ is a first-class representation of a field of type @b@ within
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
  { shift :: !BitCount,
    index :: !Word16,
    mask :: !Word64,
    defaultValue :: !Word64
  }

-- | An instance of 'HasUnion' indicates that the given type is a capnproto struct
-- (or group) with an anonymous union.
class R.IsStruct a => HasUnion a where
  -- | 'unionField' is a field holding the union's tag.
  unionField :: Field 'Slot a Word16

  -- | 'Which' is the abstract capnproto type of the union itself. Like
  -- generated struct types (in this case @a@), this is typically
  -- uninhabitied, and used to define instances and/or act as a phantom type.
  data Which a

  -- | Concrete view into a union embedded in a message. This will be a sum
  -- type with other 'Raw' values as arguments.
  data RawWhich a (mut :: M.Mutability)

  -- | Helper used in generated code to extract a 'RawWhich' from its
  -- surrounding struct.
  internalWhich :: U.ReadCtx m mut => Word16 -> R.Raw a mut -> m (RawWhich a mut)

type instance R.ReprFor (Which a) = 'R.Ptr ('Just 'R.Struct)

instance (C.Allocate a, HasUnion a, R.IsStruct (Which a)) => C.Allocate (Which a) where
  type AllocHint (Which a) = C.AllocHint a
  new hint msg = do
    R.Raw struct <- C.new @a hint msg
    pure (R.Raw struct)

instance
  ( C.Allocate (Which a),
    C.AllocHint (Which a) ~ (),
    C.Parse (Which a) p
  ) =>
  C.EstimateAlloc (Which a) p

-- | @'Variant' k a b@ is a first-class representation of a variant of @a@'s
-- anonymous union, whose argument is of type @b@.
data Variant (k :: FieldKind) a b = Variant
  { field :: !(Field k a b),
    tagValue :: !Word16
  }

-- | An instance @'HasField' name k a b@ indicates that the struct type @a@
-- has a field named @name@ with type @b@ (with @k@ being the 'FieldKind' for
-- the field). The generated code includes instances of this for each field
-- in the schema.
class R.IsStruct a => HasField (name :: Symbol) k a b | a name -> k b where
  fieldByLabel :: Field k a b

instance HasField name k a b => IsLabel name (Field k a b) where
  fromLabel = fieldByLabel @name @k @a @b

-- | An instance @'HasVariant name k a b@ indicates that the struct type @a@
-- has an anonymous union with a variant named @name@, whose argument is of type
-- @b@.
class HasUnion a => HasVariant (name :: Symbol) k a b | a name -> k b where
  variantByLabel :: Variant k a b

instance HasVariant name k a b => IsLabel name (Variant k a b) where
  fromLabel = variantByLabel @name @k @a @b
