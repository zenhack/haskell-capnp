{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
-- | Module: Capnp.Fields
-- Description: Support for working with struct fields
module Capnp.Fields
    ( HasField
    , Field(..)
    , FieldLoc(..)
    , DataFieldLoc(..)
    ) where

import Capnp.Bits
import Data.Word
import GHC.OverloadedLabels (IsLabel)

import qualified Capnp.Classes as C
import qualified Capnp.Repr    as R

-- | @'Field' a b@ is a first-class representation of a field of type @b@ within
-- an @a@, where @a@ must be a struct type.
newtype Field a b = Field (FieldLoc (R.ReprFor b))

-- | The location of a field within a message.
data FieldLoc (r :: R.Repr) where
    GroupField :: FieldLoc ('R.Ptr ('Just 'R.Struct))
    PtrField :: R.IsPtrRepr a => Word16 -> FieldLoc ('R.Ptr a)
    DataField :: C.IsWord (R.UntypedData a) => DataFieldLoc a -> FieldLoc ('R.Data a)
    VoidField :: FieldLoc ('R.Data 'R.Sz0)

-- | The location of a data (non-pointer) field.
data DataFieldLoc (sz :: R.DataSz) = DataFieldLoc
    { shift        :: !BitCount
    , index        :: !Word16
    , mask         :: !Word64
    , defaultValue :: !Word64
    }

-- | An instance @'HasField' name a b@ indicates that the struct type @a@
-- has a field named @name@ with type @b@. The generated code includes
-- instances of this for each field in the schema.
class
    ( R.ReprFor a ~ 'R.Ptr ('Just 'R.Struct)
    , IsLabel name (Field a b)
    ) => HasField name a b | a name -> b
