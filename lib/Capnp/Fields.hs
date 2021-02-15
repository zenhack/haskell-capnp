{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
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

data FieldLoc (r :: R.Repr) where
    GroupField :: FieldLoc ('R.Ptr ('Just 'R.Struct))
    PtrField :: R.IsPtrRepr a => Word16 -> FieldLoc ('R.Ptr a)
    DataField :: C.IsWord (R.UntypedData a) => DataFieldLoc a -> FieldLoc ('R.Data a)
    VoidField :: FieldLoc ('R.Data 'R.Sz0)

data DataFieldLoc (sz :: R.DataSz) = DataFieldLoc
    { shift        :: !BitCount
    , index        :: !Word16
    , mask         :: !Word64
    , defaultValue :: !Word64
    }

newtype Field a b = Field (FieldLoc (R.ReprFor b))

class
    ( R.ReprFor a ~ 'R.Ptr ('Just 'R.Struct)
    , IsLabel name (Field a b)
    ) => HasField name a b | a name -> b
