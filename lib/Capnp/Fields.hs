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

import qualified Capnp.Repr as R

data FieldLoc (r :: R.Repr) where
    GroupField :: FieldLoc ('R.Ptr ('Just 'R.Struct))
    UnionField :: Word16 -> FieldLoc ('R.Ptr ('Just 'R.Struct))
    PtrField :: Word16 -> FieldLoc ('R.Ptr a)
    DataField :: DataFieldLoc a -> FieldLoc ('R.Data a)

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

