{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}
module Capnp.GenHelpers.New
    ( dataField
    , ptrField
    , groupField
    , voidField
    , TypeParam
    ) where

import           Capnp.Bits
import qualified Capnp.Fields as F
import qualified Capnp.Repr   as R
import           Data.Bits
import           Data.Word

dataField :: (R.ReprFor b ~ 'R.Data sz) => BitCount -> Word16 -> BitCount -> Word64 -> F.Field a b
dataField shift index nbits defaultValue = F.Field $ F.DataField F.DataFieldLoc
    { shift
    , index
    , mask = (1 `shiftL` fromIntegral nbits) - 1
    , defaultValue
    }

ptrField :: (R.ReprFor b ~ 'R.Ptr p) => Word16 -> F.Field a b
ptrField = F.Field . F.PtrField

groupField :: (R.ReprFor b ~ 'R.Ptr ('Just 'R.Struct)) => F.Field a b
groupField = F.Field F.GroupField

voidField :: (R.ReprFor b ~ 'R.Data 'R.Sz0) => F.Field a b
voidField = dataField 0 0 0 0

type TypeParam a pr =
    ( R.ReprFor a ~ 'R.Ptr pr
    , R.IsPtrRepr pr
    )
