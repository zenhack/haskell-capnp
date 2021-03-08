{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Capnp.GenHelpers.New
    ( dataField
    , ptrField
    , groupField
    , voidField
    , TypeParam
    ) where

import           Capnp.Bits
import qualified Capnp.Classes as C
import qualified Capnp.Fields  as F
import qualified Capnp.Repr    as R
import           Data.Bits
import           Data.Word

dataField
    :: forall b a sz.
    ( R.ReprFor b ~ 'R.Data sz
    , C.IsWord (R.UntypedData sz)
    )
    => BitCount -> Word16 -> BitCount -> Word64 -> F.Field 'F.Slot a b
dataField shift index nbits defaultValue = F.Field $ F.DataField @sz F.DataFieldLoc
    { shift
    , index
    , mask = (1 `shiftL` fromIntegral nbits) - 1
    , defaultValue
    }

ptrField :: (R.IsPtrRepr p, R.ReprFor b ~ 'R.Ptr p) => Word16 -> F.Field 'F.Slot a b
ptrField = F.Field . F.PtrField

groupField :: (R.ReprFor b ~ 'R.Ptr ('Just 'R.Struct)) => F.Field 'F.Group a b
groupField = F.Field F.GroupField

voidField :: (R.ReprFor b ~ 'R.Data 'R.Sz0) => F.Field 'F.Slot a b
voidField = F.Field F.VoidField

type TypeParam a pr =
    ( R.ReprFor a ~ 'R.Ptr pr
    , R.IsPtrRepr pr
    )
