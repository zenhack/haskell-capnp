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
    , readVariant
    , TypeParam
    , newStruct
    , parseField
    , encodeField
    , unionWhich
    , readField
    , structUnion
    , module F
    , module Capnp.Repr.Methods
    ) where

import           Capnp.Bits
import qualified Capnp.Classes      as C
import           Capnp.Fields       as F
import           Capnp.Message      (Mutability(..))
import qualified Capnp.Message      as M
import           Capnp.New
    (encodeField, parseField, readField, structUnion, unionWhich)
import qualified Capnp.New.Basics   as NB
import qualified Capnp.New.Classes  as NC
import qualified Capnp.Repr         as R
import           Capnp.Repr.Methods
import qualified Capnp.Untyped      as U
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
    , mask = ((1 `shiftL` fromIntegral nbits) - 1) `shiftL` fromIntegral shift
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

-- | Like 'readField', but accepts a variant. Warning: *DOES NOT CHECK* that the
-- variant is the one that is set. This should only be used by generated code.
readVariant
    ::  forall k a b mut m.
        ( R.IsStruct a
        , U.ReadCtx m mut
        )
    => F.Variant k a b -> R.Raw mut a -> m (R.Raw mut b)
readVariant F.Variant{field} = readField field


newStruct :: forall a m s. (U.RWCtx m s, NC.TypedStruct a) => () -> M.Message ('Mut s) -> m (R.Raw ('Mut s) a)
newStruct () msg = R.Raw . R.fromRaw <$> NC.new @NB.AnyStruct (NC.numStructWords @a, NC.numStructPtrs @a) msg
