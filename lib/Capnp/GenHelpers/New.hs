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
    , Mutability(..)
    , TypeParam
    , newStruct
    , parseField
    , encodeField
    , encodeVariant
    , initVariant
    , unionWhich
    , readField
    , structUnion
    , unionStruct
    , parseEnum
    , encodeEnum
    , parseCap
    , encodeCap
    , getPtrConst
    , module F
    , module Capnp.Repr.Methods
    , module Capnp.New.Rpc.Server
    , buildCallHandler
    ) where

import           Capnp.Bits
import qualified Capnp.Classes        as C
import           Capnp.Fields         as F
import           Capnp.GenHelpers     (getPtrConst)
import           Capnp.Message        (Mutability(..))
import qualified Capnp.Message        as M
import           Capnp.New
    ( TypeParam
    , encodeField
    , encodeVariant
    , initVariant
    , parseField
    , readField
    , structUnion
    , unionStruct
    , unionWhich
    )
import qualified Capnp.New.Basics     as NB
import qualified Capnp.New.Classes    as NC
import           Capnp.New.Rpc.Server
import qualified Capnp.Repr           as R
import           Capnp.Repr.Methods
import qualified Capnp.Untyped        as U
import           Data.Bits
import qualified Data.Map.Strict      as M
import qualified Data.Vector          as V
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

ptrField :: forall a b. R.IsPtr b => Word16 -> F.Field 'F.Slot a b
ptrField = F.Field . F.PtrField @(R.PtrReprFor (R.ReprFor b))

groupField :: (R.ReprFor b ~ 'R.Ptr ('Just 'R.Struct)) => F.Field 'F.Group a b
groupField = F.Field F.GroupField

voidField :: (R.ReprFor b ~ 'R.Data 'R.Sz0) => F.Field 'F.Slot a b
voidField = F.Field F.VoidField

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


parseEnum :: (R.ReprFor a ~ 'R.Data 'R.Sz16, Enum a, Applicative m)
    => R.Raw 'Const a -> m a
parseEnum (R.Raw n) = pure $ toEnum $ fromIntegral n

encodeEnum :: forall a m s. (R.ReprFor a ~ 'R.Data 'R.Sz16, Enum a, U.RWCtx m s)
    => M.Message ('Mut s) -> a -> m (R.Raw ('Mut s) a)
encodeEnum _msg value = pure $ R.Raw $ fromIntegral $ fromEnum @a value

parseCap :: (R.IsCap a, U.ReadCtx m 'Const) => R.Raw 'Const a -> m (Client a)
parseCap (R.Raw cap) = Client <$> U.getClient cap

encodeCap :: (R.IsCap a, U.RWCtx m s) => M.Message ('Mut s) -> Client a -> m (R.Raw ('Mut s) a)
encodeCap msg (Client c) = R.Raw <$> U.appendCap msg c


buildCallHandler :: [(Word64, [UntypedMethodHandler])] -> CallHandler
buildCallHandler hs = M.fromList [ (k, V.fromList v) | (k, v) <- hs ]
