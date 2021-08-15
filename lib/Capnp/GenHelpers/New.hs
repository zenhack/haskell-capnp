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
    , parseEnum
    , encodeEnum
    , getPtrConst
    , BS.ByteString
    , module F
    , module Capnp.New.Accessors

    -- * Re-exports from the standard library.
    , Proxy(..)
    ) where



import           Capnp.Bits
import           Capnp.Convert         (bsToRaw)
import           Capnp.Fields          as F
import           Capnp.Message         (Mutability(..))
import qualified Capnp.Message         as M
import           Capnp.New.Accessors
import qualified Capnp.New.Basics      as NB
import qualified Capnp.New.Classes     as NC
import           Capnp.New.Constraints (TypeParam)
import qualified Capnp.Repr            as R
import           Capnp.TraversalLimit  (evalLimitT)
import qualified Capnp.Untyped         as U
import           Data.Bits
import qualified Data.ByteString       as BS
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Data.Proxy            (Proxy(..))
import           Data.Word

dataField
    :: forall b a sz.
    ( R.ReprFor b ~ 'R.Data sz
    , NC.IsWord (R.UntypedData sz)
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
    => F.Variant k a b -> R.Raw a mut -> m (R.Raw b mut)
readVariant F.Variant{field} = readField field

newStruct :: forall a m s. (U.RWCtx m s, NC.TypedStruct a) => () -> M.Message ('Mut s) -> m (R.Raw a ('Mut s))
newStruct () msg = R.Raw . R.fromRaw <$> NC.new @NB.AnyStruct (NC.numStructWords @a, NC.numStructPtrs @a) msg


parseEnum :: (R.ReprFor a ~ 'R.Data 'R.Sz16, Enum a, Applicative m)
    => R.Raw a 'Const -> m a
parseEnum (R.Raw n) = pure $ toEnum $ fromIntegral n

encodeEnum :: forall a m s. (R.ReprFor a ~ 'R.Data 'R.Sz16, Enum a, U.RWCtx m s)
    => M.Message ('Mut s) -> a -> m (R.Raw a ('Mut s))
encodeEnum _msg value = pure $ R.Raw $ fromIntegral $ fromEnum @a value

-- | Get a pointer from a ByteString, where the root object is a struct with
-- one pointer, which is the pointer we will retrieve. This is only safe for
-- trusted inputs; it reads the message with a traversal limit of 'maxBound'
-- (and so is suseptable to denial of service attacks), and it calls 'error'
-- if decoding is not successful.
--
-- The purpose of this is for defining constants of pointer type from a schema.
getPtrConst :: forall a. R.IsPtr a => BS.ByteString -> R.Raw a 'Const
getPtrConst bytes = fromJust $ evalLimitT maxBound $ do
    R.Raw root <- bsToRaw @NB.AnyStruct bytes
    U.getPtr 0 root
        >>= R.fromPtr @(R.PtrReprFor (R.ReprFor a)) (U.message root)
        <&> R.Raw
