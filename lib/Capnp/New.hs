{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Capnp.New
    ( readField
    , getField
    ) where


import qualified Capnp.Classes        as C
import qualified Capnp.Fields         as F
import           Capnp.Message        (Mutability(..))
import qualified Capnp.Repr           as R
import           Capnp.TraversalLimit (evalLimitT)
import qualified Capnp.Untyped        as U
import           Data.Bits
import           Data.Maybe           (fromJust)

{-# INLINE readField #-}
readField
    ::  forall a b mut m.
        ( R.ReprFor a ~ 'R.Ptr ('Just 'R.Struct)
        , U.ReadCtx m mut
        )
    => F.Field a b
    -> R.Raw mut a
    -> m (R.Raw mut b)
readField (F.Field field) (R.Raw struct) =
    case field of
        F.DataField F.DataFieldLoc{ shift, index, mask, defaultValue } -> do
            word <- U.getData (fromIntegral index) struct
            pure $ R.Raw $ C.fromWord $ ((word `shiftR` fromIntegral shift) .&. mask) `xor` defaultValue
        F.PtrField index ->
            U.getPtr (fromIntegral index) struct >>= readPtrField
        F.GroupField ->
            pure $ R.Raw struct
        F.VoidField ->
            pure $ R.Raw ()
  where
    -- This is broken out because the type checker needs some extra help:
    readPtrField
        :: forall pr.
        ( R.ReprFor b ~ 'R.Ptr pr
        , R.IsPtrRepr pr
        ) => Maybe (U.Ptr mut) -> m (R.Raw mut b)
    readPtrField ptr =
        R.Raw <$> R.rFromPtr @pr (U.message struct) ptr


{-# INLINE getField #-}
getField
    ::  ( R.ReprFor a ~ 'R.Ptr ('Just 'R.Struct)
        , R.ReprFor b ~ 'R.Data sz
        )
    => F.Field a b
    -> R.Raw 'Const a
    -> R.Raw 'Const b
getField field struct =
    fromJust $ evalLimitT maxBound $ readField field struct
