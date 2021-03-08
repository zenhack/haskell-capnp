{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Capnp.New
    ( readField
    , getField
    , setField
    , setVariant
    , initVariant
    ) where


import qualified Capnp.Classes        as C
import qualified Capnp.Fields         as F
import           Capnp.Message        (Mutability(..))
import qualified Capnp.New.Classes    as C
import qualified Capnp.Repr           as R
import           Capnp.TraversalLimit (evalLimitT)
import qualified Capnp.Untyped        as U
import           Data.Bits
import           Data.Maybe           (fromJust)
import           Data.Word

{-# INLINE readField #-}
readField
    ::  forall a b mut m k.
        ( R.ReprFor a ~ 'R.Ptr ('Just 'R.Struct)
        , U.ReadCtx m mut
        )
    => F.Field k a b
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
        , C.Parse b
        )
    => F.Field 'F.Slot a b
    -> R.Raw 'Const a
    -> C.Parsed b
getField field struct =
    fromJust $ evalLimitT maxBound $
        readField field struct >>= C.parseConst

{-# INLINE setField #-}
setField ::
    forall a b m s.
    ( R.ReprFor a ~ 'R.Ptr ('Just 'R.Struct)
    , U.RWCtx m s
    ) => F.Field 'F.Slot a b -> R.Raw ('Mut s) b -> R.Raw ('Mut s) a -> m ()
setField (F.Field field) (R.Raw value) (R.Raw struct) =
    case field of
        F.DataField F.DataFieldLoc{ shift, index, mask, defaultValue } -> do
            oldWord <- U.getData (fromIntegral index) struct
            let valueWord = (C.toWord value `xor` defaultValue) `shiftL` fromIntegral shift
            let newWord = (oldWord .&. complement mask) .|. valueWord
            U.setData newWord (fromIntegral index) struct
        F.PtrField index ->
            setPtrField index value struct
        F.VoidField ->
            pure ()
  where
    -- This is broken out because the type checker needs some extra help:
    setPtrField
        :: forall pr.
        ( R.ReprFor b ~ 'R.Ptr pr
        , R.IsPtrRepr pr
        ) => Word16 -> R.UntypedPtr ('Mut s) pr -> U.Struct ('Mut s) -> m ()
    setPtrField index value struct =
        U.setPtr (R.rToPtr @pr (U.message struct) value) (fromIntegral index) struct

setVariant
    :: forall a b m s.
    ( F.HasUnion a
    , U.RWCtx m s
    ) => F.Variant 'F.Slot a b -> R.Raw ('Mut s) a -> R.Raw ('Mut s) b -> m ()
setVariant F.Variant{field, tagValue} struct value = do
    setField (F.unionField @a) (R.Raw tagValue) struct
    setField field value struct

initVariant
    :: forall a b m s. (F.HasUnion a, U.RWCtx m s)
    => F.Variant 'F.Group a b -> R.Raw ('Mut s) a -> m (R.Raw ('Mut s) b)
initVariant F.Variant{field, tagValue} struct = do
    setField (F.unionField @a) (R.Raw tagValue) struct
    readField field struct
