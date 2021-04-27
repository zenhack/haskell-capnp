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
    , encodeField
    , parseField
    , setVariant
    , initVariant
    , encodeVariant
    , which
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
    ::  forall k a b mut m.
        ( R.IsStruct a
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
    ::  ( R.IsStruct a
        , R.ReprFor b ~ 'R.Data sz
        , C.Parse b bp
        )
    => F.Field 'F.Slot a b
    -> R.Raw 'Const a
    -> bp
getField field struct =
    fromJust $ evalLimitT maxBound $
        readField field struct >>= C.parse

{-# INLINE setField #-}
setField ::
    forall a b m s.
    ( R.IsStruct a
    , U.RWCtx m s
    ) => F.Field 'F.Slot a b -> R.Raw ('Mut s) b -> R.Raw ('Mut s) a -> m ()
setField (F.Field field) (R.Raw value) (R.Raw struct) =
    case field of
        F.DataField fieldLoc ->
            setDataField fieldLoc
        F.PtrField index ->
            setPtrField index value struct
        F.VoidField ->
            pure ()
  where
    -- This was originally broken out because the type checker needs some extra
    -- help, but it's probably more readable this way anyway.
    setPtrField
        :: forall pr.
        ( R.ReprFor b ~ 'R.Ptr pr
        , R.IsPtrRepr pr
        ) => Word16 -> R.UntypedPtr ('Mut s) pr -> U.Struct ('Mut s) -> m ()
    setPtrField index value struct =
        U.setPtr (R.rToPtr @pr (U.message struct) value) (fromIntegral index) struct

    setDataField
        :: forall sz.
        ( R.ReprFor b ~ 'R.Data sz
        , C.IsWord (R.UntypedData sz)
        ) => F.DataFieldLoc sz -> m ()
    setDataField F.DataFieldLoc{ shift, index, mask, defaultValue } = do
        oldWord <- U.getData (fromIntegral index) struct
        let valueWord = (C.toWord value `xor` defaultValue) `shiftL` fromIntegral shift
            shiftedMask = mask `shiftL` fromIntegral shift
            newWord = (oldWord .&. complement shiftedMask) .|. valueWord
        U.setData newWord (fromIntegral index) struct

encodeField ::
    forall a b m s bp.
    ( R.IsStruct a
    , C.Parse b bp
    , U.RWCtx m s
    ) => F.Field 'F.Slot a b -> bp -> R.Raw ('Mut s) a -> m ()
encodeField field parsed struct = do
    encoded <- C.encode (U.message struct) parsed
    setField field encoded struct

parseField ::
    ( R.IsStruct a
    , C.Parse b bp
    , U.ReadCtx m 'Const
    ) => F.Field k a b -> R.Raw 'Const a -> m bp
parseField field raw =
    readField field raw >>= C.parse

setVariant
    :: forall a b m s.
    ( F.HasUnion a
    , U.RWCtx m s
    ) => F.Variant 'F.Slot a b -> R.Raw ('Mut s) a -> R.Raw ('Mut s) b -> m ()
setVariant F.Variant{field, tagValue} struct value = do
    setField (F.unionField @a) (R.Raw tagValue) struct
    setField field value struct

encodeVariant
    :: forall a b m s bp.
    ( F.HasUnion a
    , C.Parse b bp
    , U.RWCtx m s
    ) => F.Variant 'F.Slot a b -> bp -> R.Raw ('Mut s) a -> m ()
encodeVariant F.Variant{field, tagValue} value struct = do
    setField (F.unionField @a) (R.Raw tagValue) struct
    encodeField field value struct

initVariant
    :: forall a b m s. (F.HasUnion a, U.RWCtx m s)
    => F.Variant 'F.Group a b -> R.Raw ('Mut s) a -> m (R.Raw ('Mut s) b)
initVariant F.Variant{field, tagValue} struct = do
    setField (F.unionField @a) (R.Raw tagValue) struct
    readField field struct

which :: forall a mut m. U.ReadCtx m mut => F.HasUnion a => R.Raw mut a -> m (F.RawWhich mut a)
which struct = do
    R.Raw tagValue <- readField (F.unionField @a) struct
    F.internalWhich tagValue struct
