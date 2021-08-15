{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Module: Capnp.New.Accessors
-- Description: Functions for accessing parts of messaages.
module Capnp.New.Accessors
    ( readField
    , getField
    , setField
    , newField
    , hasField
    , encodeField
    , parseField
    , setVariant
    , initVariant
    , encodeVariant
    , structWhich
    , unionWhich
    , structUnion
    , unionStruct
    ) where


import qualified Capnp.Fields         as F
import           Capnp.Message        (Mutability(..))
import qualified Capnp.New.Classes    as C
import qualified Capnp.Repr           as R
import           Capnp.TraversalLimit (evalLimitT)
import qualified Capnp.Untyped        as U
import           Data.Bits
import           Data.Maybe           (fromJust, isJust)
import           Data.Word
import           GHC.Prim             (coerce)

{-# INLINE readField #-}
-- | Read the value of a field of a struct.
readField
    ::  forall k a b mut m.
        ( R.IsStruct a
        , U.ReadCtx m mut
        )
    => F.Field k a b
    -> R.Raw a mut
    -> m (R.Raw b mut)
readField (F.Field field) (R.Raw struct) =
    case field of
        F.DataField F.DataFieldLoc{ shift, index, mask, defaultValue } -> do
            word <- U.getData (fromIntegral index) struct
            pure $ R.Raw $ C.fromWord $ ((word .&. mask) `shiftR` fromIntegral shift) `xor` defaultValue
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
        ) => Maybe (U.Ptr mut) -> m (R.Raw b mut)
    readPtrField ptr =
        R.Raw <$> R.fromPtr @pr (U.message @U.Struct struct) ptr

-- | Return whether the specified field is present. Only applicable for pointer
-- fields.
hasField ::
    ( U.ReadCtx m mut
    , R.IsStruct a
    , R.IsPtr b
    ) => F.Field 'F.Slot a b -> R.Raw a mut -> m Bool
hasField (F.Field (F.PtrField index)) (R.Raw struct) =
    isJust <$> U.getPtr (fromIntegral index) struct

{-# INLINE getField #-}
-- | Like 'readField', but:
--
-- * Doesn't need the monadic context; can be used in pure code.
-- * Only works for immutable values.
-- * Only works for fields in the struct's data section.
getField
    ::  ( R.IsStruct a
        , R.ReprFor b ~ 'R.Data sz
        , C.Parse b bp
        )
    => F.Field 'F.Slot a b
    -> R.Raw a 'Const
    -> bp
getField field struct =
    fromJust $ evalLimitT maxBound $
        readField field struct >>= C.parse

{-# INLINE setField #-}
-- | Set a struct field to a value. Not usable for group fields.
setField ::
    forall a b m s.
    ( R.IsStruct a
    , U.RWCtx m s
    ) => F.Field 'F.Slot a b -> R.Raw b ('Mut s) -> R.Raw a ('Mut s) -> m ()
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
        ) => Word16 -> U.Unwrapped (R.UntypedPtr pr ('Mut s)) -> U.Struct ('Mut s) -> m ()
    setPtrField index value struct =
        U.setPtr (R.toPtr @pr value) (fromIntegral index) struct

    setDataField
        :: forall sz.
        ( R.ReprFor b ~ 'R.Data sz
        , C.IsWord (R.UntypedData sz)
        ) => F.DataFieldLoc sz -> m ()
    setDataField F.DataFieldLoc{ shift, index, mask, defaultValue } = do
        oldWord <- U.getData (fromIntegral index) struct
        let valueWord = C.toWord value `xor` defaultValue
            newWord = (oldWord .&. complement mask)
                  .|. (valueWord `shiftL` fromIntegral shift)
        U.setData newWord (fromIntegral index) struct

-- | Allocate space for the value of a field, and return it.
newField ::
    forall a b m s.
    ( R.IsStruct a
    , C.Allocate b
    , U.RWCtx m s
    ) => F.Field 'F.Slot a b -> C.AllocHint b -> R.Raw a ('Mut s) -> m (R.Raw b ('Mut s))
newField field hint parent = do
    value <- C.new @b hint (U.message @(R.Raw a) parent)
    setField field value parent
    pure value

-- | Marshal a parsed value into a struct's field.
encodeField ::
    forall a b m s bp.
    ( R.IsStruct a
    , C.Parse b bp
    , U.RWCtx m s
    ) => F.Field 'F.Slot a b -> bp -> R.Raw a ('Mut s) -> m ()
encodeField field parsed struct = do
    encoded <- C.encode (U.message @(R.Raw a) struct) parsed
    setField field encoded struct

-- | parse a struct's field and return its parsed form.
parseField ::
    ( R.IsStruct a
    , C.Parse b bp
    , U.ReadCtx m 'Const
    ) => F.Field k a b -> R.Raw a 'Const -> m bp
parseField field raw =
    readField field raw >>= C.parse

-- | Set the struct's anonymous union to the given variant, with the
-- supplied value as its argument. Not applicable for variants whose
-- argument is a group; use 'initVariant' instead.
setVariant
    :: forall a b m s.
    ( F.HasUnion a
    , U.RWCtx m s
    ) => F.Variant 'F.Slot a b -> R.Raw a ('Mut s) -> R.Raw b ('Mut s) -> m ()
setVariant F.Variant{field, tagValue} struct value = do
    setField (F.unionField @a) (R.Raw tagValue) struct
    setField field value struct

-- | Set the struct's anonymous union to the given variant, marshalling
-- the supplied value into the message to be its argument. Not applicable
-- for variants whose argument is a group; use 'initVariant' instead.
encodeVariant
    :: forall a b m s bp.
    ( F.HasUnion a
    , C.Parse b bp
    , U.RWCtx m s
    ) => F.Variant 'F.Slot a b -> bp -> R.Raw a ('Mut s) -> m ()
encodeVariant F.Variant{field, tagValue} value struct = do
    setField (F.unionField @a) (R.Raw tagValue) struct
    encodeField field value struct

-- | Set the struct's anonymous union to the given variant, returning
-- the variant's argument, which must be a group (for non-group fields,
-- use 'setVariant' or 'encodeVariant'.
initVariant
    :: forall a b m s. (F.HasUnion a, U.RWCtx m s)
    => F.Variant 'F.Group a b -> R.Raw a ('Mut s) -> m (R.Raw b ('Mut s))
initVariant F.Variant{field, tagValue} struct = do
    setField (F.unionField @a) (R.Raw tagValue) struct
    readField field struct

-- | Get the anonymous union for a struct.
structUnion :: F.HasUnion a => R.Raw a mut -> R.Raw (F.Which a) mut
structUnion = coerce

-- | Get the struct enclosing an anonymous union.
unionStruct :: F.HasUnion a => R.Raw (F.Which a) mut -> R.Raw a mut
unionStruct = coerce

-- | Get a non-opaque view on the struct's anonymous union, which
-- can be used to pattern match on.
structWhich :: forall a mut m. (U.ReadCtx m mut, F.HasUnion a) => R.Raw a mut -> m (F.RawWhich a mut)
structWhich struct = do
    R.Raw tagValue <- readField (F.unionField @a) struct
    F.internalWhich tagValue struct

-- | Get a non-opaque view on the anonymous union, which can be
-- used to pattern match on.
unionWhich :: forall a mut m. (U.ReadCtx m mut, F.HasUnion a) => R.Raw (F.Which a) mut -> m (F.RawWhich a mut)
unionWhich = structWhich . unionStruct
