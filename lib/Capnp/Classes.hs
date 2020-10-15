{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{- |
Module: Capnp.Classes
Description: Misc. type classes

This module defines several type classes concerning encoding and decoding
values in the capnproto wire format (as well as instances for some basic
types).

Note that much of this is unlikely to be used directly by developers.
Typically these are either used internally by generated code, or
transitively via higher level functions in the API. It is recommended
to look elsewhere in the library for what you need, and refer to this
module only when trying to understand what the class constraints on a
function mean.
-}
module Capnp.Classes
    ( IsWord(..)
    , ListElem(..)
    , MutListElem(..)
    , FromPtr(..)
    , ToPtr(..)
    , FromStruct(..)
    , ToStruct(..)
    , Allocate(..)
    , Marshal(..)
    , Cerialize(..)
    , Decerialize(..)
    , cerializeBasicVec
    , cerializeCompositeVec
    ) where

import Prelude hiding (length)

import Data.Bits
import Data.Int
import Data.ReinterpretCast
import Data.Word

import Control.Monad.Catch (MonadThrow(throwM))
import Data.Foldable       (for_)

import Capnp.Bits    (Word1 (..))
import Capnp.Errors  (Error(SchemaViolationError))
import Capnp.Untyped (Cap, ListOf, Ptr (..), ReadCtx, Struct, messageDefault)

import qualified Capnp.Message as M
import qualified Capnp.Untyped as U

import qualified Data.Vector as V

-- | Types that can be converted to and from a 64-bit word.
--
-- Anything that goes in the data section of a struct will have
-- an instance of this.
class IsWord a where
    -- | Convert from a 64-bit words Truncates the word if the
    -- type has less than 64 bits.
    fromWord :: Word64 -> a

    -- | Convert to a 64-bit word.
    toWord :: a -> Word64

-- | Types which may be stored as an element of a capnproto list.
class ListElem msg e where
    -- | The type of lists of @e@ stored in messages of type @msg@
    data List msg e

    -- | Convert an untyped list to a list of this type. May fail
    -- with a 'SchemaViolationError' if the list does not have the
    -- correct representation.
    --
    -- TODO: this is basically just fromPtr; refactor so this is less
    -- redundant.
    listFromPtr :: U.ReadCtx m msg => msg -> Maybe (U.Ptr msg) -> m (List msg e)

    toUntypedList :: List msg e -> U.List msg

    -- | Get the length of a list.
    length :: List msg e -> Int

    -- | @'index' i list@ gets the @i@th element of a list.
    index :: U.ReadCtx m msg => Int -> List msg e -> m e

-- | Types which may be stored as an element of a *mutable* capnproto list.
class (ListElem (M.MutMsg s) e) => MutListElem s e where
    -- | @'setIndex' value i list@ sets the @i@th index in @list@ to @value@
    setIndex :: U.RWCtx m s => e -> Int -> List (M.MutMsg s) e -> m ()

    -- | @'newList' msg size@ allocates and returns a new list of length
    -- @size@ inside @msg@.
    newList :: M.WriteCtx m s => M.MutMsg s -> Int -> m (List (M.MutMsg s) e)

-- | Types which may be stored in a capnproto message, and have a fixed size.
--
-- This applies to typed structs, but not e.g. lists, because the length
-- must be known to allocate a list.
class Allocate s e where
    -- @'new' msg@ allocates a new value of type @e@ inside @msg@.
    new :: M.WriteCtx m s => M.MutMsg s -> m e

-- | Types which may be extracted from a message.
--
-- typically, instances of 'Decerialize' will be the algebraic data types
-- defined in generated code for the high-level API.
class Decerialize a where
    -- | A variation on @a@ which is encoded in the message.
    --
    -- For the case of instances in generated high-level API code, this will
    -- be the low-level API analouge of the type.
    type Cerial msg a

    -- | Extract the value from the message.
    decerialize :: U.ReadCtx m M.ConstMsg => Cerial M.ConstMsg a -> m a

-- | Types which may be marshaled into a pre-allocated object in a message.
class Decerialize a => Marshal a where

    -- | Marshal a value into the pre-allocated object inside the message.
    --
    -- Note that caller must arrange for the object to be of the correct size.
    -- This is is not necessarily guaranteed; for example, list types must
    -- coordinate the length of the list.
    marshalInto :: U.RWCtx m s => Cerial (M.MutMsg s) a -> a -> m ()

-- | Types which may be inserted into a message.
class Decerialize a => Cerialize a where

    -- | Cerialize a value into the supplied message, returning the result.
    cerialize :: U.RWCtx m s => M.MutMsg s -> a -> m (Cerial (M.MutMsg s) a)

    default cerialize :: (U.RWCtx m s, Marshal a, Allocate s (Cerial (M.MutMsg s) a))
        => M.MutMsg s -> a -> m (Cerial (M.MutMsg s) a)
    cerialize msg value = do
        raw <- new msg
        marshalInto raw value
        pure raw

-- | Types that can be converted from an untyped pointer.
--
-- Note that decoding do not have to succeed, if the pointer is
-- the wrong type.
class FromPtr msg a where
    -- | Convert an untyped pointer to an @a@.
    fromPtr :: ReadCtx m msg => msg -> Maybe (Ptr msg) -> m a

-- | Types that can be converted to an untyped pointer.
class ToPtr s a where
    -- | Convert an @a@ to an untyped pointer.
    toPtr :: M.WriteCtx m s => M.MutMsg s -> a -> m (Maybe (Ptr (M.MutMsg s)))

-- | Types that can be extracted from a struct.
class FromStruct msg a where
    -- | Extract a value from a struct.
    fromStruct :: ReadCtx m msg => Struct msg -> m a

-- | Types that can be converted to a struct.
class ToStruct msg a where
    -- | Convert a value to a struct.
    toStruct :: a -> Struct msg

------- instances -------

instance IsWord Bool where
    fromWord n = (n .&. 1) == 1
    toWord True  = 1
    toWord False = 0

instance IsWord Word1 where
    fromWord = Word1 . fromWord
    toWord = toWord . word1ToBool

-- IsWord instances for integral types; they're all the same.
instance IsWord Int8 where
    fromWord = fromIntegral
    toWord = fromIntegral
instance IsWord Int16 where
    fromWord = fromIntegral
    toWord = fromIntegral
instance IsWord Int32 where
    fromWord = fromIntegral
    toWord = fromIntegral
instance IsWord Int64 where
    fromWord = fromIntegral
    toWord = fromIntegral
instance IsWord Word8 where
    fromWord = fromIntegral
    toWord = fromIntegral
instance IsWord Word16 where
    fromWord = fromIntegral
    toWord = fromIntegral
instance IsWord Word32 where
    fromWord = fromIntegral
    toWord = fromIntegral
instance IsWord Word64 where
    fromWord = fromIntegral
    toWord = fromIntegral

instance IsWord Float where
    fromWord = wordToFloat . fromIntegral
    toWord = fromIntegral . floatToWord
instance IsWord Double where
    fromWord = wordToDouble
    toWord = doubleToWord

-- helper function for throwing SchemaViolationError "expected ..."
expected :: MonadThrow m => String -> m a
expected msg = throwM $ SchemaViolationError $ "expected " ++ msg

-- To/FromPtr instance for lists of Void/().
instance FromPtr msg (ListOf msg ()) where
    fromPtr msg Nothing                         = pure $ messageDefault msg
    fromPtr _ (Just (PtrList (U.List0 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 0"
instance ToPtr s (ListOf (M.MutMsg s) ()) where
    toPtr _ = pure . Just . PtrList . U.List0

-- To/FromPtr instances for lists of unsigned integers.
instance FromPtr msg (ListOf msg Word8) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr _ (Just (PtrList (U.List8 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 8"
instance ToPtr s (ListOf (M.MutMsg s) Word8) where
    toPtr _ = pure . Just . PtrList . U.List8
instance FromPtr msg (ListOf msg Word16) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr _ (Just (PtrList (U.List16 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 16"
instance ToPtr s (ListOf (M.MutMsg s) Word16) where
    toPtr _ = pure . Just . PtrList . U.List16
instance FromPtr msg (ListOf msg Word32) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr _ (Just (PtrList (U.List32 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 32"
instance ToPtr s (ListOf (M.MutMsg s) Word32) where
    toPtr _ = pure . Just . PtrList . U.List32
instance FromPtr msg (ListOf msg Word64) where
    fromPtr msg Nothing                       = pure $ messageDefault msg
    fromPtr _ (Just (PtrList (U.List64 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 64"
instance ToPtr s (ListOf (M.MutMsg s) Word64) where
    toPtr _ = pure . Just . PtrList . U.List64

instance FromPtr msg (ListOf msg Bool) where
    fromPtr msg Nothing = pure $ messageDefault msg
    fromPtr _ (Just (PtrList (U.List1 list))) = pure list
    fromPtr _ _ = expected "pointer to list with element size 1."
instance ToPtr s (ListOf (M.MutMsg s) Bool) where
    toPtr _ = pure . Just . PtrList . U.List1

-- To/FromPtr instance for pointers -- this is just the identity.
instance FromPtr msg (Maybe (Ptr msg)) where
    fromPtr _ = pure
instance ToPtr s (Maybe (Ptr (M.MutMsg s))) where
    toPtr _ = pure

-- To/FromPtr instance for composite lists.
instance FromPtr msg (ListOf msg (Struct msg)) where
    fromPtr msg Nothing                            = pure $ messageDefault msg
    fromPtr _ (Just (PtrList (U.ListStruct list))) = pure list
    fromPtr _ _ = expected "pointer to list of structs"
instance ToPtr s (ListOf (M.MutMsg s) (Struct (M.MutMsg s))) where
    toPtr _ = pure . Just . PtrList . U.ListStruct

-- To/FromPtr instance for lists of pointers.
instance FromPtr msg (ListOf msg (Maybe (Ptr msg))) where
    fromPtr msg Nothing                           = pure $ messageDefault msg
    fromPtr _ (Just (PtrList (U.ListPtr list))) = pure list
    fromPtr _ _ = expected "pointer to list of pointers"
instance ToPtr s (ListOf (M.MutMsg s) (Maybe (Ptr (M.MutMsg s)))) where
    toPtr _ = pure . Just . PtrList . U.ListPtr

-- To/FromPtr instance for *typed* lists.
instance ListElem msg e => FromPtr msg (List msg e) where
    fromPtr = listFromPtr
instance ListElem (M.MutMsg s) e => ToPtr s (List (M.MutMsg s) e) where
    toPtr _ = pure . Just . PtrList . toUntypedList

-- ListElem instance for (typed) nested lists.
instance ListElem msg e => ListElem msg (List msg e) where
    newtype List msg (List msg e) = NestedList (U.ListOf msg (Maybe (U.Ptr msg)))

    listFromPtr msg ptr = NestedList <$> fromPtr msg ptr
    toUntypedList (NestedList l) = U.ListPtr l

    length (NestedList l) = U.length l
    index i (NestedList l) = do
        ptr <- U.index i l
        fromPtr (U.message l) ptr

instance MutListElem s e => MutListElem s (List (M.MutMsg s) e) where
    setIndex e i (NestedList l) = U.setIndex (Just (U.PtrList (toUntypedList e))) i l
    newList msg len = NestedList <$> U.allocListPtr msg len

-- FromStruct instance for Struct; just the identity.
instance FromStruct msg (Struct msg) where
    fromStruct = pure

instance ToStruct msg (Struct msg) where
    toStruct = id

instance FromPtr msg (Struct msg) where
    fromPtr msg Nothing              = fromStruct (go msg) where
        -- the type checker needs a bit of help inferring the type here.
        go :: msg -> Struct msg
        go = messageDefault
    fromPtr _ (Just (PtrStruct s)) = fromStruct s
    fromPtr _ _                      = expected "pointer to struct"
instance ToPtr s (Struct (M.MutMsg s)) where
    toPtr _ = pure . Just . PtrStruct

instance FromPtr msg (Maybe (Cap msg)) where
    fromPtr _ Nothing             = pure Nothing
    fromPtr _ (Just (PtrCap cap)) = pure (Just cap)
    fromPtr _ _                   = expected "pointer to capability"
instance ToPtr s (Maybe (Cap (M.MutMsg s))) where
    toPtr _ = pure . fmap PtrCap

-- | A valid implementation of 'cerialize', which just cerializes the
-- elements of a list individually and puts them in the list.
--
-- Note that while this is *correct* for composite lists, it is inefficient,
-- since it will separately allocate the elements and then copy them into
-- the list, doing extra work and leaking space. See 'cerializeCompositeVec'.
cerializeBasicVec ::
    ( U.RWCtx m s
    , MutListElem s (Cerial (M.MutMsg s) a)
    , Cerialize a
    )
    => M.MutMsg s
    -> V.Vector a
    -> m (List (M.MutMsg s) (Cerial (M.MutMsg s) a))
cerializeBasicVec msg vec = do
    list <- newList msg (V.length vec)
    for_ [0..V.length vec - 1] $ \i -> do
        e <- cerialize msg (vec V.! i)
        setIndex e i list
    pure list

-- | A valid implementation of 'cerialize', which allocates a list of the
-- correct size and then marshals the elements of a vector into the elements
-- of the list. This is more efficient for composite types than
-- 'cerializeBasicVec', hence the name.
cerializeCompositeVec ::
    ( U.RWCtx m s
    , MutListElem s (Cerial (M.MutMsg s) a)
    , Marshal a
    )
    => M.MutMsg s
    -> V.Vector a
    -> m (List (M.MutMsg s) (Cerial (M.MutMsg s) a))
cerializeCompositeVec msg vec = do
    list <- newList msg (V.length vec)
    for_ [0..V.length vec - 1] $ \i -> do
        targ <- index i list
        marshalInto targ (vec V.! i)
    pure list

-- Generic decerialize instances for lists, given that the element type has an instance.
instance
    ( ListElem M.ConstMsg (Cerial M.ConstMsg a)
    , Decerialize a
    ) => Decerialize (V.Vector a)
  where
    type Cerial msg (V.Vector a) = List msg (Cerial msg a)
    decerialize raw = V.generateM (length raw) (\i -> index i raw >>= decerialize)
