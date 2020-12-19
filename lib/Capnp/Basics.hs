{-|
Module: Capnp.Basics
Description: Handling of "basic" capnp datatypes.

In particular

* 'Text' and 'Data' (which are primitive types in the schema language,
  but are both the same as @List(UInt8)@ on the wire).
* Lists of types other than those in "Capnp.Untyped".
  Whereas 'U.ListOf' only deals with low-level encodings of lists,
  this module's 'List' type can represent typed lists.
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Capnp.Basics
    ( Text
    , Data(..)
    , ListElem(..)
    , MutListElem(..)
    , getData
    , getText
    , newData
    , newText
    , dataBytes
    , textBuffer
    , textBytes
    ) where

import Data.Word

import Control.Monad       (when, (>=>))
import Control.Monad.Catch (MonadThrow(throwM))

import qualified Data.ByteString as BS

import Capnp.Classes
    (FromPtr (..), ListElem (..), MutListElem (..), ToPtr (..))
import Capnp.Message          (Mutability (..))
import Data.Mutable           (Thaw (..))
import Internal.Gen.Instances ()

import qualified Capnp.Errors  as E
import qualified Capnp.Message as M
import qualified Capnp.Untyped as U


-- | A textual string (@Text@ in capnproto's schema language). On the wire,
-- this is NUL-terminated. The encoding should be UTF-8, but the library
-- /does not/ verify this; users of the library must do validation themselves, if
-- they care about this.
--
-- Rationale: validation would require doing an up-front pass over the data,
-- which runs counter to the overall design of capnproto.
newtype Text mut = Text (U.ListOf mut Word8)
-- The argument to the data constructor is the slice of the original message
-- containing the text, including the NUL terminator.

instance Thaw (Text 'Const) where
    type Mutable s (Text 'Const) = Text ('Mut s)
    thaw (Text l) = Text <$> thaw l
    unsafeThaw (Text l) = Text <$> unsafeThaw l
    freeze (Text l) = Text <$> freeze l
    unsafeFreeze (Text l) = Text <$> unsafeFreeze l

-- | A blob of bytes (@Data@ in capnproto's schema language). The argument
-- to the data constructor is a slice into the message, containing the raw
-- bytes.
newtype Data mut = Data (U.ListOf mut Word8)

instance Thaw (Data 'Const) where
    type Mutable s (Data 'Const) = Data ('Mut s)
    thaw (Data l) = Data <$> thaw l
    unsafeThaw (Data l) = Data <$> unsafeThaw l
    freeze (Data l) = Data <$> freeze l
    unsafeFreeze (Data l) = Data <$> unsafeFreeze l

-- | @'newData' msg len@ allocates a new data blob of length @len@ bytes
-- inside the message.
newData :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (Data ('Mut s))
newData msg len = Data <$> U.allocList8 msg len

-- | Interpret a list of 'Word8' as a capnproto 'Data' value.
getData :: U.ReadCtx m mut => U.ListOf mut Word8 -> m (Data mut)
getData = pure . Data

-- | @'newText' msg len@ Allocates a new 'Text' inside the message. The
-- value has space for @len@ *bytes* (not characters).
newText :: M.WriteCtx m s => M.Message ('Mut s) -> Int -> m (Text ('Mut s))
newText msg len =
    Text <$> U.allocList8 msg (len+1)

-- | Interpret a list of 'Word8' as a capnproto 'Text' value.
--
-- This vaildates that the list is NUL-terminated, but not that it is valid
-- UTF-8. If it is not NUL-terminaed, a 'SchemaViolationError' is thrown.
getText :: U.ReadCtx m mut => U.ListOf mut Word8 -> m (Text mut)
getText list = do
    let len = U.length list
    when (len == 0) $ throwM $ E.SchemaViolationError
        "Text is not NUL-terminated (list of bytes has length 0)"
    lastByte <- U.index (len - 1) list
    when (lastByte /= 0) $ throwM $ E.SchemaViolationError $
        "Text is not NUL-terminated (last byte is " ++ show lastByte ++ ")"
    pure $ Text list

-- | Convert a 'Data' to a 'BS.ByteString'.
dataBytes :: U.ReadCtx m 'Const => Data 'Const -> m BS.ByteString
dataBytes (Data list) = U.rawBytes list

-- | Return the underlying buffer containing the text. This does not include the
-- null terminator.
textBuffer :: U.ReadCtx m mut => Text mut -> m (U.ListOf mut Word8)
textBuffer (Text list) = U.take (U.length list - 1) list

-- | Convert a 'Text' to a 'BS.ByteString', comprising the raw bytes of the text
-- (not counting the NUL terminator).
textBytes :: U.ReadCtx m 'Const => Text 'Const -> m BS.ByteString
textBytes = textBuffer >=> U.rawBytes

------------------- (Mut)ListElem instances for text and data ------------------

instance ListElem mut (Data mut) where
    newtype List mut (Data mut) = DataList (U.ListOf mut (Maybe (U.Ptr mut)))

    listFromPtr msg ptr = DataList <$> fromPtr msg ptr
    toUntypedList (DataList l) = U.ListPtr l

    length (DataList l) = U.length l
    index i (DataList l) = ptrListIndex i l

instance MutListElem s (Data ('Mut s)) where
    setIndex (Data e) i (DataList l) =
        U.setIndex (Just (U.PtrList (U.List8 e))) i l
    newList msg len = DataList <$> U.allocListPtr msg len

instance ListElem mut (Text mut) where
    newtype List mut (Text mut) = TextList (U.ListOf mut (Maybe (U.Ptr mut)))

    listFromPtr msg ptr = TextList <$> fromPtr msg ptr
    toUntypedList (TextList l) = U.ListPtr l

    length (TextList l) = U.length l
    index i (TextList l) = ptrListIndex i l

instance MutListElem s (Text ('Mut s)) where
    setIndex (Text e) i (TextList l) =
        U.setIndex (Just (U.PtrList (U.List8 e))) i l
    newList msg len = TextList <$> U.allocListPtr msg len

-- helper for the above instances.
ptrListIndex
    :: (U.ReadCtx m mut, FromPtr mut a)
    => Int -> U.ListOf mut (Maybe (U.Ptr mut)) -> m a
ptrListIndex i list = do
    ptr <- U.index i list
    fromPtr (U.message list) ptr

--------- To/FromPtr instances for Text and Data. These wrap lists of bytes. --------

instance FromPtr mut (Data mut) where
    fromPtr msg ptr = fromPtr msg ptr >>= getData
instance ToPtr s (Data ('Mut s)) where
    toPtr msg (Data l) = toPtr msg l

instance FromPtr mut (Text mut) where
    fromPtr msg ptr = case ptr of
        Just _ ->
            fromPtr msg ptr >>= getText
        Nothing -> do
            -- getText expects and strips off a NUL byte at the end of the
            -- string. In the case of a null pointer we just want to return
            -- the empty string, so we bypass it here.
            Data bytes <- fromPtr msg ptr
            pure $ Text bytes
instance ToPtr s (Text ('Mut s)) where
    toPtr msg (Text l) = toPtr msg l
