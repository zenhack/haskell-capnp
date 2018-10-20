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

import Control.Monad       (when)
import Control.Monad.Catch (MonadThrow(throwM))

import qualified Data.ByteString as BS

import Capnp.Classes     (IsPtr(..), ListElem(..), MutListElem(..))
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
newtype Text msg = Text (U.ListOf msg Word8)
-- The argument to the data constructor is the slice of the original message
-- containing the text, including the NUL terminator.

-- | A blob of bytes (@Data@ in capnproto's schema language). The argument
-- to the data constructor is a slice into the message, containing the raw
-- bytes.
newtype Data msg = Data (U.ListOf msg Word8)

-- | @'newData' msg len@ allocates a new data blob of length @len@ bytes
-- inside the message.
newData :: M.WriteCtx m s => M.MutMsg s -> Int -> m (Data (M.MutMsg s))
newData msg len = Data <$> U.allocList8 msg len

-- | Interpret a list of 'Word8' as a capnproto 'Data' value.
getData :: U.ReadCtx m msg => U.ListOf msg Word8 -> m (Data msg)
getData = pure . Data

-- | @'newText' msg len@ Allocates a new 'Text' inside the message. The
-- value has space for @len@ *bytes* (not characters).
newText :: M.WriteCtx m s => M.MutMsg s -> Int -> m (Text (M.MutMsg s))
newText msg len =
    Text <$> U.allocList8 msg (len+1)

-- | Interpret a list of 'Word8' as a capnproto 'Text' value.
--
-- This vaildates that the list is NUL-terminated, but not that it is valid
-- UTF-8. If it is not NUL-terminaed, a 'SchemaViolationError' is thrown.
getText :: U.ReadCtx m msg => U.ListOf msg Word8 -> m (Text msg)
getText list = do
    let len = U.length list
    when (len == 0) $ throwM $ E.SchemaViolationError
        "Text is not NUL-terminated (list of bytes has length 0)"
    lastByte <- U.index (len - 1) list
    when (lastByte /= 0) $ throwM $ E.SchemaViolationError $
        "Text is not NUL-terminated (last byte is " ++ show lastByte ++ ")"
    Text <$> U.take (len - 1) list

-- | Convert a 'Data' to a 'BS.ByteString'.
dataBytes :: U.ReadCtx m msg => Data msg -> m BS.ByteString
dataBytes (Data list) = U.rawBytes list

-- | Return the underlying buffer containing the text. This does not include the
-- null terminator.
textBuffer :: U.ReadCtx m msg => Text msg -> m (U.ListOf msg Word8)
textBuffer (Text list) = U.take (U.length list - 1) list

-- | Convert a 'Text' to a 'BS.ByteString', comprising the raw bytes of the text
-- (not counting the NUL terminator).
textBytes :: U.ReadCtx m msg => Text msg -> m BS.ByteString
textBytes (Text list) = U.rawBytes list

-- IsPtr instances for Text and Data. These wrap lists of bytes.
instance IsPtr msg (Data msg) where
    fromPtr msg ptr = fromPtr msg ptr >>= getData
    toPtr msg (Data l) = toPtr msg l
instance IsPtr msg (Text msg) where
    fromPtr msg ptr = case ptr of
        Just _ ->
            fromPtr msg ptr >>= getText
        Nothing -> do
            -- getText expects and strips off a NUL byte at the end of the
            -- string. In the case of a null pointer we just want to return
            -- the empty string, so we bypass it here.
            Data bytes <- fromPtr msg ptr
            pure $ Text bytes
    toPtr msg (Text l) = toPtr msg l
