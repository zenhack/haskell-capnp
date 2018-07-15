{-|
Module: Data.Capnp.Basics
Description: Handling of "basic" capnp datatypes.

In particular

* Text and Data (which are primitive types in the schema language,
  but are both the same as List(uint8) on the wire).
* List of types other than those in Data.Capnp.Untyped.
  Whereas 'U.ListOf' only deals with low-level encodings of lists,
  this module's 'List' type can represent typed lists.
-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.Capnp.Basics
    ( Text(..)
    , Data(..)
    , ListElem(..)
    , MutListElem(..)
    , getData
    , getText
    , dataBytes
    , textBytes
    ) where

import Data.Word

import Control.Monad       (when)
import Control.Monad.Catch (MonadThrow(throwM))

import Codec.Capnp           (IsPtr(..), ListElem(..), MutListElem(..))
import Internal.Gen.ListElem ()

import qualified Data.ByteString    as BS
import qualified Data.Capnp.Errors  as E
import qualified Data.Capnp.Untyped as U


-- | A textual string (@Text@ in capnproto's schema language). On the wire,
-- this is NUL-terminated. The encoding should be UTF-8, but the library
-- /does not/ verify this; users of the library must do validation themselves, if
-- they care about this.
--
-- Rationale: validation would require doing an up-front pass over the data,
-- which runs counter to the overall design of capnproto.
--
-- The argument to the data constructor is the slice of the original message
-- containing the text, excluding the NUL terminator.
newtype Text msg = Text (U.ListOf msg Word8)

-- | A blob of bytes (@Data@ in capnproto's schema language). The argument
-- to the data constructor is a slice into the message, containing the raw
-- bytes.
newtype Data msg = Data (U.ListOf msg Word8)

-- | Interpret a list of 'Word8' as a capnproto 'Data' value.
getData :: U.ReadCtx m msg => U.ListOf msg Word8 -> m (Data msg)
getData = pure . Data

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

-- | Convert a 'Text' to a 'BS.ByteString', comprising the raw bytes of the text
-- (not counting the NUL terminator).
textBytes :: U.ReadCtx m msg => Text msg -> m BS.ByteString
textBytes (Text list) = U.rawBytes list

-- IsPtr instances for Text and Data. These wrap lists of bytes.
instance IsPtr msg (Data msg) where
    fromPtr msg ptr = fromPtr msg ptr >>= getData
    toPtr (Data l) = toPtr l
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
    toPtr (Text l) = toPtr l
