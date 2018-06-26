{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-| Module: Data.Capnp.BuiltinTypes.Generic
    Description: Handling of "built-in" capnp datatypes.

    In particular

    * Text and Data (which are primitive types in the schema language,
      but are both the same as List(uint8) on the wire).
    * List of types other than those in 'Data.Capnp.Untyped.Generic'.
      Whereas 'GU.ListOf' only deals with low-level encodings of lists,
      this modules 'List' type can represent typed lists.
-}
module Data.Capnp.BuiltinTypes.Generic
    ( Text(..)
    , Data(..)
    , ListElem(..)
    , getData
    , getText
    , dataBytes
    , textBytes
    ) where

import Data.Word

import Control.Monad       (when)
import Control.Monad.Catch (MonadThrow(throwM))

import Codec.Capnp                   (ListElem(..))
import Data.Capnp.BuiltinTypes.Lists ()

import qualified Data.ByteString            as BS
import qualified Data.Capnp.Errors          as E
import qualified Data.Capnp.Message.Generic as GM
import qualified Data.Capnp.Untyped.Generic as GU


-- | A textual string ("Text" in capnproto's schema language). On the wire,
-- this is NUL-terminated. The encoding should be UTF-8, but the library *does
-- not* verify this; users of the library must do validation themselves, if
-- they care about this.
--
-- Rationale: validation would require doing an up-front pass over the data,
-- which runs counter to the overall design of capnproto.
--
-- The argument to the data constructor is the slice of the original message
-- containing the text, excluding the NUL terminator.
newtype Text msg = Text (GU.ListOf msg Word8)

-- | A blob of bytes ("Data" in capnproto's schema language). The argument
-- to the data constructor is a slice into the message, containing the raw
-- bytes.
newtype Data msg = Data (GU.ListOf msg Word8)

-- | Interpret a list of Word8 as a capnproto 'Data' value.
getData :: (GM.Message m msg, GU.ReadCtx m) => GU.ListOf msg Word8 -> m (Data msg)
getData = pure . Data

-- | Interpret a list of Word8 as a capnproto 'Text' value.
--
-- This vaildates that the list is NUL-terminated, but not that it is valid
-- UTF-8. If it is not NUL-terminaed, a 'SchemaViolationError' is thrown.
getText :: (GM.Message m msg, GU.ReadCtx m) => GU.ListOf msg Word8 -> m (Text msg)
getText list = do
    let len = GU.length list
    when (len == 0) $ throwM $ E.SchemaViolationError
        "Text is not NUL-terminated (list of bytes has length 0)"
    lastByte <- GU.index (len - 1) list
    when (lastByte /= 0) $ throwM $ E.SchemaViolationError $
        "Text is not NUL-terminated (last byte is " ++ show lastByte ++ ")"
    Text <$> GU.take (len - 1) list

-- | Convert a 'Data' to a 'BS.ByteString.
dataBytes :: (GM.Message m msg, GU.ReadCtx m) => Data msg -> m BS.ByteString
dataBytes (Data list) = GU.rawBytes list

-- | Convert a 'Text' to a 'BS.ByteString', comprising the raw bytes of the text
-- (not counting the NUL terminator).
textBytes :: (GM.Message m msg, GU.ReadCtx m) => Text msg -> m BS.ByteString
textBytes (Text list) = GU.rawBytes list
