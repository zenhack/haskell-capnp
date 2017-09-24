{-| Module: Data.CapNProto.BasicTypes
    Description: Handling of "built-in" capnp datatypes.

    In particular, things that are primitive types in the schema, language,
    but not on the wire (chiefly Data and Text, which are both just lists of
    bytes).
-}
module Data.CapNProto.BasicTypes
    ( Text(..)
    , Data(..)
    , getData
    , getText
    )
  where

import Data.Word

import Control.Monad       (when)
import Control.Monad.Catch (throwM)

import qualified Data.CapNProto.Blob    as B
import qualified Data.CapNProto.Errors  as E
import qualified Data.CapNProto.Untyped as U

-- | A textual string ("Text" in capnproto's schema language). On the wire,
-- this is NUL-terminated. The encoding should be UTF-8, but the library *does
-- not* verify this; users of the library must do validation themselves, if
-- they care about this.
--
-- The argument to the constructor is the slice of the original message
-- containing the text (but not the NUL terminator).
newtype Text b = Text b

-- | A blob of bytes ("Data" in capnproto's schema language). The argument
-- to the constructor is a slice into the message, containing the raw bytes.
newtype Data b = Data b

-- | Interpret a list of Word8 as a capnproto 'Data' value. This validates that
-- the bytes are in-bounds, throwing a 'BoundsError' if not.
getData :: (U.ReadCtx m b, B.Slice m b) => U.ListOf b Word8 -> m (Data b)
getData list = Data <$> U.rawBytes list

-- | Interpret a list of Word8 as a capnproto 'Text' value.
--
-- This vaidates that the list is in-bounds, and that it is NUL-terminated,
-- but not that it is valid UTF-8. If it is not NUL-terminaed, a x
-- 'SchemaViolationError' is thrown.
getText :: (U.ReadCtx m b, B.Slice m b) => U.ListOf b Word8 -> m (Text b)
getText list = do
    bytes <- U.rawBytes list
    len <- B.length bytes
    when (len == 0) $ throwM $ E.SchemaViolationError
        "Text is not NUL-terminated (list of bytes has length 0)"
    lastByte <- B.index bytes (len - 1)
    when (lastByte /= 0) $ throwM $ E.SchemaViolationError $
        "Text is not NUL-terminated (last byte is " ++ show lastByte ++ ")"
    Text <$> B.slice bytes 0 (len - 1)
