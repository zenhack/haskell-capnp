{-| Module: Data.CapNProto.BasicTypes
    Description: Handling of "built-in" capnp datatypes.

    In particular, things that are primitive types in the schema, language,
    but not on the wire (chiefly Data and Text, which are both just lists of
    bytes).
-}
module Data.CapNProto.BasicTypes where

import Data.Word
import qualified Data.CapNProto.Untyped as U

-- | A textual string. On the wire, this is NUL-terminated. The encoding
-- should be UTF-8, but the library *does not* verify this; users of the
-- library must do validation themselves, if they care about this.
newtype Text b = Text (U.ListOf b Word8)

-- | A blob of bytes.
newtype Data b = Data (U.ListOf b Word8)
