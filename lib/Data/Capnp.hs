{- |
Module: Data.Capnp
Description: The most commonly used functionality from the low-level API.
-}
module Data.Capnp
    (
    -- * Working with capnproto lists
      Classes.ListElem(..)
    , Classes.MutListElem(..)

    -- * Working with capnproto Text and Data values.
    , Basics.Data
    , Basics.dataBytes
    , Basics.Text
    , Basics.textBytes

    -- * Working with messages
    , Message.ConstMsg
    , Message.Message(..)
    , Message.Mutable(..)
    , Message.MutMsg
    , Message.newMessage
    , decodeMessage
    , encodeMessage

    -- ** Reading and writing messages
    , Message.hPutMsg
    , Message.putMsg
    , Message.hGetMsg
    , Message.getMsg

    -- * Manipulating the root object of a message
    , Codec.getRoot
    , Codec.newRoot
    , Codec.setRoot

    -- * Reading values
    , hGetValue
    , getValue

    -- * Type aliases for common contexts
    , Message.WriteCtx
    , Untyped.ReadCtx
    , Untyped.RWCtx

    -- * Managing resource limits
    , module Data.Capnp.TraversalLimit
    ) where

import Control.Monad.Catch (MonadThrow)

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BB

import Data.Capnp.TraversalLimit

import Data.Capnp.IO (getValue, hGetValue)

import qualified Codec.Capnp        as Codec
import qualified Data.Capnp.Basics  as Basics
import qualified Data.Capnp.Classes as Classes
import qualified Data.Capnp.Message as Message
import qualified Data.Capnp.Untyped as Untyped

-- | Alias for 'Message.encode'
encodeMessage :: MonadThrow m => Message.ConstMsg -> m BB.Builder
encodeMessage = Message.encode

-- | Alias for 'Message.decode'
decodeMessage :: MonadThrow m => BS.ByteString -> m Message.ConstMsg
decodeMessage = Message.decode
