{- |
Module: Data.Capnp
Description: The most commonly used functionality from the low-level API.
-}
module Data.Capnp
    ( Codec.getRoot
    , Codec.ListElem(..)
    , Codec.MutListElem(..)
    , Codec.newRoot
    , Codec.setRoot
    , Codec.getValue
    , Codec.hGetValue

    , Basics.Data
    , Basics.dataBytes
    , Basics.Text
    , Basics.textBytes

    , Message.ConstMsg
    , Message.Message(..)
    , Message.Mutable(..)
    , Message.MutMsg
    , Message.newMessage
    , Message.WriteCtx

    , decodeMessage
    , encodeMessage

    , Untyped.ReadCtx
    , Untyped.RWCtx

    , module Data.Capnp.TraversalLimit
    ) where

import Control.Monad.Catch (MonadThrow)

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BB

import Data.Capnp.TraversalLimit

import qualified Codec.Capnp        as Codec
import qualified Data.Capnp.Basics  as Basics
import qualified Data.Capnp.Message as Message
import qualified Data.Capnp.Untyped as Untyped

-- | Alias for 'Message.encode'
encodeMessage :: MonadThrow m => Message.ConstMsg -> m BB.Builder
encodeMessage = Message.encode

-- | Alias for 'Message.decode'
decodeMessage :: MonadThrow m => BS.ByteString -> m Message.ConstMsg
decodeMessage = Message.decode
