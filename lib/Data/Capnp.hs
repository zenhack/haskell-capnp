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

    , Basics.Data
    , Basics.dataBytes
    , Basics.Text
    , Basics.textBytes

    , Message.ConstMsg
    , Message.Message(..)
    , Message.Mutable(..)
    , Message.MutMsg
    , Message.newMessage

    , decodeMessage
    , encodeMessage

    , module Data.Capnp.TraversalLimit
    ) where

import Control.Monad.Catch (MonadThrow)

import Data.Capnp.TraversalLimit

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BB

import qualified Codec.Capnp        as Codec
import qualified Data.Capnp.Basics  as Basics
import qualified Data.Capnp.Message as Message

-- | Alias for 'Message.encode'
encodeMessage :: MonadThrow m => Message.ConstMsg -> m BB.Builder
encodeMessage = Message.encode

-- | Alias for 'Message.decode'
decodeMessage :: MonadThrow m => BS.ByteString -> m Message.ConstMsg
decodeMessage = Message.decode
