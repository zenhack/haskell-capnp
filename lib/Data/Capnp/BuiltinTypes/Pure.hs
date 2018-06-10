{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Data.Capnp.BuiltinTypes.Pure
    ( Data(..)
    , Text(..)

    -- Reexported from Untyped.Pure
    , List(..)
    ) where

import Codec.Capnp

import Control.Monad             (when)
import Control.Monad.Catch       (MonadThrow(throwM))
import Data.Capnp.Errors         (Error(InvalidUtf8Error, SchemaViolationError))
import Data.Capnp.TraversalLimit (MonadLimit)
import Data.Capnp.Untyped.Pure   (List)
import Data.Text.Encoding        (decodeUtf8')
import Data.Word                 (Word8)

import qualified Data.ByteString         as BS
import qualified Data.Capnp.Blob         as B
import qualified Data.Capnp.BuiltinTypes as BuiltinTypes
import qualified Data.Text               as T
import qualified Data.Vector             as V

type Data = BS.ByteString
type Text = T.Text

instance (MonadThrow m, MonadLimit m) => Decerialize m (BuiltinTypes.Data BS.ByteString) Data where
    decerialize (BuiltinTypes.Data bytes) = pure bytes

instance (MonadThrow m, MonadLimit m) => Decerialize m (BuiltinTypes.Text BS.ByteString) Text where
    decerialize (BuiltinTypes.Text bytes) =
            case decodeUtf8' bytes of
                Left e    -> throwM $ InvalidUtf8Error e
                Right txt -> pure txt


-- TODO: remove these, once stuff is bootstrapped.
instance (MonadThrow m, MonadLimit m) => Decerialize m (List Word8) Data where
    decerialize = pure . BS.pack . V.toList
instance (MonadThrow m, MonadLimit m) => Decerialize m (List Word8) Text where
    decerialize raw = do
        bytes <- decerialize raw
        len <- B.length (bytes :: BS.ByteString)
        if len < 1
            then pure ""
            else do
                lastByte <- B.index bytes (len - 1)
                when (lastByte /= 0) $ throwM $ SchemaViolationError $
                    "Text is not NUL-terminated (last byte is " ++ show lastByte ++ ")"
                decerialize =<< (BuiltinTypes.Text <$> B.slice bytes 0 (len - 1))
