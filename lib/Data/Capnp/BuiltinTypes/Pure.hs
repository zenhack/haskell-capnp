{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Data.Capnp.BuiltinTypes.Pure
    ( Data(..)
    , Text(..)

    -- Reexported from Untyped.Pure
    , List(..)
    ) where

import Data.Word

import Codec.Capnp
import Control.Monad.Catch     (MonadThrow(throwM))
import Data.Capnp.Errors       (Error(InvalidUtf8Error, SchemaViolationError))
import Data.Capnp.Untyped.Pure (List)
import Data.Text.Encoding      (decodeUtf8')

import qualified Data.ByteString as BS
import qualified Data.Text       as T
import qualified Data.Vector     as V

type Data = BS.ByteString
type Text = T.Text

instance Decerialize (List Word8) Data where
    decerialize = pure . BS.pack . V.toList

instance Decerialize (List Word8) Text where
    decerialize bytes = do
            trimedBS <- decerialize bytes >>= trim
            case decodeUtf8' trimedBS of
                Left e    -> throwM $ InvalidUtf8Error e
                Right txt -> pure txt
      where
        trim bs
            | BS.length bs == 0 = pure bs
            | BS.index bs (BS.length bs - 1) /= 0 = throwM $ SchemaViolationError "Text did not end with NUL"
            | otherwise = pure $ BS.take (BS.length bs - 1) bs
