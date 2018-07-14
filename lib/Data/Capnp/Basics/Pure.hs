{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Data.Capnp.Basics.Pure
    ( Data(..)
    , Text(..)

    -- Reexported from Untyped.Pure
    , List(..)
    ) where

import Codec.Capnp hiding (ListElem(List))

import Control.Monad.Catch     (MonadThrow(throwM))
import Data.Capnp.Errors       (Error(InvalidUtf8Error))
import Data.Capnp.Untyped      (rawBytes)
import Data.Capnp.Untyped.Pure (List)
import Data.Text.Encoding      (decodeUtf8')

import qualified Data.ByteString    as BS
import qualified Data.Capnp.Basics  as Basics
import qualified Data.Capnp.Message as M
import qualified Data.Text          as T

type Data = BS.ByteString
type Text = T.Text

instance Decerialize (Basics.Data M.Message) Data where
    decerialize (Basics.Data list) = rawBytes list

instance Decerialize (Basics.Text M.Message) Text where
    decerialize (Basics.Text list) = do
            bytes <- rawBytes list
            case decodeUtf8' bytes of
                Left e    -> throwM $ InvalidUtf8Error e
                Right txt -> pure txt
