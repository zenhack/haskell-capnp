{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Data.Capnp.Basics.Pure
    ( Data(..)
    , Text(..)

    -- Reexported from Untyped.Pure
    , List(..)
    ) where

import Codec.Capnp hiding (ListElem(List))

import Control.Monad           (forM_)
import Control.Monad.Catch     (MonadThrow(throwM))
import Data.Capnp.Errors       (Error(InvalidUtf8Error))
import Data.Capnp.Untyped      (rawBytes)
import Data.Capnp.Untyped.Pure (List)
import Data.Text.Encoding      (decodeUtf8')

import qualified Data.ByteString    as BS
import qualified Data.Capnp.Basics  as Basics
import qualified Data.Capnp.Untyped as Untyped
import qualified Data.Text          as T

type Data = BS.ByteString
type Text = T.Text

instance Decerialize Data where
    type Cerial msg Data = Basics.Data msg
    decerialize (Basics.Data list) = rawBytes list

instance Marshal Data where
    marshalInto (Basics.Data list) bytes =
        forM_ [0..BS.length bytes - 1] $ \i ->
            Untyped.setIndex (BS.index bytes i) i list

instance Decerialize Text where
    type Cerial msg Text = Basics.Text msg
    decerialize text = do
            bytes <- Basics.textBytes text
            case decodeUtf8' bytes of
                Left e    -> throwM $ InvalidUtf8Error e
                Right txt -> pure txt
