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

import Control.Monad           (forM_)
import Control.Monad.Catch     (MonadThrow(throwM))
import Data.Capnp.Errors       (Error(InvalidUtf8Error))
import Data.Capnp.Untyped      (rawBytes)
import Data.Capnp.Untyped.Pure (List)
import Data.Text.Encoding      (decodeUtf8')

import qualified Data.ByteString    as BS
import qualified Data.Capnp.Basics  as Basics
import qualified Data.Capnp.Message as M
import qualified Data.Capnp.Untyped as Untyped
import qualified Data.Text          as T

type Data = BS.ByteString
type Text = T.Text

instance Decerialize (Basics.Data M.ConstMsg) Data where
    decerialize (Basics.Data list) = rawBytes list

instance Cerialize s Data (Basics.Data (M.MutMsg s)) where
    cerialize msg bytes = do
        list <- Untyped.allocList8 msg (BS.length bytes)
        forM_ [0..BS.length bytes - 1] $ \i -> do
            Untyped.setIndex (BS.index bytes i) i list
        pure $ Basics.Data list

instance Decerialize (Basics.Text M.ConstMsg) Text where
    decerialize (Basics.Text list) = do
            bytes <- rawBytes list
            case decodeUtf8' bytes of
                Left e    -> throwM $ InvalidUtf8Error e
                Right txt -> pure txt
