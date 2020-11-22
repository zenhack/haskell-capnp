{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{- |
Module: Capnp.Basics.Pure
Description: Handling of "basic" capnp datatypes (high-level API).

Analogous to 'Capnp.Basics' in the low-level API, this module deals
with capnproto's @Text@ and @Data@ types. These are simply aliases for
'BS.ByteString' and the text package's 'T.Text'; mostly this module provides
helper functions and type class instances.

Unlike with the low-level API, typed lists do not require special
treatment -- they're just Vectors.
-}
module Capnp.Basics.Pure
    ( Data
    , Text
    ) where

import Prelude hiding (length)

import Control.Monad       (forM_)
import Control.Monad.Catch (MonadThrow(throwM))
import Data.Text.Encoding  (decodeUtf8', encodeUtf8)

import qualified Data.ByteString as BS
import qualified Data.Text       as T
import qualified Data.Vector     as V

import Capnp.Classes

import Capnp.Errors  (Error(InvalidUtf8Error))
import Capnp.Untyped (rawBytes)

import qualified Capnp.Basics  as Basics
import qualified Capnp.Message as M
import qualified Capnp.Untyped as Untyped

-- | A capnproto @Data@ value. This is just an alias for 'BS.ByteString'.
type Data = BS.ByteString

-- | A capnproto @Text@. This  is just an alias for the text package's 'T.Text'.
type Text = T.Text

instance Decerialize Data where
    type Cerial msg Data = Basics.Data msg
    decerialize (Basics.Data list) = rawBytes list

instance Marshal s Data where
    marshalInto (Basics.Data list) bytes =
        forM_ [0..BS.length bytes - 1] $ \i ->
            Untyped.setIndex (BS.index bytes i) i list

instance Cerialize s Data where
    cerialize msg bytes = do
        dest <- Basics.newData msg (BS.length bytes)
        marshalInto dest bytes
        pure dest

instance Decerialize Text where
    type Cerial msg Text = Basics.Text msg
    decerialize text = do
            bytes <- Basics.textBytes text
            case decodeUtf8' bytes of
                Left e    -> throwM $ InvalidUtf8Error e
                Right txt -> pure txt

instance Marshal s Text where
    marshalInto dest text = marshalTextBytes (encodeUtf8 text) dest

instance Cerialize s Text where
    cerialize msg text = do
        let bytes = encodeUtf8 text
        ret <- Basics.newText msg (BS.length bytes)
        marshalTextBytes bytes ret
        pure ret

marshalTextBytes :: Untyped.RWCtx m s => BS.ByteString -> Basics.Text (M.MutMsg s) -> m ()
marshalTextBytes bytes text = do
    buffer <- Basics.textBuffer text
    marshalInto (Basics.Data buffer) bytes

instance Cerialize s (V.Vector Text) where cerialize = cerializeBasicVec
instance Cerialize s (V.Vector Data) where cerialize = cerializeBasicVec
instance Cerialize s (V.Vector (V.Vector Text)) where cerialize = cerializeBasicVec
instance Cerialize s (V.Vector (V.Vector Data)) where cerialize = cerializeBasicVec
instance Cerialize s (V.Vector (V.Vector (V.Vector Text))) where cerialize = cerializeBasicVec
instance Cerialize s (V.Vector (V.Vector (V.Vector Data))) where cerialize = cerializeBasicVec
instance Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector Text)))) where cerialize = cerializeBasicVec
instance Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector Data)))) where cerialize = cerializeBasicVec
instance Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Text))))) where cerialize = cerializeBasicVec
instance Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Data))))) where cerialize = cerializeBasicVec
