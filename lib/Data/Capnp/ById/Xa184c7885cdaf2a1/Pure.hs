{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.Capnp.ById.Xa184c7885cdaf2a1.Pure where

-- Code generated by capnpc-haskell. DO NOT EDIT.
-- Generated from schema file: schema/capnp/rpc-twoparty.capnp

import Data.Int
import Data.Word

import Data.Capnp.Untyped.Pure (List)
import Data.Capnp.BuiltinTypes.Pure (Data, Text)
import Control.Monad.Catch (MonadThrow)
import Data.Capnp.TraversalLimit (MonadLimit)

import qualified Data.Capnp.Untyped.Pure
import qualified Data.Capnp.Untyped
import qualified Codec.Capnp

import Data.ByteString as BS

import qualified Data.Capnp.ById.Xa184c7885cdaf2a1
import qualified Data.Capnp.ById.Xbdf87d7bb8304e81.Pure
import qualified Data.Capnp.ById.Xbdf87d7bb8304e81

data JoinKeyPart
    = JoinKeyPart
        { joinId :: Word32
        , partCount :: Word16
        , partNum :: Word16
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa184c7885cdaf2a1.JoinKeyPart m BS.ByteString) JoinKeyPart where
    decerialize raw = JoinKeyPart
            <$> (Data.Capnp.ById.Xa184c7885cdaf2a1.get_JoinKeyPart'joinId raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa184c7885cdaf2a1.get_JoinKeyPart'partCount raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa184c7885cdaf2a1.get_JoinKeyPart'partNum raw >>= Codec.Capnp.decerialize)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.IsStruct m JoinKeyPart BS.ByteString where
    fromStruct struct = do
        raw <- Codec.Capnp.fromStruct struct
        Codec.Capnp.decerialize (raw :: Data.Capnp.ById.Xa184c7885cdaf2a1.JoinKeyPart m BS.ByteString)

data JoinResult
    = JoinResult
        { joinId :: Word32
        , succeeded :: Bool
        , cap :: Maybe (Data.Capnp.Untyped.Pure.PtrType)
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa184c7885cdaf2a1.JoinResult m BS.ByteString) JoinResult where
    decerialize raw = JoinResult
            <$> (Data.Capnp.ById.Xa184c7885cdaf2a1.get_JoinResult'joinId raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa184c7885cdaf2a1.get_JoinResult'succeeded raw >>= Codec.Capnp.decerialize)
            <*> (Data.Capnp.ById.Xa184c7885cdaf2a1.get_JoinResult'cap raw >>= Codec.Capnp.decerialize)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.IsStruct m JoinResult BS.ByteString where
    fromStruct struct = do
        raw <- Codec.Capnp.fromStruct struct
        Codec.Capnp.decerialize (raw :: Data.Capnp.ById.Xa184c7885cdaf2a1.JoinResult m BS.ByteString)

data Side
    = Side'server
    | Side'client
    | Side'unknown' (Word16)
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa184c7885cdaf2a1.Side m BS.ByteString) Side where
    decerialize raw = case raw of

        Data.Capnp.ById.Xa184c7885cdaf2a1.Side'server -> pure Side'server
        Data.Capnp.ById.Xa184c7885cdaf2a1.Side'client -> pure Side'client
        Data.Capnp.ById.Xa184c7885cdaf2a1.Side'unknown' val -> Side'unknown' <$> Codec.Capnp.decerialize val

data ProvisionId
    = ProvisionId
        { joinId :: Word32
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa184c7885cdaf2a1.ProvisionId m BS.ByteString) ProvisionId where
    decerialize raw = ProvisionId
            <$> (Data.Capnp.ById.Xa184c7885cdaf2a1.get_ProvisionId'joinId raw >>= Codec.Capnp.decerialize)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.IsStruct m ProvisionId BS.ByteString where
    fromStruct struct = do
        raw <- Codec.Capnp.fromStruct struct
        Codec.Capnp.decerialize (raw :: Data.Capnp.ById.Xa184c7885cdaf2a1.ProvisionId m BS.ByteString)

data VatId
    = VatId
        { side :: Side
        }
    deriving(Show, Read, Eq)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.Decerialize m (Data.Capnp.ById.Xa184c7885cdaf2a1.VatId m BS.ByteString) VatId where
    decerialize raw = VatId
            <$> (Data.Capnp.ById.Xa184c7885cdaf2a1.get_VatId'side raw >>= Codec.Capnp.decerialize)

instance (MonadThrow m, MonadLimit m) => Codec.Capnp.IsStruct m VatId BS.ByteString where
    fromStruct struct = do
        raw <- Codec.Capnp.fromStruct struct
        Codec.Capnp.decerialize (raw :: Data.Capnp.ById.Xa184c7885cdaf2a1.VatId m BS.ByteString)

