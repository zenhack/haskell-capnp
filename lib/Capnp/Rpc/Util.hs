{-|
Module: Capnp.Rpc.Util
Description: Misc. utilities for the rpc layer.
-}
{-# LANGUAGE OverloadedStrings #-}
module Capnp.Rpc.Util
    ( wrapException
    ) where

import Data.Default       (def)
import Data.Maybe         (fromMaybe)
import Data.String        (fromString)
import UnliftIO.Exception (Exception, SomeException, fromException)

import qualified Capnp.Gen.Capnp.Rpc.Pure as RpcGen

instance Exception RpcGen.Exception

-- | @'wrapException' debugMode e@ converts an arbitrary haskell exception
-- @e@ into an rpc exception, which can be communicated to a remote vat.
-- If @debugMode@ is true, the returned exception will include the text of
-- @show e@.
wrapException :: Bool -> SomeException -> RpcGen.Exception
wrapException debugMode e = fromMaybe
    def { RpcGen.type_ = RpcGen.Exception'Type'failed
        , RpcGen.reason =
            if debugMode then
                "Unhandled exception: " <> fromString (show e)
            else
                "Unhandled exception"
        }
    (fromException e)
