{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Capnp.Rpc.Errors
Description: helpers for working with capnproto exceptions.

In addition to the values exposed in the API, this module also
defines an instance of Haskell's 'E.Exception' type class, for
Cap'n Proto's 'Exception'.
-}
module Capnp.Rpc.Errors
    (
    -- * Converting arbitrary exceptions to capnproto exceptions
      wrapException
    -- * Helpers for constructing exceptions
    , eMethodUnimplemented
    , eUnimplemented
    , eDisconnected
    , eFailed
    , throwFailed
    ) where

import Data.Default (Default(def))
import Data.Maybe   (fromMaybe)
import Data.String  (fromString)
import Data.Text    (Text)

import qualified Control.Exception.Safe as E

import Capnp.Gen.Capnp.Rpc.New

-- | Construct an exception with a type field of failed and the
-- given text as its reason.
eFailed :: Text -> Parsed Exception
eFailed reason = def
    { type_ = Exception'Type'failed
    , reason = reason
    }

-- | An exception with type = disconnected
eDisconnected :: Parsed Exception
eDisconnected = def
    { type_ = Exception'Type'disconnected
    , reason = "Disconnected"
    }

-- | An exception indicating an unimplemented method.
eMethodUnimplemented :: Parsed Exception
eMethodUnimplemented =
    eUnimplemented "Method unimplemented"

-- | An @unimplemented@ exception with a custom reason message.
eUnimplemented :: Text -> Parsed Exception
eUnimplemented reason = def
    { type_ = Exception'Type'unimplemented
    , reason = reason
    }

instance E.Exception (Parsed Exception)

-- | @'wrapException' debugMode e@ converts an arbitrary haskell exception
-- @e@ into an rpc exception, which can be communicated to a remote vat.
-- If @debugMode@ is true, the returned exception's reason field will include
-- the text of @show e@.
wrapException :: Bool -> E.SomeException -> Parsed Exception
wrapException debugMode e = fromMaybe
    def { type_ = Exception'Type'failed
        , reason =
            if debugMode then
                "Unhandled exception: " <> fromString (show e)
            else
                "Unhandled exception"
        }
    (E.fromException e)

-- | Throw an exception with a type field of 'Exception'Type'failed' and
-- the argument as a reason.
throwFailed :: E.MonadThrow m => Text -> m a
throwFailed = E.throwM . eFailed
