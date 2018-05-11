{-# LANGUAGE MultiParamTypeClasses #-}
module Codec.Capnp where

import Data.Capnp.Errors (Error(..), ThrowError(..))

class Decerialize from to where
    decerialize :: (ThrowError m, Monad m) => from -> m to

expected :: ThrowError m => String -> m a
expected msg = throwError $ SchemaViolationError $ "expected " ++ msg
