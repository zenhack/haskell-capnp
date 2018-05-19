{-# LANGUAGE MultiParamTypeClasses #-}
module Codec.Capnp where

import Control.Monad.Catch (MonadThrow(throwM))
import Data.Capnp.Errors   (Error(SchemaViolationError))

class Decerialize from to where
    decerialize :: MonadThrow m => from -> m to

expected :: MonadThrow m => String -> m a
expected msg = throwM $ SchemaViolationError $ "expected " ++ msg
