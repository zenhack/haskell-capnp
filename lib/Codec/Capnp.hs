{-# LANGUAGE MultiParamTypeClasses #-}
module Codec.Capnp where

import Data.Capnp.Errors (ThrowError)

class Decerialize from to where
    decerialize :: (ThrowError m, Monad m) => from -> m to
