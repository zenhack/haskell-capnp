{-# LANGUAGE MultiParamTypeClasses #-}
module Codec.CapNProto where

import Data.CapNProto.Errors (ThrowError)

class Decerialize from to where
    decerialize :: (ThrowError m, Monad m) => from -> m to
