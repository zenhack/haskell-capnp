{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
module Capnp.GenHelpers.Rpc where

import Capnp.Classes        (Decerialize(..), IsPtr(..))
import Capnp.TraversalLimit (evalLimitT)

import qualified Capnp.GenHelpers.Pure as PH
import qualified Capnp.Message         as M
import qualified Capnp.Rpc             as Rpc

handleMethod server method paramContent = do
    content <- evalLimitT maxBound $
        fromPtr M.empty paramContent >>= decerialize
    results <- method content server
    resultStruct <- evalLimitT maxBound $ PH.convertValue results
    (promise, fulfiller) <- Rpc.newPromiseIO
    Rpc.fulfillIO fulfiller resultStruct
    pure promise
