{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
module Data.Capnp.GenHelpers.Rpc where

import Control.Monad.Catch (throwM)
import Data.Default        (def)

import Capnp.Capnp.Rpc.Pure      (Payload(..))
import Data.Capnp.TraversalLimit (evalLimitT)

import qualified Data.Capnp.Errors          as E
import qualified Data.Capnp.GenHelpers.Pure as PH
import qualified Data.Capnp.Untyped.Pure    as PU
import qualified Network.RPC.Capnp          as Rpc

handleMethod server method payload@Payload{content=Nothing} =
    handleMethod server method payload { content = Just $ PU.PtrStruct def }
handleMethod server method Payload{ content = Just (PU.PtrStruct params) } = do
    typedParams <- evalLimitT maxBound $ PH.convertValue params
    results <- method typedParams server
    resultStruct <- evalLimitT maxBound $ PH.convertValue results
    (promise, fulfiller) <- Rpc.newPromiseIO
    Rpc.fulfillIO fulfiller resultStruct
    pure promise
handleMethod _ _ _ =
    throwM $ E.SchemaViolationError "Parameter was non-struct"
