{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module: Capnp.GenHelpers.Rpc
-- Description: Rpc-system helpers for genrated code.
--
-- This module defines various helpers used by generated code. Users
-- of the library are not expected to use this module directly.
module Capnp.GenHelpers.Rpc where

import Control.Exception.Safe (fromException, tryAny)
import Control.Monad.Catch    (MonadThrow(..))
import Data.Default           (def)

import Capnp.Classes        (Decerialize(..), FromPtr(..))
import Capnp.TraversalLimit (evalLimitT)

import qualified Capnp.Errors          as E
import qualified Capnp.GenHelpers.Pure as PH
import qualified Capnp.Message         as M
import qualified Capnp.Rpc.Promise     as Promise
import qualified Capnp.Rpc.Untyped     as Rpc
import qualified Capnp.Untyped         as U

handleMethod server method paramContent fulfiller = do
    ret <- tryAny $ do
        content <- evalLimitT maxBound $
            fromPtr M.empty paramContent >>= decerialize
        results <- method content server
        evalLimitT maxBound $ PH.convertValue results
    case ret of
        Right resultStruct ->
            Promise.fulfill fulfiller resultStruct
        Left e ->
            case fromException e of
                Just exn ->
                    Promise.breakPromise fulfiller exn
                Nothing ->
                    Promise.breakPromise fulfiller def
                        { Rpc.type_ = Rpc.Exception'Type'failed
                        , Rpc.reason = "Method threw an unhandled exception."
                        }

-- | A valid implementation of 'fromPtr' for any type that implements 'IsClient'.
--
-- GHC gets very confused if we try to just define a single instance
-- @IsClient a => FromPtr msg a@, so instead we define this helper function and
-- emit a trivial instance for each type from the code generator.
isClientFromPtr :: (Rpc.IsClient a, U.ReadCtx m msg) => msg -> Maybe (U.Ptr msg) -> m a
isClientFromPtr _ Nothing                     = pure $ Rpc.fromClient Rpc.nullClient
isClientFromPtr _ (Just (U.PtrCap cap)) = Rpc.fromClient <$> U.getClient cap
isClientFromPtr _ (Just _) = throwM $ E.SchemaViolationError "Expected capability pointer"

-- | A valid implementation of 'toPtr' for any type that implements 'IsClient'.
--
-- See the notes for 'isClientFromPtr'.
isClientToPtr :: (Rpc.IsClient a, M.WriteCtx m s) => M.MutMsg s -> a -> m (Maybe (U.Ptr (M.MutMsg s)))
isClientToPtr msg client = do
        cap <- U.appendCap msg (Rpc.toClient client)
        pure $ Just $ U.PtrCap cap
