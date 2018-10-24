{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
module Capnp.GenHelpers.Rpc where

import Control.Monad.Catch (MonadThrow(..))

import Capnp.Classes        (Decerialize(..), FromPtr(..))
import Capnp.TraversalLimit (evalLimitT)

import qualified Capnp.Errors          as E
import qualified Capnp.GenHelpers.Pure as PH
import qualified Capnp.Message         as M
import qualified Capnp.Rpc             as Rpc
import qualified Capnp.Untyped         as U

handleMethod server method paramContent = do
    content <- evalLimitT maxBound $
        fromPtr M.empty paramContent >>= decerialize
    results <- method content server
    resultStruct <- evalLimitT maxBound $ PH.convertValue results
    (promise, fulfiller) <- Rpc.newPromiseIO
    Rpc.fulfillIO fulfiller resultStruct
    pure promise

-- | A valid implementation of 'fromPtr' for any type that implements 'IsClient'.
--
-- GHC gets very confused if we try to just define a single instance
-- @IsClient a => FromPtr msg a@, so instead we define this helper function and
-- emit a trivial instance for each type from the code generator.
isClientFromPtr :: (Rpc.IsClient a, U.ReadCtx m msg) => msg -> Maybe (U.Ptr msg) -> m a
isClientFromPtr msg Nothing                     = pure $ Rpc.fromClient Rpc.nullClient
isClientFromPtr msg (Just (U.PtrCap cap)) = Rpc.fromClient <$> U.getClient cap
isClientFromPtr msg (Just _) = throwM $ E.SchemaViolationError "Expected capability pointer"

-- | A valid implementation of 'toPtr' for any type that implements 'IsClient'.
--
-- See the notes for 'isClientFromPtr'.
isClientToPtr :: (Rpc.IsClient a, M.WriteCtx m s) => M.MutMsg s -> a -> m (Maybe (U.Ptr (M.MutMsg s)))
isClientToPtr msg client = do
        cap <- U.appendCap msg (Rpc.toClient client)
        pure $ Just $ U.PtrCap cap
