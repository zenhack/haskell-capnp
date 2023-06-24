{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Capnp.GenHelpers.Rpc
  ( module Capnp.Rpc.Server,
    module Capnp.Repr.Methods,
    parseCap,
    encodeCap,
  )
where

import Capnp.Message (Mutability (..))
import qualified Capnp.Message as M
import qualified Capnp.Repr as R
import Capnp.Repr.Methods
import Capnp.Rpc.Server
import qualified Capnp.Untyped as U

parseCap :: (R.IsCap a, U.ReadCtx m 'Const) => R.Raw a 'Const -> m (Client a)
parseCap (R.Raw cap) = Client <$> U.getClient cap

encodeCap :: (R.IsCap a, U.RWCtx m s) => M.Message ('Mut s) -> Client a -> m (R.Raw a ('Mut s))
encodeCap msg (Client c) = R.Raw <$> U.appendCap msg c
