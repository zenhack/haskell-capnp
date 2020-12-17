{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Codec.Capnp
    ( newRoot
    , setRoot
    , getRoot
    ) where

import Capnp.Classes

import Capnp.Message (Mutability (..))

import qualified Capnp.Message as M
import qualified Capnp.Untyped as U

-- | 'newRoot' allocates and returns a new value inside the message, setting
-- it as the root object of the message.
newRoot :: (ToStruct ('Mut s) a, Allocate s a, M.WriteCtx m s)
    => M.Message ('Mut s) -> m a
newRoot msg = do
    ret <- new msg
    setRoot ret
    pure ret

-- | 'setRoot' sets its argument to be the root object in its message.
setRoot :: (ToStruct ('Mut s) a, M.WriteCtx m s) => a -> m ()
setRoot = U.setRoot . toStruct

-- | 'getRoot' returns the root object of a message.
getRoot :: (FromStruct mut a, U.ReadCtx m mut) => M.Message mut -> m a
getRoot msg = U.rootPtr msg >>= fromStruct
