{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
module Capnp.Rpc.Object where

import Data.Word

import Control.Monad.Catch (MonadThrow)

import Capnp.Classes (Cerialize, Decerialize(Cerial), FromPtr)
import Capnp.Message (ConstMsg)
import Capnp.Promise (Fulfiller)
import Capnp.Untyped (Ptr)

-- | a @'MethodHandler' m p r@ handles a method call with parameters @p@
-- and return type @r@, in monad @m@. See Note [Method handling].
data MethodHandler (m :: * -> *) p r

-- | 'pureHandler' creates a method handler from a function accepting the
-- receiver and parameters, and returning a monadic value computing the
-- result. See Note [Method handling].
pureHandler ::
    ( MonadThrow m
    , Decerialize p
    , FromPtr ConstMsg (Cerial ConstMsg p)
    , Cerialize r
    -- TODO: something like the below is needed, but I(zenhack) am having a
    -- hard time figuring out what the constraint should actually be; the
    -- below doesn't type-check.
    -- , ToPtr s (Cerial ConstMsg r)
    )
    => (c -> p -> m r)
    -> MethodHandler m (Cerial ConstMsg p) (Cerial ConstMsg r)
pureHandler = undefined

-- | A value that can receive and handle method calls, i.e. an object. It is
-- parametrized over the monadic context in which methods are serviced.
data MethodReceiver m = MethodReceiver
    { handleCall
        :: Word64 -- ^ Interface Id
        -> Word16 -- ^ Method Id
        -> MethodHandler m (Maybe (Ptr ConstMsg)) (Maybe (Ptr ConstMsg))
    -- ^ Handle a method call; takes the interface and method id and returns
    -- a handler for the specific method.
    , handleStop :: m ()
    -- ^ Handle shutting-down the receiver; this is called when the last
    -- reference to the capability is dropped.
    }

-- | A 'CallInfo' contains the information necessary to handle an untyped
-- method call.
data CallInfo = CallInfo
    { interfaceId :: !Word64
    -- ^ The id of the interface whose method is being called.
    , methodId    :: !Word16
    -- ^ The method id of the method being called.
    , arguments   :: Maybe (Ptr ConstMsg)
    -- ^ The arguments to the method call.
    , response    :: Fulfiller (Maybe (Ptr ConstMsg))
    -- ^ A 'Fulfiller' which accepts the method's return value.
    }

data ReceiverOp = Stop | Call CallInfo


-- Note [Method handling]
-- ======================
--
-- We represent method handlers via an abstract type 'MethodHandler',
-- parametrized over the reciever type, parameter and return types, and the
-- monadic context in which it runs. This allows us to provide different
-- strategies for actually handling methods; there are helper functions
-- which construct these handlers. For example:
--
-- * 'pureHandler' constructs a 'MethodHandler' from a function that works
--   with the types exposed by the high-level API.
-- * We will likely additionally provide handlers affording:
--   * Working directly with the low-level data types.
--   * Replying to the method call asynchronously, allowing later method
--     calls to be serviced before the current one is finished.
