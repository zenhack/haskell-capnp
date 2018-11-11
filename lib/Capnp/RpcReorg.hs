{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE KindSignatures #-}
module Capnp.RpcReorg
    ( Conn
    ) where

import Data.Word
import UnliftIO.STM

import Control.Monad.Catch (MonadThrow)

import qualified StmContainers.Map as M

import Capnp.Classes (FromPtr, ToPtr, Decerialize(Cerial), Cerialize)
import Capnp.Message (ConstMsg)
import Capnp.Promise (Fulfiller)
import Capnp.Untyped (Ptr)

-- These alias are the same ones defined in rpc.capnp; unfortunately the
-- schema compiler doesn't supply information about type aliases, so we
-- have to re-define them ourselves. See the comments in rpc.capnp for
-- more information.
type QuestionId = Word32
type AnswerId   = QuestionId
type ExportId   = Word32
type ImportId   = ExportId
type EmbargoId  = Word32

-- | A connection to a remote vat
data Conn = Conn
    -- queues of messages to send and receive; each of these has a dedicated
    -- thread doing the IO:
    { sendQ    :: TBQueue ConstMsg
    , recvQ    :: TBQueue ConstMsg

    -- TODO: the four tables.

    -- Outstanding embargos. When we receive a 'Disembargo' message with its
    -- context field set to receiverLoopback, we look up the embargo id in
    -- this table, and execute the STM we have registered.
    , embargos :: M.Map EmbargoId (STM ())
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
    , Cerialize s r
    , ToPtr s (Cerial ConstMsg r)
    )
    => (c -> p -> m r)
    -> MethodHandler m (Cerial ConstMsg p) (Cerial ConstMsg r)
pureHandler = undefined

-- | Convert a typed method handler into one that deals in untyped
-- pointers.
untypedHandler
    :: (FromPtr ConstMsg p, forall s. ToPtr s r)
    => MethodHandler m p r
    -> MethodHandler
        m
        (Maybe (Ptr ConstMsg))
        (Maybe (Ptr ConstMsg))
untypedHandler = undefined

-- | A value that can receive and handle method calls, i.e. an object. It is
-- parametrized over the monadic context in which methods are serviced.
data MethodReceiver m = MethodReceiver
    { handleMethod
        :: Word64 -- ^ Interface Id
        -> Word16 -- ^ Method Id
        -> MethodHandler m (Maybe (Ptr ConstMsg)) (Maybe (Ptr ConstMsg))
    , handleStop :: m ()
    }

-- The coordinator processes incoming messages.
coordinator :: Conn -> IO ()
-- The logic here mostly routes messages to other parts of the code that know
-- more about the objects in question; See Note [Organization] for more info.
coordinator = undefined

-- Note [Organization]
-- ===================
--
-- As much as possible, the logic in this module is centralized according to
-- type types of objects it concerns.
--
-- As an example, consider how we handle embargos: The 'Conn' type's 'embargos'
-- table has values that are arbitrary 'STM' transactions. This allows the code
-- which triggers sending embargoes to have full control over what happens when
-- they return, while the code that routes incoming messages (in 'coordinator')
-- doesn't need to concern itself with the details of embargos -- it just needs
-- to route them to the right place.
--
-- This approach generally results in better separation of concerns.

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

-- Note [Limiting resource usage]
-- =============================
--
-- We employ various strategies to prevent remote vats from causing excessive
-- resource usage. In particular:
--
-- * We set a maximum size for incoming messages; this is in keeping with how
--   we mitigate these concerns when dealing with plain capnp data (i.e. not
--   rpc).
-- * We set a limit on the total *size* of all messages from the remote vat that
--   are currently being serviced. For example, if a Call message comes in,
--   we note its size, and deduct it from the quota. Once we have sent a return
--   and received a finish for this call, and thus can safely forget about it,
--   we remove it from our answers table, and add its size back to the available
--   quota.
--
-- Still TBD:
--
-- * We should come up with some way of guarding against too many intra-vat calls;
--   depending on the object graph, it may be possible for an attacker to get us
--   to "eat our own tail" so to speak.
--
--   Ideas:
--     * Per-object bounded queues for messages
--     * Global limit on intra-vat calls.
--
--   Right now I(zenhack) am more fond of the former.
--
-- * What should we actually do when limits are exceeded?
--
--   Possible strategies:
--     * Block
--     * Throw an 'overloaded' exception
--     * Some combination of the two; block with a timeout, then throw.
