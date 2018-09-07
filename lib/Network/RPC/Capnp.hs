{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Network.RPC.Capnp
    ( RpcT
    , Client
    , VatConfig
    , Transport(..)
    , HandleTransport
    , runRpcT
    , bootstrap
    , call

    , Promise
    , Fulfiller
    , newPromise
    , fulfill
    , isResolved

    , nullClient
    ) where

import Control.Concurrent.STM
import Data.Word

import Control.Concurrent.Async  (concurrently_)
import Control.Exception         (Exception, throwIO)
import Control.Monad             (forever)
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Control.Monad.Reader      (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Maybe                (isJust)
import System.IO                 (Handle, hClose)

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

import Capnp.Capnp.Rpc.Pure hiding (Exception)

import Data.Capnp.Pure (def, hGetValue, hPutValue)

import qualified Capnp.Capnp.Rpc.Pure as Rpc

instance Exception Rpc.Exception

-- These aliases are actually defined in the schema, but the schema compiler
-- doesn't expose them to the code generator plugin, so we re-define them
-- ourselves.
type QuestionId = Word32
type AnswerId = Word32
type ExportId = Word32
type ImportId = Word32

class Transport t m where
    sendMsg :: t -> Message -> m ()
    recvMsg :: t -> m Message
    disconnect :: t -> m ()

data HandleTransport = HandleTransport
    { handle :: Handle
    , limit  :: !Int
    }

-- | Get a new exportId/questionId. The argument gets the pool to allocate from.
newId :: MonadIO m => (Vat -> TVar [Word32]) -> RpcT m Word32
newId pool = do
    vat <- RpcT ask
    liftIO $ atomically $ do
        oldIds <- readTVar (pool vat)
        case oldIds of
            [] ->
                retry
            (id:ids) -> do
                writeTVar (pool vat) ids
                pure id

-- | A "null" client, which throws unimplemented in response to all method
-- calls.
nullClient :: Client
nullClient = error "TODO"

newExportId :: MonadIO m => RpcT m ExportId
newExportId = newId exportIdPool

newQuestionId :: MonadIO m => RpcT m QuestionId
newQuestionId = newId questionIdPool

newtype RpcT m a = RpcT (ReaderT Vat m a)
    deriving(Functor, Applicative, Monad)

instance MonadTrans RpcT where
    lift = RpcT . lift

instance MonadIO m => MonadIO (RpcT m) where
    liftIO = lift . liftIO

instance Transport HandleTransport IO where
    sendMsg HandleTransport{handle} = liftIO . hPutValue handle
    recvMsg HandleTransport{handle, limit} = liftIO $ hGetValue handle limit
    disconnect = liftIO . hClose . handle

data Client
    = RemoteClient
        { target   :: MessageTarget
        , localVat :: Vat
        }

bootstrap :: MonadIO m => RpcT m Client
bootstrap = do
    questionId <- newQuestionId
    vat <- RpcT ask
    let msg = Message'bootstrap def { questionId }
    liftIO $ atomically $ sendQuestion vat (BootstrapQuestion questionId)
    pure RemoteClient
        { target = MessageTarget'promisedAnswer def { questionId }
        , localVat = vat
        }

sendQuestion :: Vat -> Question -> STM ()
sendQuestion Vat{sendQ,questions} question = do
    writeTBQueue sendQ $ getQuestionMessage question
    modifyTVar questions $ M.insert (getQuestionId question) question

call :: MonadIO m => Word64 -> Word16 -> Payload -> Client -> RpcT m (Promise Return)
call interfaceId methodId params RemoteClient{ target, localVat } = do
    questionId <- newQuestionId
    let callMsg = Call
            { sendResultsTo = Call'sendResultsTo'caller
            , allowThirdPartyTailCall = False
            , ..
            }
    (promise, fulfiller) <- liftIO $ atomically newPromise
    liftIO $ atomically $ sendQuestion localVat $ CallQuestion
        { callMsg
        , sendReturn = fulfiller
        }
    pure promise

newtype Fulfiller a = Fulfiller
    { var :: TVar (Maybe a)
    }

fulfill :: Fulfiller a -> a -> STM ()
fulfill Fulfiller{var} val = modifyTVar' var $ \case
    Nothing ->
        Just val
    Just _ ->
        -- TODO: report this in a more controlled way.
        error "BUG: tried to fullfill a promise twice!"

wait :: Promise a -> STM a
wait Promise{var} = do
    val <- readTVar var
    case val of
        Nothing ->
            retry
        Just result ->
            pure result

isResolved :: Promise a -> STM Bool
isResolved Promise{var} = isJust <$> readTVar var

newPromise :: STM (Promise a, Fulfiller a)
newPromise = do
    var <- newTVar Nothing
    pure (Promise{var}, Fulfiller{var})

newtype Promise a = Promise
    { var :: TVar (Maybe a)
    }

data Question
    = CallQuestion
        { callMsg    :: Call
        , sendReturn :: Fulfiller Return
        }
    | BootstrapQuestion !QuestionId

getQuestionMessage :: Question -> Message
getQuestionMessage CallQuestion{callMsg} = Message'call callMsg
getQuestionMessage (BootstrapQuestion questionId) =
    Message'bootstrap def { questionId }

getQuestionId :: Question -> QuestionId
getQuestionId CallQuestion{callMsg=Call{questionId}} = questionId
getQuestionId (BootstrapQuestion questionId)         = questionId

data Vat = Vat
    { questions      :: TVar (M.Map QuestionId Question)
    , answers        :: TVar (M.Map AnswerId Message)
    , imports        :: TVar (M.Map ImportId CapDescriptor)
    , exports        :: TVar (M.Map ExportId Client)

    , questionIdPool :: TVar [Word32]
    , exportIdPool   :: TVar [Word32]

    , sendQ          :: TBQueue Message
    , recvQ          :: TBQueue Message
    }

data VatConfig = VatConfig
    { maxQuestions :: !Word32
    , maxExports   :: !Word32
    }

runRpcT :: Transport t IO => VatConfig -> t -> RpcT IO () -> IO ()
runRpcT config transport (RpcT m) = do
    vat <- newVat config
    foldl concurrently_
        (recvLoop transport vat)
        [ sendLoop transport vat
        , coordinator vat
        , runReaderT m vat
        ]

newVat :: VatConfig -> IO Vat
newVat VatConfig{..} = atomically $ do
    questions <- newTVar M.empty
    answers <- newTVar M.empty
    imports  <- newTVar M.empty
    exports <- newTVar M.empty

    questionIdPool <- newTVar [0..maxQuestions-1]
    exportIdPool <- newTVar [0..maxExports-1]

    sendQ <- newTBQueue $ fromIntegral maxQuestions
    recvQ <- newTBQueue $ fromIntegral maxQuestions

    pure Vat{..}

sendLoop :: Transport t IO => t -> Vat -> IO ()
sendLoop transport Vat{sendQ} =
    forever $ atomically (readTBQueue sendQ) >>= sendMsg transport

recvLoop :: Transport t IO => t -> Vat -> IO ()
recvLoop transport Vat{recvQ} =
    forever $ recvMsg transport >>= atomically . writeTBQueue recvQ

coordinator :: Vat -> IO ()
coordinator Vat{..} = forever $ do
    msg <- atomically $ readTBQueue recvQ
    case msg of
        Message'abort exn ->
            throwIO exn
        Message'return ret@Return{..} ->
            atomically $ do
                question <- M.lookup answerId <$> readTVar questions
                case question of
                    Just CallQuestion{callMsg, sendReturn} -> do
                        -- We don't support caps other than the bootstrap yet, so we can
                        -- send Finish right away.
                        writeTBQueue sendQ $ Message'finish def { questionId = answerId }
                        modifyTVar questions $ M.delete answerId
                        fulfill sendReturn ret
                    Just (BootstrapQuestion _) -> do
                        -- This will case the other side to drop the resolved cap; we'll
                        -- just keep using the promise.
                        writeTBQueue sendQ $ Message'unimplemented msg
                        modifyTVar questions $ M.delete answerId
                    Nothing ->
                        writeTBQueue sendQ $ Message'abort def
                            { reason = "Received 'Return' for non-existant question #"
                                <> T.pack (show answerId)
                            , type_ = Exception'Type'failed
                            }
        _ ->
            atomically $ writeTBQueue sendQ $ Message'unimplemented msg
