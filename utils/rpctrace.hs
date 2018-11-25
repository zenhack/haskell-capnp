-- This is a simple program that:
--
-- * listens on localhost:6000
-- * connects to localhost:4000
-- * copies capnproto rpc messages between the connections, and pretty-prints
--   them to stdout.
--
-- It is useful for debugging the rpc system.
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.Async
import Control.Concurrent.STM

import Control.Monad      (forever)
import Network.Simple.TCP (connect, serve)
import Text.Show.Pretty   (pPrint)

import Capnp.Rpc.Transport

import Capnp                    (msgToValue)
import Capnp.Gen.Capnp.Rpc.Pure (Message)

main = do
    q <- newTQueueIO
    concurrently_
        (output q)
        (proxy q)

output q = forever $ do
    (memo, msg) <- atomically $ readTQueue q
    putStrLn $ "======================= " ++ memo ++ " ========================"
    pureMsg <- msgToValue msg
    pPrint (pureMsg :: Message)

proxy q = serve "localhost" "6000" $ \(client, _) ->
    connect "localhost" "4000" $ \(server, _) -> do
        concurrently_
            (copy "From Server" server client q)
            (copy "From Client" client server q)

copy memo fromSock toSock q = forever go
  where
    go = do
        msg <- recvMsg fromTrans
        atomically $ writeTQueue q (memo, msg)
        sendMsg toTrans msg

    fromTrans = socketTransport fromSock maxBound
    toTrans = socketTransport toSock maxBound
