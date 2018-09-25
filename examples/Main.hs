{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception      (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Function          ((&))
import Network.Simple.TCP     (connectSock)
import Network.Socket         (socketToHandle)
import System.Exit            (exitSuccess)
import System.IO              (IOMode(ReadWriteMode), hClose)

import Data.Capnp        (def, defaultLimit)
import Network.RPC.Capnp (bootstrap, handleTransport, runRpcT)

import Capnp.Echo.Pure

main = bracket openConn hClose talk
  where
    openConn = do
        (sock, _addr) <- connectSock "localhost" "4000"
        socketToHandle sock ReadWriteMode
    talk handle =
        runRpcT def (handleTransport defaultLimit handle) $ do
            echoSrv <- Echo <$> bootstrap
            result <- echoSrv & echo'echo def { query = "Hello, World!" }
            liftIO $ do
                print result
                exitSuccess
