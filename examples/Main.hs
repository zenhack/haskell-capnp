{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Function          ((&))
import Network.Simple.TCP     (connect, serve)
import System.Environment     (getArgs)
import System.Exit            (exitFailure, exitSuccess)
import System.IO              (IOMode(ReadWriteMode), hClose)

import Capnp     (def, defaultLimit)
import Capnp.Rpc (VatConfig(..), bootstrap, runVat, socketTransport)

import Capnp.Gen.Echo.Pure

data MyEchoServer = MyEchoServer

instance Echo'server_ MyEchoServer where
    echo'echo params MyEchoServer = pure def { reply = query params }

main = do
    args <- getArgs
    case args of
        ["client"] -> runClient
        ["server"] -> runServer
        _ -> do
            putStrLn "usage: echo-demo ( client | server )"
            exitFailure

runClient =
    connect "localhost" "4000" $ \(sock, _addr) -> do
        transport <- socketTransport defaultLimit sock
        runVat def { debugMode = True } transport $ do
            echoSrv <- Echo <$> bootstrap
            result <- echoSrv & echo'echo def { query = "Hello, World!" }
            liftIO $ do
                print result
                exitSuccess

runServer =
    serve "localhost" "4000" $ \(sock, _addr) -> do
        transport <- socketTransport defaultLimit sock
        runVat
            def { bootstrapServer = Just $ do
                    Echo client <- export_Echo MyEchoServer
                    pure client
                , debugMode = True
                }
            transport
            (pure ())
