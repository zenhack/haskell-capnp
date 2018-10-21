{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Function          ((&))
import Network.Simple.TCP     (connect, serve)
import System.Environment     (getArgs)
import System.Exit            (exitFailure, exitSuccess)
import System.IO              (IOMode(ReadWriteMode), hClose)

import Capnp     (def, defaultLimit)
import Capnp.Rpc (VatConfig(..), bootstrap, runVat, socketTransport, stopVat)

import Capnp.Gen.Echo.Pure

main = connect "localhost" "4000" $ \(sock, _addr) -> do
    transport <- socketTransport defaultLimit sock
    runVat def { debugMode = True } transport $ do
        echoSrv <- Echo <$> bootstrap
        result <- echoSrv & echo'echo def { query = "Hello, World!" }
        liftIO $ print result
        stopVat
