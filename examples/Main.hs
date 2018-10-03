{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Function          ((&))
import Network.Simple.TCP     (connect)
import System.Exit            (exitSuccess)
import System.IO              (IOMode(ReadWriteMode), hClose)

import Data.Capnp        (def, defaultLimit)
import Network.RPC.Capnp (bootstrap, runVat, socketTransport)

import Capnp.Echo.Pure

main = connect "localhost" "4000" $ \(sock, _addr) -> do
    transport <- socketTransport defaultLimit sock
    runVat def transport $ do
        echoSrv <- Echo <$> bootstrap
        result <- echoSrv & echo'echo def { query = "Hello, World!" }
        liftIO $ do
            print result
            exitSuccess
