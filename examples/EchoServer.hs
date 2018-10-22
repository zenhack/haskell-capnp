{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Function          ((&))
import Network.Simple.TCP     (connect, serve)
import System.Environment     (getArgs)
import System.Exit            (exitFailure, exitSuccess)
import System.IO              (IOMode(ReadWriteMode), hClose)

import Capnp     (def, defaultLimit)
import Capnp.Rpc
    (VatConfig(..), bootstrap, runVat, socketTransport, stopVat, vatConfig)

import Capnp.Gen.Echo.Pure

data MyEchoServer = MyEchoServer

instance Echo'server_ MyEchoServer where
    echo'echo params MyEchoServer = pure def { reply = query params }

main = serve "localhost" "4000" $ \(sock, _addr) -> do
    runVat $ (vatConfig $ socketTransport sock)
        { offerBootstrap  = Just $ do
            Echo client <- export_Echo MyEchoServer
            pure client
        , debugMode = True
        }
