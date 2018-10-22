{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Function          ((&))
import Network.Simple.TCP     (connect)

import Capnp     (def)
import Capnp.Rpc (VatConfig(..), runVat, socketTransport, stopVat, vatConfig)

import Capnp.Gen.Echo.Pure

main :: IO ()
main = connect "localhost" "4000" $ \(sock, _addr) -> do
    runVat $ (vatConfig $ socketTransport sock)
        { debugMode = True
        , withBootstrap = Just $ \client -> do
            let echoSrv = Echo client
            result <- echoSrv & echo'echo def { query = "Hello, World!" }
            liftIO $ print result
            stopVat
        }
