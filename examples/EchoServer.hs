{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Simple.TCP (serve)

import Capnp     (def)
import Capnp.Rpc (VatConfig(..), runVat, socketTransport, vatConfig)

import Capnp.Gen.Echo.Pure

data MyEchoServer = MyEchoServer

instance Echo'server_ MyEchoServer where
    echo'echo params MyEchoServer = pure def { reply = query params }

main :: IO ()
main = serve "localhost" "4000" $ \(sock, _addr) ->
    runVat $ (vatConfig $ socketTransport sock)
        { offerBootstrap  = Just $ do
            Echo client <- export_Echo MyEchoServer
            pure client
        , debugMode = True
        }
