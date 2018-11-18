{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Network.Simple.TCP (serve)

import Capnp               (def, defaultLimit)
import Capnp.Rpc.Server    (pureHandler)
import Capnp.Rpc.Transport (socketTransport)
import Capnp.Rpc.Untyped   (ConnConfig(..), handleConn, toClient)

import Capnp.Gen.Echo.Pure

data MyEchoServer = MyEchoServer

instance Echo'server_ IO MyEchoServer where
    echo'echo = pureHandler $ \MyEchoServer params ->
        pure def { reply = query params }

main :: IO ()
main = serve "localhost" "4000" $ \(sock, _addr) ->
    handleConn (socketTransport sock defaultLimit) def
        { debugMode = True
        , getBootstrap = \sup -> toClient <$> export_Echo sup MyEchoServer
        }
