{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Network.Simple.TCP (serve)
import Supervisors        (withSupervisor)

import Capnp               (def, defaultLimit)
import Capnp.Rpc           (ConnConfig(..), handleConn, toClient)
import Capnp.Rpc.Server    (pureHandler)
import Capnp.Rpc.Transport (socketTransport)

import Capnp.Gen.Echo.Pure

data MyEchoServer = MyEchoServer

instance Echo'server_ IO MyEchoServer where
    echo'echo = pureHandler $ \MyEchoServer params ->
        pure def { reply = query params }

main :: IO ()
main = withSupervisor $ \sup -> do
    bsClient <- export_Echo sup MyEchoServer
    serve "localhost" "4000" $ \(sock, _addr) ->
        handleConn (socketTransport sock defaultLimit) def
            { debugMode = True
            , getBootstrap = \_ -> pure (toClient bsClient)
            }
