{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Examples.Rpc.EchoServer (main) where

import Network.Simple.TCP (serve)

import Capnp     (def, defaultLimit)
import Capnp.Rpc
    ( ConnConfig(..)
    , Server
    , handleConn
    , pureHandler
    , socketTransport
    , toClient
    )

import Capnp.Gen.Echo.Pure

data MyEchoServer = MyEchoServer

instance Server IO MyEchoServer
instance Echo'server_ IO MyEchoServer where
    echo'echo = pureHandler $ \MyEchoServer params ->
        pure def { reply = query params }

main :: IO ()
main = serve "localhost" "4000" $ \(sock, _addr) ->
    handleConn (socketTransport sock defaultLimit) def
        { debugMode = True
        , getBootstrap = \sup -> Just . toClient <$> export_Echo sup MyEchoServer
        }
