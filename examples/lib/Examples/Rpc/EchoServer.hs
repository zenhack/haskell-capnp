{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module Examples.Rpc.EchoServer (main) where

import Network.Simple.TCP (serve)

import Capnp.New (SomeServer, def, defaultLimit, export, handleParsed)
import Capnp.Rpc (ConnConfig(..), handleConn, socketTransport, toClient)

import Capnp.Gen.Echo.New

data MyEchoServer = MyEchoServer

instance SomeServer MyEchoServer

instance Echo'server_ MyEchoServer where
    echo'echo MyEchoServer = handleParsed $ \params ->
        pure def { reply = query params }

main :: IO ()
main = serve "localhost" "4000" $ \(sock, _addr) ->
    handleConn (socketTransport sock defaultLimit) def
        { debugMode = True
        , getBootstrap = \sup -> Just . toClient <$> export @Echo sup MyEchoServer
        }
