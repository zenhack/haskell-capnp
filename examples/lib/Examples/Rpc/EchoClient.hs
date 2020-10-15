{-# LANGUAGE OverloadedStrings #-}
module Examples.Rpc.EchoClient (main) where

import Network.Simple.TCP (connect)

import Capnp     (def, defaultLimit)
import Capnp.Rpc (ConnConfig (..), handleConn, socketTransport, wait, (?))

import Capnp.Gen.Echo.Pure

main :: IO ()
main = connect "localhost" "4000" $ \(sock, _addr) ->
    handleConn (socketTransport sock defaultLimit) def
        { debugMode = True
        , withBootstrap = Just $ \_sup client ->
            echo'echo (Echo client) ? def { query = "Hello, World!" }
                >>= wait
                >>= print
        }
