{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Simple.TCP (connect)

import Capnp               (def, defaultLimit)
import Capnp.Promise       (waitIO)
import Capnp.Rpc           ((?))
import Capnp.Rpc.Transport (socketTransport)
import Capnp.Rpc.Untyped   (ConnConfig(..), handleConn)

import Capnp.Gen.Echo.Pure

main :: IO ()
main = connect "localhost" "4000" $ \(sock, _addr) ->
    handleConn (socketTransport sock defaultLimit) def
        { debugMode = True
        , withBootstrap = Just $ \_sup client ->
            echo'echo (Echo client) ? def { query = "Hello, World!" }
                >>= waitIO
                >>= print
            -- TODO: stop the vat.
        }
