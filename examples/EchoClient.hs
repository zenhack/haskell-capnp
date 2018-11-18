{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Function          ((&))
import Network.Simple.TCP     (connect)

import Capnp               (def, defaultLimit)
import Capnp.Rpc.Transport (socketTransport)
import Capnp.Rpc.Untyped   (ConnConfig(..), handleConn)

import Capnp.Gen.Echo.Pure

main :: IO ()
main = connect "localhost" "4000" $ \(sock, _addr) ->
    handleConn (socketTransport sock defaultLimit) def
        { debugMode = True
        , withBootstrap = Just $ \_sup client -> do
            let echoSrv = Echo client
            result <- echoSrv & echo'echo def { query = "Hello, World!" }
            print result
            -- TODO: stop the vat.
        }
