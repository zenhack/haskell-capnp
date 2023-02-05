{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.Rpc.EchoClient (main) where

import qualified Capnp as C
import Capnp.Gen.Echo
import Capnp.Rpc
  ( ConnConfig (..),
    fromClient,
    requestBootstrap,
    socketTransport,
    withConn,
  )
import Data.Function ((&))
import Data.Functor ((<&>))
import Network.Simple.TCP (connect)

main :: IO ()
main = connect "localhost" "4000" $ \(sock, _addr) ->
  withConn
    (socketTransport sock C.defaultLimit)
    (C.def {debugMode = True})
    $ \conn -> do
      client <- requestBootstrap conn
      let echoClient :: C.Client Echo
          echoClient = fromClient client
      echoClient
        & C.callP #echo C.def {query = "Hello, World!"}
        <&> C.pipe #reply
        >>= C.waitPipeline
        >>= C.evalLimitT C.defaultLimit . C.parse
        >>= print
