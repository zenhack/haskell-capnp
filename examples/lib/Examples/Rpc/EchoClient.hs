{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.Rpc.EchoClient (main) where

import Capnp.Gen.Echo
import qualified Capnp as C
import Capnp.Rpc
  ( ConnConfig (..),
    fromClient,
    handleConn,
    socketTransport,
  )
import Data.Function ((&))
import Data.Functor ((<&>))
import Network.Simple.TCP (connect)

main :: IO ()
main = connect "localhost" "4000" $ \(sock, _addr) ->
  handleConn
    (socketTransport sock C.defaultLimit)
    C.def
      { debugMode = True,
        withBootstrap = Just $ \_sup client ->
          let echoClient :: C.Client Echo
              echoClient = fromClient client
           in echoClient
                & C.callP #echo C.def {query = "Hello, World!"}
                <&> C.pipe #reply
                >>= C.waitPipeline
                >>= C.evalLimitT C.defaultLimit . C.parse
                >>= print
      }
