{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Examples.Rpc.EchoServer (main) where

import Capnp (SomeServer, def, defaultLimit, export, handleParsed)
import Capnp.Gen.Echo
import Capnp.Rpc (ConnConfig (..), handleConn, socketTransport, toClient)
import Network.Simple.TCP (serve)
import Supervisors (withSupervisor)

data MyEchoServer = MyEchoServer

instance SomeServer MyEchoServer

instance Echo'server_ MyEchoServer where
  echo'echo MyEchoServer = handleParsed $ \params ->
    pure def {reply = query params}

main :: IO ()
main =
  withSupervisor $ \sup -> do
    boot <- export @Echo sup MyEchoServer
    serve "localhost" "4000" $ \(sock, _addr) ->
      handleConn
        (socketTransport sock defaultLimit)
        def
          { debugMode = True,
            bootstrap = Just (toClient boot)
          }
