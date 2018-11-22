{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Main where

import Network.Simple.TCP (connect)
import Supervisors        (Supervisor)

import qualified Data.Vector as V

import Capnp               (def, defaultLimit)
import Capnp.Promise       (waitIO)
import Capnp.Rpc           (throwFailed, (?))
import Capnp.Rpc.Server    (pureHandler)
import Capnp.Rpc.Transport (socketTransport)
import Capnp.Rpc.Untyped   (ConnConfig(..), handleConn, stopVat, toClient)
import Control.Monad       (when)

import Capnp.Gen.Calculator.Pure

main = connect "localhost" "4000" $ \(sock, _addr) ->
    handleConn (socketTransport sock defaultLimit) def
        { debugMode = True
        , withBootstrap = Just $ \_sup client -> do
            let calc = Calculator client

            Calculator'evaluate'results{value} <-
                calculator'evaluate calc ? def
                    { expression = Expression'literal 123 }
                    >>= waitIO
            Value'read'results{value} <- value'read value ? def >>= waitIO
            assertEq value 123

            Calculator'getOperator'results{func=add} <-
                calculator'getOperator calc ? def { op = Operator'add      } >>= waitIO
            Calculator'getOperator'results{func=subtract} <-
                calculator'getOperator calc ? def { op = Operator'subtract } >>= waitIO
            Calculator'evaluate'results{value} <- calculator'evaluate calc ? def
                { expression =
                    Expression'call
                        { function = subtract
                        , params = V.fromList
                            [ Expression'call
                                { function = add
                                , params = V.fromList
                                    [ Expression'literal 123
                                    , Expression'literal 45
                                    ]
                                }
                            , Expression'literal 67
                            ]
                        }
                }
                >>= waitIO
            Value'read'results{value} <- value'read value ? def >>= waitIO
            assertEq value 101

            putStrLn "PASS"
            stopVat
        }

assertEq :: (Show a, Eq a) => a -> a -> IO ()
assertEq got want =
    when (got /= want) $
        error $ "Got " ++ show got ++ " but wanted " ++ show want
