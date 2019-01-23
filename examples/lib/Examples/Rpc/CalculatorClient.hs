{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Examples.Rpc.CalculatorClient (main) where

import Network.Simple.TCP (connect)

import qualified Data.Vector as V

import Capnp               (def, defaultLimit)
import Capnp.Promise       (wait)
import Capnp.Rpc           ((?))
import Capnp.Rpc.Transport (socketTransport)
import Capnp.Rpc.Untyped   (ConnConfig(..), handleConn)
import Control.Monad       (when)

import Capnp.Gen.Calculator.Pure

main :: IO ()
main = connect "localhost" "4000" $ \(sock, _addr) ->
    handleConn (socketTransport sock defaultLimit) def
        { debugMode = True
        , withBootstrap = Just $ \_sup client -> do
            let calc = Calculator client

            Calculator'evaluate'results{value} <-
                calculator'evaluate calc ? def
                    { expression = Expression'literal 123 }
                    >>= wait
            Value'read'results{value} <- value'read value ? def >>= wait
            assertEq value 123

            Calculator'getOperator'results{func=add} <-
                calculator'getOperator calc ? def { op = Operator'add      } >>= wait
            Calculator'getOperator'results{func=subtract} <-
                calculator'getOperator calc ? def { op = Operator'subtract } >>= wait
            Calculator'evaluate'results{value} <- calculator'evaluate calc ? def
                { expression =
                    Expression'call Expression'call'
                        { function = subtract
                        , params = V.fromList
                            [ Expression'call Expression'call'
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
                >>= wait
            Value'read'results{value} <- value'read value ? def >>= wait
            assertEq value 101

            putStrLn "PASS"
        }

assertEq :: (Show a, Eq a) => a -> a -> IO ()
assertEq got want =
    when (got /= want) $
        error $ "Got " ++ show got ++ " but wanted " ++ show want
