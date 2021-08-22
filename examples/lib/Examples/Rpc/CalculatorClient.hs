{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
module Examples.Rpc.CalculatorClient (main) where

import qualified Capnp.New          as C
import           Capnp.Rpc
    (ConnConfig(..), fromClient, handleConn, socketTransport)
import           Control.Monad      (when)
import           Data.Function      ((&))
import           Data.Functor       ((<&>))
import qualified Data.Vector        as V
import           Network.Simple.TCP (connect)

import Capnp.Gen.Calculator.New

main :: IO ()
main = connect "localhost" "4000" $ \(sock, _addr) ->
    handleConn (socketTransport sock C.defaultLimit) C.def
        { debugMode = True
        , withBootstrap = Just $ \_sup client -> do
            let calc :: C.Client Calculator
                calc = fromClient client

            value <- calc
                & C.callP #evaluate C.def { expression = Expression $ Expression'literal 123 }
                <&> C.pipe #value
                >>= C.callR #read C.def
                >>= C.waitPipeline
                >>= C.evalLimitT C.defaultLimit . C.parseField #value
            assertEq value 123

            let getOp op = calc
                    & C.callP #getOperator C.def { op }
                    <&> C.pipe #func
                    >>= C.asClient

            add <- getOp Operator'add
            subtract <- getOp Operator'subtract

            value <- calc
                & C.callP #evaluate C.def
                    { expression =
                        Expression $ Expression'call Expression'call'
                            { function = subtract
                            , params = V.fromList
                                [ Expression $ Expression'call Expression'call'
                                    { function = add
                                    , params = V.fromList
                                        [ Expression $ Expression'literal 123
                                        , Expression $ Expression'literal 45
                                        ]
                                    }
                                , Expression $ Expression'literal 67
                                ]
                            }
                    }
                <&> C.pipe #value
                >>= C.callR #read C.def
                >>= C.waitPipeline
                >>= C.evalLimitT C.defaultLimit . C.parseField #value
            assertEq value 101

            putStrLn "PASS"
        }

assertEq :: (Show a, Eq a) => a -> a -> IO ()
assertEq got want =
    when (got /= want) $
        error $ "Got " ++ show got ++ " but wanted " ++ show want
