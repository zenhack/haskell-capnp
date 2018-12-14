module Main (main) where

import qualified Examples.CalculatorClient
import qualified Examples.CalculatorServer
import qualified Examples.EchoClient
import qualified Examples.EchoServer

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["calculator-client"] -> Examples.CalculatorClient.main
        ["calculator-server"] -> Examples.CalculatorServer.main
        ["echo-client"]       -> Examples.EchoClient.main
        ["echo-server"]       -> Examples.EchoServer.main
        _                     -> usageErr

usageErr :: IO ()
usageErr = do
    hPutStrLn
        stderr
        "Usage: run-capnp-example ( calculator-client | calculator-server | echo-client | echo-server )"
    exitFailure
