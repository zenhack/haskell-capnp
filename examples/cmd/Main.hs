module Main (main) where

import qualified Examples.Rpc.CalculatorClient
import qualified Examples.Rpc.CalculatorServer
import qualified Examples.Rpc.EchoClient
import qualified Examples.Rpc.EchoServer
import qualified Examples.Serialization.HighLevel.Read
import qualified Examples.Serialization.HighLevel.Write
import qualified Examples.Serialization.LowLevel.Read
import qualified Examples.Serialization.LowLevel.Write

import Data.List          (intercalate)
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["calculator-client"] -> Examples.Rpc.CalculatorClient.main
        ["calculator-server"] -> Examples.Rpc.CalculatorServer.main
        ["echo-client"]       -> Examples.Rpc.EchoClient.main
        ["echo-server"]       -> Examples.Rpc.EchoServer.main
        ["highlevel-read"]    -> Examples.Serialization.HighLevel.Read.main
        ["highlevel-write"]   -> Examples.Serialization.HighLevel.Write.main
        ["lowlevel-read"]     -> Examples.Serialization.LowLevel.Read.main
        ["lowlevel-write"]    -> Examples.Serialization.LowLevel.Write.main
        _                     -> usageErr

usageErr :: IO ()
usageErr = do
    hPutStrLn
        stderr
        ("Usage: run-capnp-example ( "
            ++ intercalate " | "
            [ "calculator-client"
            , "calculator-server"
            , "echo-client"
            , "echo-server"
            , "highlevel-read"
            , "highlevel-write"
            , "lowlevel-read"
            , "lowlevel-write"
            ]
            ++ " )"
        )
    exitFailure
