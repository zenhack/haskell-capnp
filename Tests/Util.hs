module Tests.Util where

import qualified Data.ByteString.Lazy as L
import System.Process
import GHC.IO.Handle (hSetBinaryMode)

encode :: String -> String -> String -> IO L.ByteString
encode schemaName constName typeName = do
    input <- L.readFile (constFile constName)
    interactProcess "capnp" [ "encode"
                            , schemaFile schemaName
                            , typeName
                            ]
                    input
  where
    schemaFile name = "testdata/schema/" ++ name ++ ".capnp"
    constFile name = "testdata/constant/" ++ name ++ ".capnp"

interactProcess :: String -> [String] -> L.ByteString -> IO L.ByteString
interactProcess cmd args inBytes = do
    let p = (proc cmd args) { std_in = CreatePipe
                            , std_out = CreatePipe
                            }
    (Just hin, Just hout, Nothing, _) <- createProcess p
    hSetBinaryMode hin True
    hSetBinaryMode hout True
    L.hPut hin inBytes
    L.hGetContents hout
