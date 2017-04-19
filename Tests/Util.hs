{-# LANGUAGE RecordWildCards #-}
module Tests.Util where

import GHC.IO.Handle (hSetBinaryMode)
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import System.Process

import TmpUtil(getMessage, Message)

data TestMessage = TestMessage
    { schemaName :: String
    , typeName :: String
    , constName :: String
    } deriving(Show)

getTestMessage :: TestMessage
                -> Int -- Max message size
                -> IO Message
getTestMessage testMessage quota = do
    contents <- encode testMessage
    getMessage contents quota

encode :: TestMessage -> IO L.ByteString
encode TestMessage{..} = do
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
