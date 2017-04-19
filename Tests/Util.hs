{-# LANGUAGE RecordWildCards #-}
module Tests.Util where

import Control.Monad.Catch(throwM)
import GHC.IO.Handle (hSetBinaryMode)
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import System.IO
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
    case getMessage contents quota of
        Left e -> throwM e
        Right (msg,_) -> return msg

encode :: TestMessage -> IO L.ByteString
encode TestMessage{..} = do
    hInput <- openFile (constFile constName) ReadMode
    hSetBinaryMode hInput True
    let p = (proc "capnp" [ "encode"
                          , schemaFile schemaName
                          , typeName
                          ]) { std_in = UseHandle hInput
                             , std_out = CreatePipe
                             }
    (Nothing, Just hout, Nothing, _) <- createProcess p
    hSetBinaryMode hout True
    L.hGetContents hout
  where
    schemaFile name = "testdata/schema/" ++ name ++ ".capnp"
    constFile name = "testdata/constants/" ++ name ++ ".capnp"
