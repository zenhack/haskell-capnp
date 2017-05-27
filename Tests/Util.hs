{-# LANGUAGE RecordWildCards #-}
module Tests.Util where

import Control.Concurrent (forkIO)

import Control.Exception (bracket)
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.Resource (runResourceT, allocate)
import Control.Monad.Trans (lift)
import GHC.IO.Handle (hSetBinaryMode)
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import System.IO
import System.Process
import System.Directory (removeFile)

import TmpUtil(getMessage, Message)

getTestMessage :: TestMessage
                -> Int -- Max message size
                -> IO Message
getTestMessage testMessage quota = do
    contents <- encode testMessage
    case getMessage contents quota of
        Left e -> throwM e
        Right (msg,_) -> return msg


data TestMessage = TestMessage
    { msgSchema :: String
    , msgType :: String
    , msgValue :: String
    } deriving(Show)

encode TestMessage{..} = runResourceT $ do
    (_, schemaPath) <- allocate
        (writeTempFile "schema.capnp" msgSchema)
        removeFile
    (_, valuePath) <- allocate
        (writeTempFile "value.capnp" msgValue)
        removeFile
    contents <- lift $ runCapnp schemaPath valuePath msgType
    seq (L.length contents) (return contents)
  where
    writeTempFile template contents = runResourceT $ do
        (_, (path, hndl)) <- allocate
            (openTempFile "/tmp" template)
            (\(_, hndl) -> hClose hndl)
        lift $ hPutStr hndl contents
        return path
    runCapnp schemaFile valueFile typeName = do
        hInput <- openFile valueFile ReadMode
        let p = (proc "capnp" [ "encode"
                              , schemaFile
                              , typeName
                              ]) { std_in = UseHandle hInput
                                 , std_out = CreatePipe
                                 }
        (Nothing, Just hout, Nothing, _) <- createProcess p
        hSetBinaryMode hout True
        L.hGetContents hout
