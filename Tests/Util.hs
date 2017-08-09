{-# LANGUAGE RecordWildCards #-}
module Tests.Util where

import Control.Monad.Trans.Resource (runResourceT, allocate)
import Control.Monad.Trans (lift)
import GHC.IO.Handle (hSetBinaryMode)
import qualified Data.ByteString as BS
import System.IO
import System.Process
import System.Directory (removeFile)

import Data.CapNProto.Blob (BlobSlice)
import qualified Data.CapNProto.Message as M

getTestMessage :: TestMessage
                -> Int -- Max message size
                -> IO (M.Message (BlobSlice BS.ByteString))
getTestMessage testMessage quota = do
    contents <- encode testMessage
    M.decode contents


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
    lift $ runCapnp schemaPath valuePath msgType
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
        BS.hGetContents hout
