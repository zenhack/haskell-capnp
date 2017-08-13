{-# LANGUAGE RecordWildCards #-}
module Tests.Util where

import Control.Concurrent (forkIO)
import Control.Monad.Trans.Resource (runResourceT, allocate)
import Control.Monad.Trans (lift)
import GHC.IO.Handle (hSetBinaryMode)
import qualified Data.ByteString as BS
import System.IO
import System.Process
import System.Directory (removeFile)

import Data.CapNProto.Blob (BlobSlice)
import qualified Data.CapNProto.Message as M

import Test.Framework (testGroup, Test)
import qualified Test.HUnit as H
import Test.Framework.Providers.HUnit (hUnitTestToTests)

-- | Information about the contents of a capnp message. This is enough
-- to encode/decode both textual and binary forms.
data MsgMetaData = MsgMetaData
    { msgSchema :: String -- ^ The source of the schema
    , msgType   :: String -- ^ The name of the root struct's type
    } deriving(Show)

-- | @capnpEncode msg meta@ runs @capnp encode@ on the message, providing
-- the needed metadata and returning the output
capnpEncode :: String -> MsgMetaData -> IO BS.ByteString
capnpEncode msgValue MsgMetaData{..} = runResourceT $ do
    (_, schemaPath) <- allocate
        (writeTempFile "schema.capnp" msgSchema)
        removeFile
    runCapnp schemaPath
  where
    writeTempFile template contents = runResourceT $ do
        (_, (path, hndl)) <- allocate
            (openTempFile "/tmp" template)
            (\(_, hndl) -> hClose hndl)
        lift $ hPutStr hndl contents
        return path
    runCapnp schemaFile = do
        let p = (proc "capnp" [ "encode"
                              , schemaFile
                              , msgType
                              ]) { std_in = CreatePipe
                                 , std_out = CreatePipe
                                 }
        (_, (Just hin, Just hout, Nothing, _)) <- allocate
            (createProcess p)
            (\(Just hin, Just hout, Nothing, _p) -> do
                hClose hout
                hClose hin)
        lift $ do
            forkIO $ do
                hPutStr hin msgValue
                hClose hin
            hSetBinaryMode hout True
            BS.hGetContents hout


-- | Convert a list of 'Assertion's to a test group with the given name.
assertionsToTest :: String -> [H.Assertion] -> Test
assertionsToTest name =
    testGroup name . hUnitTestToTests . H.TestList . map H.TestCase
