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

import Test.Framework (testGroup, Test)
import qualified Test.HUnit as H
import Test.Framework.Providers.HUnit (hUnitTestToTests)

-- | Convert the textual message into a binary one, by calling the capnp tool.
getTextMessage :: TextMessage -> IO (M.Message (BlobSlice BS.ByteString))
getTextMessage textMessage = capnpEncode textMessage >>= M.decode

-- | A message in textual form, with enough information to encode it.
data TextMessage = TextMessage
    { msgSchema :: String -- ^ The source of the schema
    , msgType   :: String -- ^ The name of the root struct's type
    , msgValue  :: String -- ^ The textual representation of the value.
    } deriving(Show)

-- | Run @capnp encode@ on the message, returning the output
capnpEncode :: TextMessage -> IO BS.ByteString
capnpEncode TextMessage{..} = runResourceT $ do
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


-- | Convert a list of 'Assertion's to a test group with the given name.
assertionsToTest :: String -> [H.Assertion] -> Test
assertionsToTest name =
    testGroup name . hUnitTestToTests . H.TestList . map H.TestCase
