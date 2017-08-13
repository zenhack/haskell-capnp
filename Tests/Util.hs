{-# LANGUAGE RecordWildCards #-}
module Tests.Util where

import Control.Concurrent (forkIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, allocate)
import Control.Monad.Trans (lift)
import GHC.IO.Handle (hSetBinaryMode)
import qualified Data.ByteString as BS
import System.IO
import System.Process
import System.Directory (removeFile)

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
capnpEncode msgValue meta@MsgMetaData{..} = runResourceT $ do
    (hin, hout) <- interactCapnp "encode" meta
    lift $ do
        forkIO $ do
            hPutStr hin msgValue
            hClose hin
        hSetBinaryMode hout True
        BS.hGetContents hout

-- | @capnpDecode msg meta@ runs @capnp encode@ on the message, providing
-- the needed metadata and returning the output
capnpDecode :: BS.ByteString -> MsgMetaData -> IO String
capnpDecode msgValue meta@MsgMetaData{..} = runResourceT $ do
    schemaFile <- saveTmpSchema msgSchema
    (hin, hout) <- interactCapnp "decode" meta
    lift $ do
        forkIO $ do
            hSetBinaryMode hin True
            BS.hPutStr hin msgValue
            hClose hin
        hGetContents hout

-- | A helper for @capnpEncode@ and @capnpDecode@. Launches the capnp command
-- with the given subcommand (either "encode" or "decode") and metadata,
-- returning handles to its standard in and standard out. This runs inside
-- ResourceT, and sets the handles up to be freed when the ResourceT exits.
interactCapnp :: String -> MsgMetaData -> ResourceT IO (Handle, Handle)
interactCapnp subCommand MsgMetaData{..} = do
    schemaFile <- saveTmpSchema msgSchema
    let p = (proc "capnp" [ subCommand
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
    return (hin, hout)


saveTmpSchema msgSchema = snd <$> allocate writeTempFile removeFile
  where
    writeTempFile = runResourceT $ do
        (_, (path, hndl)) <- allocate
            (openTempFile "/tmp" "schema.capnp")
            (\(_, hndl) -> hClose hndl)
        lift $ hPutStr hndl msgSchema
        return path


-- | Convert a list of 'Assertion's to a test group with the given name.
assertionsToTest :: String -> [H.Assertion] -> Test
assertionsToTest name =
    testGroup name . hUnitTestToTests . H.TestList . map H.TestCase
