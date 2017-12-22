{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Util
    ( MsgMetaData(..)
    , capnpEncode, capnpDecode, capnpCompile
    , assertionsToTest
    , freezeAsByteString
    )
    where

import System.IO
import System.Exit                    (ExitCode(..))
import System.Process                 hiding (readCreateProcessWithExitCode)
import System.Process.ByteString.Lazy (readCreateProcessWithExitCode)

import Control.Concurrent             (forkIO)
import Control.DeepSeq                (deepseq)
import Control.Monad                  (void, when)
import Control.Monad.Primitive        (PrimMonad, PrimState)
import Control.Monad.Trans            (lift)
import Control.Monad.Trans.Resource   (ResourceT, allocate, runResourceT)
import Data.CapNProto.Bits            (ByteCount(..))
import Data.CapNProto.Blob            (BlobSlice(..))
import Data.Char                      (isHexDigit)
import Data.Primitive.ByteArray       (MutableByteArray, readByteArray)
import System.Directory               (removeFile)
import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Test.HUnit      as H

-- | Information about the contents of a capnp message. This is enough
-- to encode/decode both textual and binary forms.
data MsgMetaData = MsgMetaData
    { msgSchema :: String -- ^ The source of the schema
    , msgType   :: String -- ^ The name of the root struct's type
    } deriving(Show)

-- | @capnpEncode msg meta@ runs @capnp encode@ on the message, providing
-- the needed metadata and returning the output
capnpEncode :: String -> MsgMetaData -> IO BS.ByteString
capnpEncode msgValue meta = do
    (exitStatus, stdOut, stdErr) <- runResourceT $ do
        interactCapnpWithSchema "encode" (msgSchema meta) (LBSC8.pack msgValue) [msgType meta]
    case exitStatus of
        ExitSuccess -> return (LBS.toStrict stdOut)
        ExitFailure code -> fail ("`capnp encode` failed with exit code " ++ show code ++ ":\n" ++ show stdErr)

-- | @capnpDecode msg meta@ runs @capnp decode@ on the message, providing
-- the needed metadata and returning the output
capnpDecode :: BS.ByteString -> MsgMetaData -> IO String
capnpDecode encodedMsg meta = do
    (exitStatus, stdOut, stdErr) <- runResourceT $ do
        interactCapnpWithSchema "decode" (msgSchema meta) (LBS.fromStrict encodedMsg) [msgType meta]
    case exitStatus of
        ExitSuccess -> return (LBSC8.unpack stdOut)
        ExitFailure code -> fail ("`capnp decode` failed with exit code " ++ show code ++ ":\n" ++ show stdErr)

-- | @capnpCompile msg meta@ runs @capnp compile@ on the schema, providing
-- the needed metadata and returning the output
capnpCompile :: String -> String -> IO BS.ByteString
capnpCompile msgSchema outputArg = do
    (exitStatus, stdOut, stdErr) <- runResourceT $ do
        interactCapnpWithSchema "compile" msgSchema LBSC8.empty ["-o", outputArg]
    case exitStatus of
        ExitSuccess -> return (LBS.toStrict stdOut)
        ExitFailure code -> fail ("`capnp compile` failed with exit code " ++ show code ++ ":\n" ++ show stdErr)

-- | A helper for @capnpEncode@ and @capnpDecode@. Launches the capnp command
-- with the given subcommand (either "encode" or "decode") and metadata,
-- returning handles to its standard in and standard out. This runs inside
-- ResourceT, and sets the handles up to be closed and the process to be reaped
-- when the ResourceT exits.
interactCapnpWithSchema :: String -> String -> LBS.ByteString -> [String] -> ResourceT IO (ExitCode, LBS.ByteString, LBS.ByteString)
interactCapnpWithSchema subCommand msgSchema stdInBytes args = do
    let writeTempFile = runResourceT $ do
            (_, (path, hndl)) <- allocate
                (openTempFile "/tmp" "schema.capnp")
                (\(_, hndl) -> hClose hndl)
            lift $ hPutStr hndl msgSchema
            return path
    let saveTmpSchema msgSchema = snd <$> allocate writeTempFile removeFile
    schemaFile <- saveTmpSchema msgSchema
    lift $ readCreateProcessWithExitCode (proc "capnp" ([subCommand, schemaFile] ++ args)) stdInBytes

-- | Convert a BlobSlice (MutableByteArray s) to a ByteString. The former is the
-- result of Control.Monad.CapNProto.MessageBuilder.runBuilderT
freezeAsByteString :: (PrimMonad m, s ~ PrimState m)
    => BlobSlice (MutableByteArray s) -> m BS.ByteString
freezeAsByteString BlobSlice{..} = do
    let ByteCount off = offset
    let ByteCount len = sliceLen
    BS.pack <$> mapM (readByteArray blob) [off..off+len-1]

-- | Convert a list of 'Assertion's to a test group with the given name.
assertionsToTest :: String -> [H.Assertion] -> Test
assertionsToTest name =
    testGroup name . hUnitTestToTests . H.TestList . map H.TestCase
