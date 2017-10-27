{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Util
    ( MsgMetaData(..)
    , capnpEncode, capnpDecode, capnpCompile, capnpId
    , assertionsToTest
    , freezeAsByteString
    )
  where

import System.IO
import System.Exit                    (ExitCode(..))
import System.Process

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
import GHC.IO.Handle                  (hSetBinaryMode)
import System.Directory               (removeFile)
import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
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
  (exitStatus, stdErr, contents) <- runResourceT $ do
    (hin, hout, herr, phandle) <- interactCapnpWithSchema "encode" (msgSchema meta) [msgType meta]
    contents <- lift $ do
      forkIO $ do
        hPutStr hin msgValue
        hClose hin
      hSetBinaryMode hout True
      BS.hGetContents hout
    stdErr <- lift $ BS.hGetContents herr
    exitStatus <- lift $ waitForProcess phandle
    return (exitStatus, stdErr, contents)
  case exitStatus of
    ExitSuccess -> return contents
    ExitFailure code -> fail ("`capnp encode` failed with exit code " ++ show code ++ ":\n" ++ show stdErr)

-- | @capnpDecode msg meta@ runs @capnp decode@ on the message, providing
-- the needed metadata and returning the output
capnpDecode :: BS.ByteString -> MsgMetaData -> IO String
capnpDecode encodedMsg meta = do
  (exitStatus, stdErr, contents) <- runResourceT $ do
    (hin, hout, herr, phandle) <- interactCapnpWithSchema "decode" (msgSchema meta) [msgType meta]
    contents <- lift $ do
      forkIO $ do
        hSetBinaryMode hin True
        BS.hPutStr hin encodedMsg
        hClose hin
      ret <- hGetContents hout
      -- We need to read the whole string in strictly, otherwise hClose may
      -- happen before we're done; use deepseq to force full evaluation:
      deepseq ret (return ret)
    stdErr <- lift $ BS.hGetContents herr
    exitStatus <- lift $ waitForProcess phandle
    return (exitStatus, stdErr, contents)
  case exitStatus of
    ExitSuccess -> return contents
    ExitFailure code -> fail ("`capnp decode` failed with exit code " ++ show code ++ ":\n" ++ show stdErr)

-- | @capnpCompile msg meta@ runs @capnp compile@ on the schema, providing
-- the needed metadata and returning the output
capnpCompile :: String -> String -> IO BS.ByteString
capnpCompile msgSchema outputArg = do
  (exitStatus, stdErr, contents) <- runResourceT $ do
    (hin, hout, herr, phandle) <- interactCapnpWithSchema "compile" msgSchema [outputArg]
    contents <- lift $ do
      hSetBinaryMode hout True
      BS.hGetContents hout
    stdErr <- lift $ BS.hGetContents herr
    exitStatus <- lift $ waitForProcess phandle
    return (exitStatus, stdErr, contents)
  case exitStatus of
    ExitSuccess -> return contents
    ExitFailure code -> fail ("`capnp compile` failed with exit code " ++ show code ++ ":\n" ++ show stdErr)

capnpId :: IO BS.ByteString
capnpId = do
  (exitStatus, stdErr, cid) <- runResourceT $ do
    (hin, hout, herr, phandle) <- interactCapnp ["id"]
    contents <- lift $ BS.hGetContents hout
    stdErr <- lift $ BS.hGetContents herr
    exitStatus <- lift $ waitForProcess phandle
    return (exitStatus, stdErr, contents)
  case exitStatus of
    ExitFailure code -> fail ("`capnp id` failed with exit code " ++ show code ++ ":\n" ++ show stdErr)
    ExitSuccess -> case BSC8.stripSuffix "\n" cid of
      Just stripped -> do
        when (BSC8.length stripped /= 19)
          $ fail "`capnp id` had the wrong length (should be 19)"
        when (BSC8.take 3 stripped /= "@0x")
          $ fail "`capnp id` was not in the expected format (@0x...)"
        when (BSC8.dropWhile isHexDigit (BSC8.drop 3 stripped) /= "")
          $ fail "`capnp id` was not in the expected format (hexadecimals)"
        return stripped
      Nothing -> fail "`capnp id` did not end with a newline"

interactCapnp :: [String] -> ResourceT IO (Handle, Handle, Handle, ProcessHandle)
interactCapnp args = do
  let p = (proc "capnp" args) { std_in = CreatePipe
                              , std_out = CreatePipe
                              , std_err = CreatePipe
                              }
  (_, (Just hin, Just hout, Just herr, phandle)) <- allocate
      (createProcess p)
      (\(Just hin, Just hout, Just herr, proc) -> do
          hClose hout
          hClose hin
          hClose herr
          void $ waitForProcess proc)
  return (hin, hout, herr, phandle)

-- | A helper for @capnpEncode@ and @capnpDecode@. Launches the capnp command
-- with the given subcommand (either "encode" or "decode") and metadata,
-- returning handles to its standard in and standard out. This runs inside
-- ResourceT, and sets the handles up to be closed and the process to be reaped
-- when the ResourceT exits.
interactCapnpWithSchema :: String -> String -> [String] -> ResourceT IO (Handle, Handle, Handle, ProcessHandle)
interactCapnpWithSchema subCommand msgSchema args = do
  let writeTempFile = runResourceT $ do
        (_, (path, hndl)) <- allocate
            (openTempFile "/tmp" "schema.capnp")
            (\(_, hndl) -> hClose hndl)
        lift $ hPutStr hndl msgSchema
        return path
  let saveTmpSchema msgSchema = snd <$> allocate writeTempFile removeFile
  schemaFile <- saveTmpSchema msgSchema
  interactCapnp $ [subCommand, schemaFile] ++ args

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
