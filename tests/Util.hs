{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
module Util
    ( MsgMetaData(..)
    , capnpEncode, capnpDecode, capnpCompile
    , decodeValue
    , encodeValue
    , aircraftSchemaSrc
    , schemaSchemaSrc
    )
    where

import System.Process hiding (readCreateProcessWithExitCode)

import System.IO

import Control.Monad.Trans            (lift)
import Control.Monad.Trans.Resource   (ResourceT, allocate, runResourceT)
import System.Directory               (removeFile)
import System.Exit                    (ExitCode (..))
import System.Process.ByteString.Lazy (readCreateProcessWithExitCode)
import Text.Heredoc                   (there)

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC8

import qualified Capnp.Message as M

aircraftSchemaSrc, schemaSchemaSrc :: String
aircraftSchemaSrc = [there|tests/data/aircraft.capnp|]
schemaSchemaSrc = [there|tests/data/schema.capnp|]

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
    (exitStatus, stdOut, stdErr) <- runResourceT $
        interactCapnpWithSchema "encode" (msgSchema meta) (LBSC8.pack msgValue) [msgType meta]
    case exitStatus of
        ExitSuccess -> return (LBS.toStrict stdOut)
        ExitFailure code -> fail ("`capnp encode` failed with exit code " ++ show code ++ ":\n" ++ show stdErr)

-- | @capnpDecode msg meta@ runs @capnp decode@ on the message, providing
-- the needed metadata and returning the output
capnpDecode :: BS.ByteString -> MsgMetaData -> IO String
capnpDecode encodedMsg meta = do
    (exitStatus, stdOut, stdErr) <- runResourceT $
        interactCapnpWithSchema "decode" (msgSchema meta) (LBS.fromStrict encodedMsg) [msgType meta]
    case exitStatus of
        ExitSuccess -> return (LBSC8.unpack stdOut)
        ExitFailure code -> fail ("`capnp decode` failed with exit code " ++ show code ++ ":\n" ++ show stdErr)

-- | @capnpCompile msg meta@ runs @capnp compile@ on the schema, providing
-- the needed metadata and returning the output
capnpCompile :: String -> String -> IO BS.ByteString
capnpCompile msgSchema outputArg = do
    (exitStatus, stdOut, stdErr) <- runResourceT $
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
    schemaFile <- snd <$> allocate writeTempFile removeFile
    lift $ readCreateProcessWithExitCode (proc "capnp" ([subCommand, schemaFile] ++ args)) stdInBytes

-- | @'decodeValue' schema typename message@ decodes the value at the root of
-- the message and converts it to text. This is a thin wrapper around
-- 'capnpDecode'.
decodeValue :: String -> String -> M.ConstMsg -> IO String
decodeValue schema typename msg = do
    bytes <- M.encode msg
    capnpDecode
        (LBS.toStrict $ BB.toLazyByteString bytes)
        (MsgMetaData schema typename)

-- | @'encodeValue' schema typename value@ encodes the textual value @value@
-- as a capnp message. This is a thin wrapper around 'capnpEncode'.
encodeValue :: String -> String -> String -> IO M.ConstMsg
encodeValue schema typename value =
    let meta = MsgMetaData schema typename
    in capnpEncode value meta >>= M.decode
