{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Capnp.IO
-- Description: Utilities for reading and writing values to handles.
--
-- This module provides utilities for reading and writing values to and
-- from file 'Handle's.
module Capnp.IO
  ( sGetMsg,
    sPutMsg,
    M.hGetMsg,
    M.getMsg,
    M.hPutMsg,
    M.putMsg,
    hGetParsed,
    sGetParsed,
    getParsed,
    hPutParsed,
    sPutParsed,
    putParsed,
    hGetRaw,
    getRaw,
    sGetRaw,
  )
where

import Capnp.Bits (WordCount, wordsToBytes)
import Capnp.Convert
  ( msgToLBS,
    msgToParsed,
    msgToRaw,
    parsedToBuilder,
    parsedToLBS,
  )
import Capnp.Message (Mutability (..))
import qualified Capnp.Message as M
import Capnp.New.Classes (Parse)
import qualified Capnp.Repr as R
import Capnp.TraversalLimit (evalLimitT)
import Control.Exception (throwIO)
import Control.Monad.Trans.Class (lift)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import Network.Simple.TCP (Socket, recv, sendLazy)
import System.IO (Handle, stdin, stdout)
import System.IO.Error (eofErrorType, mkIOError)

-- | Like 'hGetMsg', except that it takes a socket instead of a 'Handle'.
sGetMsg :: Socket -> WordCount -> IO (M.Message 'Const)
sGetMsg socket limit =
  evalLimitT limit $ M.readMessage (lift read32) (lift . readSegment)
  where
    read32 = do
      bytes <- recvFull 4
      pure $
        (fromIntegral (bytes `BS.index` 0) `shiftL` 0)
          .|. (fromIntegral (bytes `BS.index` 1) `shiftL` 8)
          .|. (fromIntegral (bytes `BS.index` 2) `shiftL` 16)
          .|. (fromIntegral (bytes `BS.index` 3) `shiftL` 24)
    readSegment !words =
      M.fromByteString <$> recvFull (fromIntegral $ wordsToBytes words)

    -- \| Like recv, but (1) never returns less than `count` bytes, (2)
    -- uses `socket`, rather than taking the socket as an argument, and (3)
    -- throws an EOF exception when the connection is closed.
    recvFull :: Int -> IO BS.ByteString
    recvFull !count = do
      maybeBytes <- recv socket count
      case maybeBytes of
        Nothing ->
          throwIO $ mkIOError eofErrorType "Remote socket closed" Nothing Nothing
        Just bytes
          | BS.length bytes == count ->
              pure bytes
          | otherwise ->
              (bytes <>) <$> recvFull (count - BS.length bytes)

-- | Like 'hPutMsg', except that it takes a 'Socket' instead of a 'Handle'.
sPutMsg :: Socket -> M.Message 'Const -> IO ()
sPutMsg socket = sendLazy socket . msgToLBS

-- | Read a struct from the handle in its parsed form, using the supplied
-- read limit.
hGetParsed :: forall a pa. (R.IsStruct a, Parse a pa) => Handle -> WordCount -> IO pa
hGetParsed handle limit = do
  msg <- M.hGetMsg handle limit
  evalLimitT limit $ msgToParsed @a msg

-- | Read a struct from the socket in its parsed form, using the supplied
-- read limit.
sGetParsed :: forall a pa. (R.IsStruct a, Parse a pa) => Socket -> WordCount -> IO pa
sGetParsed socket limit = do
  msg <- sGetMsg socket limit
  evalLimitT limit $ msgToParsed @a msg

-- | Read a struct from stdin in its parsed form, using the supplied
-- read limit.
getParsed :: (R.IsStruct a, Parse a pa) => WordCount -> IO pa
getParsed = hGetParsed stdin

-- | Write the parsed form of a struct to the handle
hPutParsed :: (R.IsStruct a, Parse a pa) => Handle -> pa -> IO ()
hPutParsed h value = do
  bb <- evalLimitT maxBound $ parsedToBuilder value
  BB.hPutBuilder h bb

-- | Write the parsed form of a struct to stdout
putParsed :: (R.IsStruct a, Parse a pa) => pa -> IO ()
putParsed = hPutParsed stdout

-- | Write the parsed form of a struct to the socket.
sPutParsed :: (R.IsStruct a, Parse a pa) => Socket -> pa -> IO ()
sPutParsed socket value = do
  lbs <- evalLimitT maxBound $ parsedToLBS value
  sendLazy socket lbs

-- | Read a struct from the handle using the supplied read limit,
-- and return its root pointer.
hGetRaw :: R.IsStruct a => Handle -> WordCount -> IO (R.Raw a 'Const)
hGetRaw h limit = do
  msg <- M.hGetMsg h limit
  evalLimitT limit $ msgToRaw msg

-- | Read a struct from stdin using the supplied read limit,
-- and return its root pointer.
getRaw :: R.IsStruct a => WordCount -> IO (R.Raw a 'Const)
getRaw = hGetRaw stdin

-- | Read a struct from the socket using the supplied read limit,
-- and return its root pointer.
sGetRaw :: R.IsStruct a => Socket -> WordCount -> IO (R.Raw a 'Const)
sGetRaw socket limit = do
  msg <- sGetMsg socket limit
  evalLimitT limit $ msgToRaw msg
