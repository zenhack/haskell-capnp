{- |
Module: Capnp.IO
Description: Utilities for reading and writing values to handles.

This module provides utilities for reading and writing values to and
from file 'Handle's.
-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
module Capnp.IO
    ( hGetValue
    , getValue
    , sGetMsg
    , sGetValue
    , hPutValue
    , putValue
    , sPutValue
    , sPutMsg
    , M.hGetMsg
    , M.getMsg
    , M.hPutMsg
    , M.putMsg
    ) where

import Data.Bits

import Control.Exception         (throwIO)
import Control.Monad.Primitive   (RealWorld)
import Control.Monad.Trans.Class (lift)
import Network.Simple.TCP        (Socket, recv, sendLazy)
import System.IO                 (Handle, stdin, stdout)
import System.IO.Error           (eofErrorType, mkIOError)

import qualified Data.ByteString as BS

import Capnp.Bits           (wordsToBytes)
import Capnp.Classes
    (Cerialize(..), Decerialize(..), FromStruct(..), ToStruct(..))
import Capnp.Convert        (msgToLBS, valueToLBS)
import Capnp.TraversalLimit (evalLimitT)
import Codec.Capnp          (getRoot, setRoot)
import Data.Mutable         (Thaw(..))

import qualified Capnp.Message as M

-- | @'hGetValue' limit handle@ reads a message from @handle@, returning its root object.
-- @limit@ is used as both a cap on the size of a message which may be read and, for types
-- in the high-level API, the traversal limit when decoding the message.
--
-- It may throw a 'Capnp.Errors.Error' if there is a problem decoding the message,
-- or an 'IOError' raised by the underlying IO libraries.
hGetValue :: FromStruct M.ConstMsg a => Handle -> Int -> IO a
hGetValue handle limit = do
    msg <- M.hGetMsg handle limit
    evalLimitT limit (getRoot msg)

-- | @'getValue'@ is equivalent to @'hGetValue' 'stdin'@.
getValue :: FromStruct M.ConstMsg a => Int -> IO a
getValue = hGetValue stdin

-- | Like 'hGetValue', except that it takes a socket instead of a 'Handle'.
sGetValue :: FromStruct M.ConstMsg a => Socket -> Int -> IO a
sGetValue socket limit = do
    msg <- sGetMsg socket limit
    evalLimitT limit (getRoot msg)

-- | Like 'hGetMsg', except that it takes a socket instead of a 'Handle'.
sGetMsg :: Socket -> Int -> IO M.ConstMsg
sGetMsg socket limit =
    evalLimitT limit $ M.readMessage (lift read32) (lift . readSegment)
  where
    read32 = do
        bytes <- recvFull 4
        pure $
            (fromIntegral (bytes `BS.index` 0) `shiftL`  0) .|.
            (fromIntegral (bytes `BS.index` 1) `shiftL`  8) .|.
            (fromIntegral (bytes `BS.index` 2) `shiftL` 16) .|.
            (fromIntegral (bytes `BS.index` 3) `shiftL` 24)
    readSegment !words = do
        bytes <- recvFull (fromIntegral $ wordsToBytes words)
        M.fromByteString bytes

    -- | Like recv, but (1) never returns less than `count` bytes, (2)
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

-- | @'hPutValue' handle value@ writes @value@ to handle, as the root object of
-- a message. If it throws an exception, it will be an 'IOError' raised by the
-- underlying IO libraries.
hPutValue :: (Cerialize RealWorld a, ToStruct (M.MutMsg RealWorld) (Cerial (M.MutMsg RealWorld) a))
    => Handle -> a -> IO ()
hPutValue handle value = do
    msg <- M.newMessage
    root <- evalLimitT maxBound $ cerialize msg value
    setRoot root
    constMsg <- freeze msg
    M.hPutMsg handle constMsg

-- | 'putValue' is equivalent to @'hPutValue' 'stdin'@
putValue :: (Cerialize RealWorld a, ToStruct (M.MutMsg RealWorld) (Cerial (M.MutMsg RealWorld) a))
    => a -> IO ()
putValue = hPutValue stdout

-- | Like 'hPutMsg', except that it takes a 'Socket' instead of a 'Handle'.
sPutMsg :: Socket -> M.ConstMsg -> IO ()
sPutMsg socket = sendLazy socket . msgToLBS

-- | Like 'hPutValue', except that it takes a 'Socket' instead of a 'Handle'.
sPutValue :: (Cerialize RealWorld a, ToStruct (M.MutMsg RealWorld) (Cerial (M.MutMsg RealWorld) a))
    => Socket -> a -> IO ()
sPutValue socket value = do
    lbs <- evalLimitT maxBound $ valueToLBS value
    sendLazy socket lbs
