{- |
Module: Data.Capnp.IO
Description: Utilities for reading and writing values to handles.

This module provides utilities for reading and writing values to and
from file 'Handle's.
-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Capnp.IO
    ( hGetValue
    , getValue
    -- , sGetValue
    , hPutValue
    , putValue
    , sPutValue
    , M.hGetMsg
    , M.getMsg
    , M.hPutMsg
    , M.putMsg
    ) where

import Control.Monad.Primitive (RealWorld)
import Network.Simple.TCP      (Socket, sendLazy)
import System.IO               (Handle, stdin, stdout)

import Codec.Capnp               (getRoot, setRoot)
import Data.Capnp.Classes
    (Cerialize(..), Decerialize(..), FromStruct(..), ToStruct(..))
import Data.Capnp.Convert        (valueToLBS)
import Data.Capnp.TraversalLimit (evalLimitT)
import Data.Mutable              (Thaw(..))

import qualified Data.Capnp.Message as M

-- | @'hGetValue' limit handle@ reads a message from @handle@, returning its root object.
-- @limit@ is used as both a cap on the size of a message which may be read and, for types
-- in the high-level API, the traversal limit when decoding the message.
--
-- It may throw a 'Data.Capnp.Errors.Error' if there is a problem decoding the message,
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
sGetValue socket limit =
    error "TODO"

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

-- | Like 'hPutValue', except that it takes a 'Socket' instead of a 'Handle'.
sPutValue :: (Cerialize RealWorld a, ToStruct (M.MutMsg RealWorld) (Cerial (M.MutMsg RealWorld) a))
    => Socket -> a -> IO ()
sPutValue socket value = do
    lbs <- evalLimitT maxBound $ valueToLBS value
    sendLazy socket lbs
