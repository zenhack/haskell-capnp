{- |
Module: Data.Capnp.IO
Description: Utilities for reading and writing values to handles.
-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Capnp.IO
    ( hGetValue
    , getValue
    , hPutValue
    , putValue
    ) where

import Control.Monad.Primitive (RealWorld)
import System.IO               (Handle, stdin, stdout)

import Codec.Capnp
    ( Cerialize(..)
    , Decerialize(..)
    , FromStruct(..)
    , ToStruct(..)
    , getRoot
    , setRoot
    )
import Data.Capnp.TraversalLimit (evalLimitT)

import qualified Data.Capnp.Message as M

-- | @'hGetValue' limit handle@ reads a message from @handle@, returning its root object.
-- @limit@ is used as both a cap on the size of a message which may be read and, for types
-- in the high-level API, the traversal limit when decoding the message.
hGetValue :: FromStruct M.ConstMsg a => Handle -> Int -> IO a
hGetValue handle limit = do
    msg <- M.hGetMsg handle limit
    evalLimitT limit (getRoot msg)

-- | @'getValue'@ is equivalent to @'hGetValue' 'stdin'@.
getValue :: FromStruct M.ConstMsg a => Int -> IO a
getValue = hGetValue stdin

-- | @'hPutValue' handle value@ writes @value@ to handle, as the root object of
-- a message.
hPutValue :: (Cerialize RealWorld a, ToStruct (M.MutMsg RealWorld) (Cerial (M.MutMsg RealWorld) a))
    => Handle -> a -> IO ()
hPutValue handle value = do
    msg <- M.newMessage
    root <- evalLimitT maxBound $ cerialize msg value
    setRoot root
    constMsg <- M.freeze msg
    M.hPutMsg handle constMsg

-- | 'putValue' is equivalent to @'hPutValue' 'stdin'@
putValue :: (Cerialize RealWorld a, ToStruct (M.MutMsg RealWorld) (Cerial (M.MutMsg RealWorld) a))
    => a -> IO ()
putValue = hPutValue stdout
