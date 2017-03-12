{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module: Data.CapNProto.Message.Vector
Description: Implementation of Message on top of Vectors.

-}
module Data.CapNProto.Message.Vector where

import qualified Data.CapNProto.Message as M
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)

newtype Message = Message (B.Vector Segment)
newtype Segment = Segment (U.Vector Word64)

instance M.Array Message Segment where
    length (Message v) = B.length v
    lookup i msg@(Message v) = do
        M.checkBounds msg i
        return (v B.! i)

instance M.Array Segment Word64 where
    length (Segment v) = U.length v
    lookup i seg@(Segment v) = do
        M.checkBounds seg i
        return (v U.! i)

instance (M.Message Message Segment)
