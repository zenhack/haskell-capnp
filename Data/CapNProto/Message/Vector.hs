{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module: Data.CapNProto.Message.Vector
Description: Implementation of Message on top of Vectors.

-}
module Data.CapNProto.Message.Vector where

import Control.Monad (void, when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Quota (MonadQuota, invoice)
import qualified Data.CapNProto.Message as M
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64, Word32)

newtype Message = Message (B.Vector Segment) deriving(Show, Eq)
newtype Segment = Segment (U.Vector Word64) deriving(Show, Eq)

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

readMessage :: (MonadQuota m, MonadThrow m)
    => m Word64 -> m Word32 -> m Message
readMessage read64 read32 = do
    invoice 1
    numSegs <- read32
    invoice (fromIntegral numSegs `div` 2)
    segSizes <- B.replicateM (fromIntegral numSegs) read32
    when (numSegs `mod` 2 == 0) $ void read32
    B.mapM_ (invoice . fromIntegral) segSizes
    Message <$> B.mapM (readSegment . fromIntegral) segSizes
  where
    readSegment size = Segment <$> U.replicateM size read64

writeMessage :: (Monad m)
    => Message -> (Word64 -> m ()) -> (Word32 -> m ()) -> m ()
writeMessage (Message segs) write64 write32 = do
    let numSegs = B.length segs
    write32 (fromIntegral numSegs)
    B.forM_ segs $ \(Segment words) ->
        write32 $ fromIntegral $ U.length $ words
    when (numSegs `mod` 2 == 0) $ write32 0
    B.forM_ segs $ \(Segment words) -> U.mapM_ write64 words
