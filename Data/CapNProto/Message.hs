{-# LANGUAGE MultiParamTypeClasses, RecordWildCards #-}
{-|
Module: Data.CapNProto.Message
Description: Tools for working with messages.

-}
module Data.CapNProto.Message where

import Control.Monad (when, void)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Quota (MonadQuota, invoice)
import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import Data.CapNProto.Address (WordAddr(..))
import Data.CapNProto.Errors (BoundsError(..))
import Data.Word (Word64, Word32)

-- | A CapNProto message
class (Vector msg (seg Word64), Vector seg Word64) => Message msg seg

instance Message B.Vector U.Vector

-- | @checkBounds arr i@ verifies that @i@ is a legal index into @arr@,
-- calling throwing a @BoundsError@ if not.
checkBounds :: (MonadThrow m, Vector v a) => v a -> Int -> m ()
checkBounds vec i = when (i < 0 || i >= V.length vec) $
    throwM $ BoundsError { index = i, maxIndex = V.length vec }

-- | @getWord addr@ returns the word at @addr@ within @msg@. It deducts
-- 1 from the quota, and throws a @BoundsError@ if the address is out of
-- bounds.
getWord :: (Message msg seg, MonadThrow m, MonadQuota m)
    => WordAddr -> msg (seg Word64) -> m Word64
getWord WordAt{..} msg = do
    invoice 1
    checkBounds msg segIndex
    let seg = msg ! segIndex
    checkBounds seg wordIndex
    return $ seg ! wordIndex

-- | @readMessage read64 read32@ reads in a message using read64 to
-- read 64-bit words and read32 to read 32-bit words. Per the spec,
-- words are little-endian. It decucts the size of the message (in
-- 64-bit words) from the quota.
readMessage :: (MonadQuota m, MonadThrow m, Message msg seg, Vector msg Word32)
    => m Word64 -> m Word32 -> m (msg (seg Word64))
    -- TODO: having the @Vector msg Word32@ constraint above feels clumbsy;
    -- it's due to an implementation detail that I'd rather not expose int he
    -- type.
readMessage read64 read32 = do
    invoice 1
    numSegs' <- read32
    let numSegs = numSegs' + 1
    invoice (fromIntegral numSegs `div` 2)
    segSizes <- V.replicateM (fromIntegral numSegs) read32
    when (numSegs `mod` 2 == 0) $ void read32
    V.mapM_ (invoice . fromIntegral) segSizes
    V.mapM (readSegment . fromIntegral) segSizes
  where
    readSegment size = V.replicateM size read64

-- | @writeMesage write64 write32@ writes out the message
writeMessage :: (Monad m, Message msg seg)
    => (msg (seg Word64)) -> (Word64 -> m ()) -> (Word32 -> m ()) -> m ()
writeMessage msg write64 write32 = do
    let numSegs = V.length msg
    write32 (fromIntegral numSegs)
    V.forM_ msg $ \seg -> write32 $ fromIntegral $ V.length $ seg
    when (numSegs `mod` 2 == 0) $ write32 0
    V.forM_ msg $ \seg -> V.mapM_ write64 seg
