{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-|
Module: Data.CapNProto.Message
Description: Tools for working with messages.

-}
module Data.CapNProto.Message
    ( Message
    , getSegment
    , getWord
    , decode
    , readMessage
    , writeMessage
    )
  where

import Control.Monad             (replicateM, void, when)
import Control.Monad.Catch       (MonadThrow, throwM)
import Control.Monad.Quota       (MonadQuota, Quota(..), evalQuotaT, invoice)
import Control.Monad.State       (evalStateT, get, put)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.CapNProto.Address    (WordAddr(..))
import Data.CapNProto.Bits       (WordCount(..), hi, lo, wordsToBytes)
import Data.CapNProto.Errors     (BoundsError(..))
import Data.Word                 (Word32, Word64)

import qualified Data.CapNProto.Blob as B

import Control.Monad.ST (ST, runST)

import qualified Data.Primitive.Array as A
import qualified GHC.Exts
import qualified GHC.Prim

emptyArray :: A.Array a
emptyArray = runST (A.newArray 0 (error "emptyArray: impossible")
                    >>= A.unsafeFreezeArray)

createArray :: Int
            -- ^ array length
            -> a
            -- ^ default value to fill array with
            -> (forall s. A.MutableArray s a -> ST s ())
            -- ^ function that mutates the array
            -> A.Array a
            -- ^ the resulting frozen array
createArray n x f
  | n < 0     = error "createArray: negative index!"
  | n == 0    = emptyArray
  | otherwise = runST $ do mutArr <- A.newArray n x
                           f mutArr
                           A.unsafeFreezeArray mutArr

arraySize :: A.Array a -> Int
arraySize (A.Array unboxed) = GHC.Exts.I# (GHC.Prim.sizeofArray# unboxed)

arrayFromListN :: Int -> [a] -> A.Array a
arrayFromListN n list
  = createArray n (error "arrayFromListN: mismatched size and list")
    $ \mi -> let go i []     = pure ()
                 go i (x:xs) = A.writeArray mi i x >> go (i + 1) xs
             in go 0 list

arrayToList :: A.Array a -> [a]
arrayToList arr
  = GHC.Exts.build
    $ \c z -> let sz = arraySize arr
                  go i | i < sz    = c (A.indexArray arr i) (go (i + 1))
                       | otherwise = z
              in go 0

arrayTraverse :: (Applicative f) => (a -> f b) -> A.Array a -> f (A.Array b)
arrayTraverse f arr = let sz = arraySize arr
                      in arrayFromListN sz
                         <$> traverse (f . A.indexArray arr) [0 .. sz - 1]

arrayReplicateM :: (Monad m) => Int -> m a -> m (A.Array a)
arrayReplicateM n action = arrayFromListN n <$> replicateM n action

arrayMapM :: (Monad m) => (a -> m b) -> A.Array a -> m (A.Array b)
arrayMapM = arrayTraverse

arrayMapM_ :: (Monad m) => (a -> m b) -> A.Array a -> m ()
arrayMapM_ f arr = arrayMapM f arr >> pure () -- FIXME

arrayForM :: (Monad m) => A.Array a -> (a -> m b) -> m (A.Array b)
arrayForM = flip arrayMapM

arrayForM_ :: (Monad m) => A.Array a -> (a -> m b) -> m ()
arrayForM_ = flip arrayMapM_

newtype Message a = Message (A.Array a)

instance (Show a) => Show (Message a) where
  show (Message arr) = "<<message>>"

-- | @getSegment msg i@ gets the ith segment of a message. Throws a
-- 'BoundsError' if @i@ is out of bounds.
getSegment :: (MonadThrow m) => Message a -> Int -> m a
getSegment (Message segs) i = do
    when (i < 0 || i >= arraySize segs) $ do
        throwM (BoundsError { index = i, maxIndex = arraySize segs })
    segs `A.indexArrayM` i

-- | @getWord msg addr@ returns the word at @addr@ within @msg@. It throws a
-- @BoundsError@ if the address is out of bounds.
getWord :: (B.Blob m seg, MonadThrow m)
        => Message seg -> WordAddr -> m Word64
getWord (Message segs) WordAt{..} = do
    seg <- segs `A.indexArrayM` segIndex
    seg `B.indexWord` wordIndex

-- | @decode blob@ decodes a message from the blob.
--
-- The segments will not be copied; the resulting message will be a view into
-- the original blob.
decode :: (B.Slice m b, B.Blob m b, MonadThrow m) => b -> m (Message b)
decode blob = do
    -- Note: we use the quota to avoid needing to do bounds checking here;
    -- since readMessage invoices the quota before reading, we can rely on it
    -- not to read past the end of the blob.
    WordCount blobLen <- B.lengthInWords blob
    flip evalStateT (Nothing, 0) $ flip evalQuotaT (Quota blobLen) $
        readMessage read32 readSegment
  where
    bIndex b i = lift $ lift $ B.indexWord b i
    read32 = do
        (cur, idx) <- get
        case cur of
            Just n -> do
                put (Nothing, idx)
                return n
            Nothing -> do
                word <- bIndex blob idx
                put (Just $ hi word, idx + 1)
                return (lo word)
    readSegment len = do
        (cur, idx) <- get
        put (cur, idx + len)
        lift $ lift $ B.slice blob (wordsToBytes idx) (wordsToBytes len)

-- | @readMessage read32 readSegment@ reads in a message using the
-- monadic context, which should manage the current read position
-- into a message. read32 should read a 32-bit little-endian integer,
-- and @readSegment n@ should read a blob of @n@ 64-bit words.
-- The size of the message (in 64-bit words) is deducted from the quota,
-- which can be used to set the maximum message size.
readMessage :: (MonadQuota m, MonadThrow m)
            => m Word32 -> (WordCount -> m b) -> m (Message b)
readMessage read32 readSegment = do
    invoice 1
    numSegs' <- read32
    let numSegs = numSegs' + 1
    invoice (fromIntegral numSegs `div` 2)
    segSizes <- arrayReplicateM (fromIntegral numSegs) read32
    when (numSegs `mod` 2 == 0) $ void read32
    arrayMapM_ (invoice . fromIntegral) segSizes
    Message <$> arrayMapM (readSegment . fromIntegral) segSizes

-- | @writeMesage write32 writeSegment@ writes out the message. @write32@
-- should write a 32-bit word in little-endian format to the output stream.
-- @writeSegment@ should write a blob.
writeMessage :: (Monad m, B.Blob m b)
             => Message b -> (Word32 -> m ()) -> (b -> m ()) -> m ()
writeMessage (Message msg) write32 writeSegment = do
    let numSegs = arraySize msg
    write32 (fromIntegral numSegs - 1)
    arrayForM_ msg $ \seg -> write32 =<< fromIntegral <$> B.length seg
    when (numSegs `mod` 2 == 0) $ write32 0
    arrayForM_ msg writeSegment
