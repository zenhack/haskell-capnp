{-# LANGUAGE DoAndIfThenElse #-}
module Data.CapNProto.LowLevel
    ( Message
    , getMessage
    , defaultMaxMessageLen
    )
where

import Control.Monad (forM, void, when)
import Data.Array (Array)
import qualified Data.Array as A
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UA
import Data.Array.IArray (bounds, (!))
import Data.Binary.Get
    ( Get
    , getWord32le
    , getWord64le
    , getWord8
    )
import Data.Bits
import Data.Int
import Data.Word

data Message
    = Message !(Array Word32 Segment)
    deriving(Show, Eq)

data Segment
    = Segment !(UArray Word32 Word64)
    deriving(Show, Eq)


-- | An absolute address in a CapNProto message.
data Address = Address
    !Word32 -- ^ Remaining pointer depth; when this hits zero we get an error.
            -- See https://capnproto.org/encoding.html#security-considerations
            -- for reasoning
    !Message -- ^ The message
    !Word32 -- ^ word index
    !Word32 -- ^ segment index


-- | A (relative) pointer, as stored in the CapNProto messages themselves.
data Pointer
    = Struct !Int32 !Word16 !Word16
    | List !Int32 !ElementSize !Word32
    | Far !LandingSize !Word32 !Word32
    | Capability !Word32
    deriving(Show, Eq)


data ElementSize
    = Empty
    | Bit
    | Byte
    | TwoBytes
    | FourBytes
    | EightBytes
    | PointerSize
    | InlineComposite
    deriving(Show, Enum, Eq)


data LandingSize = OneWord | TwoWords
        deriving(Show, Eq, Enum)


data BoundsError
    = SegmentsBoundError -- ^ bad index into segments array
        !Word32 -- ^ index provided
        !Word32 -- ^ num segments
    | WordsBoundError -- ^ bad index into word array
        !Word32 -- ^ index provided
        !Word32 -- ^ num words
    | PointerDepthExceeded
    | MessageSizeExceeded
    deriving(Show)


defaultMaxMessageLen :: Word32
-- 64 MiB; this is what the C++ implementation does.
defaultMaxMessageLen = 64 `shiftL` 20


checkBounds :: Address -> Either BoundsError ()
checkBounds (Address depth (Message segs) wordsIdx segIdx) = do
    when (depth == 0) $ Left PointerDepthExceeded
    let (_, segsLen) = bounds segs
    when (segIdx >= segsLen) $ Left (SegmentsBoundError segIdx segsLen)
    let Segment words = segs ! segIdx
    let (_, wordsLen) = bounds words
    when (wordsIdx >= wordsLen) $ Left (WordsBoundError wordsIdx wordsLen)
    Right ()


pointerSeek :: Address -> Pointer -> Either BoundsError Address
pointerSeek
        (Address depth (Message segs) wordIdx segIdx)
        (Struct off dataSz ptrsSz) = do
    let result = Address
            (depth - 1)
            (Message segs)
            (fromIntegral (fromIntegral wordIdx + off))
            segIdx
    checkBounds result
    return result



bitRange :: (Integral a => Word64 -> Int -> Int -> a)
bitRange word lo hi = fromIntegral $ (.&.)
    (word `shiftR` lo)
    ((1 `shiftL` hi) - 1)


parsePointer :: Word64 -> Pointer
parsePointer word =
    case bitRange word 0 2 of
        0 -> Struct
            (bitRange word 2 32)
            (bitRange word 32 48)
            (bitRange word 48 64)
        1 -> List
            (bitRange word 2 32)
            (toEnum (bitRange word 32 35))
            (bitRange word 35 64)
        2 -> Far
            (toEnum (bitRange word 2 3))
            (bitRange word 3 32)
            (bitRange word 32 64)
        3 -> Capability (bitRange word 32 64)
{-
        0 -> Struct ((fromIntegral word :: Int32) `shiftR` 2)
                    (fromIntegral (word `shiftR` 32) :: Word16)
                    (fromIntegral (word `shiftR` 48) :: Word16)
        1 -> List ((fromIntegral word :: Int32) `shiftR` 2)
                  (toEnum $ fromIntegral ((word `shiftR` 32) `mod` 8))
                  (fromIntegral (word `shiftR` 35) :: Word32)
        2 -> Far ((word :: Int32) `shiftR` 2)
                 landingSize
              where landingSize =
                if testBit word 3
                    then TwoWords
                    else OneWord
        3 -> Capability $ fromIntegral (word `shiftR` 32)
-}


-- | @getMessage maxLen@ reads in a message formatted as described at
-- https://capnproto.org/encoding.html#serialization-over-a-stream.
-- If the total length of the message in 64-bit words is greater than
-- @maxLen@, this will return @Left MessageSizeExceeded@, otherwise it
-- will return a @Right@ carrying the message.
getMessage :: Word32 -> Get (Either BoundsError Message)
getMessage maxLen = do
    numSegs <- (1+) <$> getWord32le
    let paddingLen = (numSegs + 1) `mod` 2
    let numHeaderWords = (1 + numSegs + paddingLen) `div` 2
    if numHeaderWords `safeGt` maxLen then
        return $ Left MessageSizeExceeded
    else do
      segLengths <- getMany getWord32le numSegs
      if (sum $ numSegs:segLengths) `safeGt` maxLen then
           return $ Left MessageSizeExceeded
      else do
        void (getMany getWord32le paddingLen)
        segs <- forM segLengths $ \len ->
                     Segment <$> toArray UA.array len <$> getMany getWord64le len
        return $ Right $ Message $ toArray A.array numSegs segs
  where
    toArray arrayFn len lst = arrayFn (0, len-1) $ zip [0,1..] lst
    safeGt :: (Integral a) => a -> a -> Bool
    -- We can't risk overflow here, otherwise an attacker could e.g.
    -- specify (2**32 - 1) segments, and we'd overflow and treat this
    -- as an acceptable length. Instead we use Integer to avoid the possibility
    -- of overflow:
    safeGt l r = (fromIntegral l :: Integer) > (fromIntegral r :: Integer)


-- This is defined inside Data.Binary, but annoyingly not exported:
getMany :: (Eq n, Num n) => Get a -> n -> Get [a]
getMany g n = do getMany' [] n
  where
    getMany' xs 0 = return $ reverse $! xs
    getMany' xs n' = do
        x <- g
        x `seq` getMany' (x:xs) (n' - 1)
