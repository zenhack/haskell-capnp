module Encoding where

import Data.Binary
import Data.Binary.Get
import Data.Array (Array)
import qualified Data.Array as A
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UA
import Data.Array.IArray ((!), bounds)
import qualified Data.Array.IArray as IA
import Data.Bits
import Data.Word
import Data.Int
import Control.Monad (when, forM)

data Message = Message (Array Word32 Segment)
             deriving(Show)

data Segment = Segment (UArray Word32 Word64)
             deriving(Show)

data Pointer
    = Struct !Int32 !Word16 !Word16
    | List !Int32 !ElementSize !Word32
    | Far !LandingSize !Word32 !Word32
    | Capability !Word32

data Value
    = Pointer !Pointer
    | Void
    | Bool Bool

data LandingSize = OneWord | TwoWords

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

data Address = Address
    { msg :: !Message
    , index :: !Word32
    , segnum :: !Word32
    }


data CerializationError
    = SegmentIndexOutOfBounds
    | WordIndexOutOfBounds


class Cerialize a where
    -- TODO: output
    parse :: Address -> Either CerializationError a

instance Cerialize Pointer where
    parse (Address (Message segs) idx segn) = do
        -- XXX: there's got to be a better way to do this:
        let ulen = snd . bounds
        let alen = snd . bounds

        when (segn >= alen segs) $ Left SegmentIndexOutOfBounds
        let Segment words = segs  ! segn
        when (idx >= ulen words) $ Left WordIndexOutOfBounds
        let word = words ! idx
        Right $ case word .|. 0x3 of
            0 -> Struct ((fromIntegral word :: Int32) `shiftR` 2)
                        (fromIntegral (word `shiftR` 32) :: Word16)
                        (fromIntegral (word `shiftR` 48) :: Word16)
            1 -> List ((fromIntegral word :: Int32) `shiftR` 2)
                      (toEnum $ fromIntegral ((word `shiftR` 32) `mod` 8))
                      (fromIntegral (word `shiftR` 35) :: Word32)
            2 -> error "Far pointer, not supported yet."
{-
            2 -> Far ((word :: Int32) `shiftR` 2)
                     landingSize
                  where landingSize =
                    if testBit word 3
                        then TwoWords
                        else OneWord
-}
            3 -> Capability $ fromIntegral (word `shiftR` 32)


-- This is defined inside Data.Binary, but annoyingly not exported:
getMany :: (Eq n, Num n) => Get a -> n -> Get [a]
getMany g n = do getMany' [] n
  where
    getMany' xs 0 = return $ reverse $! xs
    getMany' xs n' = do
        x <- g
        x `seq` getMany' (x:xs) (n' - 1)


getMessage :: Get Message
getMessage = do
    numSegs <- getWord32le
    segLengths <- getMany getWord32le numSegs
    _padding <- getMany getWord8 $ neededPadding numSegs
    segs <- forM segLengths $ \len ->
        Segment <$> (UA.array (0, len-1) <$> zip [0,1..] <$> getMany getWord64le len)
    let _ = segs :: [Segment]
    return $ Message $ A.array (0, numSegs-1) $ zip [0,1..] segs
  where
    neededPadding n = case n `mod` 4 of
        0 -> 0
        n' -> 4 - n'
