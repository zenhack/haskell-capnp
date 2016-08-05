module Encoding where

import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.IArray ((!), bounds)
import Data.Bits
import Data.Word
import Data.Int
import Control.Monad (when)

data Message = Message (Array Word64 Segment)

data Segment = Segment (UArray Word64 Word64)

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
    , index :: !Word64
    , segnum :: !Word64
    }


data CerializationError
    = SegmentIndexOutOfBounds
    | WordIndexOutOfBounds


class Cerialize a where
    -- TODO: output
    parse :: Address -> Either CerializationError a

-- dropLow :: Word64 -> Int -> Word64
-- dropLow n bits = n .&. (compliment (1 shiftL

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
