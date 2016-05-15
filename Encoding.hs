module Encoding where

import Data.Array.IArray
import Data.Array.Unboxed
import Data.Bits
import Data.Word
import Data.Int

data Message = Message (IArray Word64 Segment)

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


class Cerialize a where
    -- TODO: output
    -- TODO: Either?
    parse :: Address -> Maybe a

-- dropLow :: Word64 -> Int -> Word64
-- dropLow n bits = n .&. (compliment (1 shiftL

instance Cerialize Pointer where
    parse (Address (Message segs) idx segn) =
        --if (segn > len segs)
        let word = (segs ! segn) ! idx in case word .|. 0x3 of
            0 -> Struct ((word :: Int32) `shiftR` 2)
                        ((word `shiftR` 32) :: Word16)
                        ((word `shiftR` 48) :: Word16)
            1 -> List ((word :: Int32) `shiftR` 2)
                      (toEnum ((word `shiftR` 32) `mod` 8))
                      (word `shiftR` 35) :: Word32
            2 -> error "Far pointer, not supported yet."
{-
            2 -> Far ((word :: Int32) `shiftR` 2)
                     landingSize
                  where landingSize =
                    if testBit word 3
                        then TwoWords
                        else OneWord
-}
            3 -> Capability (word `shiftR` 32)
