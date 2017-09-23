{-|
Module: Data.CapNProto.Address
Description: Utilities for manipulating addresses within capnproto messages.
-}
module Data.CapNProto.Address where

import           Data.CapNProto.Bits    (WordCount)
import qualified Data.CapNProto.Pointer as P
import           Data.Word

-- | The address of a word within a message
data WordAddr = WordAt
    { segIndex  :: !Int -- ^ Segment number
    , wordIndex :: !WordCount -- ^ offset in words from the start of the segment.
    } deriving(Show, Eq)

-- | The "address" of a capability
newtype CapAddr = Cap Word32 deriving(Show, Eq)

-- | An address, i.e. a location that a pointer may point at.
data Addr
    = WordAddr !WordAddr
    | CapAddr !CapAddr
    deriving(Show, Eq)

-- | @resolvePtr from ptr@ Resolves the pointer @ptr@ to an address
-- relative to @from@. Note that inter-segment pointers (FarPtr)
-- resolve to the address of the landing pad, *not* the the final
-- address of the object pointed to, as that would reqiure access
-- to the message.
resolvePtr :: WordAddr -> P.Ptr -> Addr
resolvePtr (WordAt seg word) (P.StructPtr off _dataSz _ptrSz) =
    WordAddr $ WordAt seg (word + fromIntegral off + 1)
resolvePtr (WordAt seg word) (P.ListPtr off _) =
    WordAddr (WordAt seg (word + fromIntegral off + 1))
resolvePtr _ (P.FarPtr _ word seg) =
    WordAddr $ WordAt
        (fromIntegral seg)
        (fromIntegral word)
resolvePtr _ (P.CapPtr cap) = CapAddr (Cap cap)
