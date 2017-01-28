module Data.CapNProto.Untyped where

import qualified Data.ByteString as B
import Data.Word
import Data.Int
import Data.Vector ((!))
import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed as UV

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Exception (ArrayException(IndexOutOfBounds))

type Segment = B.ByteString
type Message = BV.Vector Segment

data Address = Address
    !Int -- ^ Segment number
    !Int -- ^ Word index in segment
    deriving(Show)

data Pointer
    = Struct !Int32 !Word16 !Word16
    | Capability !Word32
    | List !Int32 !ElementSize
    | Far LandingSize Int32 Word32
    deriving(Show)

data PointerDest
    = WordDest Address
    | CapDest Word32
    deriving(Show)

data LandingSize = OneWord | TwoWords deriving(Show, Enum)

data ElementSize
    = Bit
    | Byte
    | TwoBytes
    | FourBytes
    | EightBytesVal
    | EightBytesPtr
    | Composite
    deriving(Show, Enum)


followPtr :: (MonadThrow m) => Message -> Address -> Pointer -> m PointerDest
followPtr msg addr@(Address segnum wordidx) _
    | segnum < 0
      || wordidx < 0
      || segnum >= BV.length msg
      || wordidx >= B.length (msg ! segnum)
      = throwM $ IndexOutOfBounds (show addr)
