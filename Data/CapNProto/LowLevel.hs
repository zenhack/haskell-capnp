{-# LANGUAGE DoAndIfThenElse #-}
module Data.CapNProto.LowLevel
    ( Message
    , BoundsError(..)
    , ElementSize(..)
    , View(..)
    , getRootView
    , getMessage
    , defaultMaxMessageLen
    , defaultMaxPointerDepth
    )
where

-- Note on the comments in this file: most functions that return
-- @Either BoundsError foo@ are documented as if they never fail;
-- unless otherwise stated the possibility of a BoundsError is
-- implied.
--
-- Functions which both take and return an Address decrement the
-- depth limit, unless otherwise specified.

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

data View
    = StructView
        !Word16 -- ^ Number of data words
        !(Word16 -> Either BoundsError Word64) -- ^ get word at index
        !Word16 -- ^ Number of pointers
        !(Word16 -> Either BoundsError View) -- ^ get view of pointer at index
    | ListView
        !Word32 -- ^ length of list
        !ElementSize -- ^ size of elements
        !(Word32 -> Either BoundsError View) -- ^ get element at index
    | CapabilityView
        !Word32 -- ^ capability index

instance Show View where
    show (StructView dataSz getData ptrSz getPtr) = concat
        [ "StructView "
        , show dataSz, " "
        , show ptrSz, " "
        , show $ getList dataSz getData
        , " "
        , show $ getList ptrSz getPtr
        ]
      where
        getList sz get = map get $ takeWhile (\i -> i < sz) [0,1..]

-- | An absolute address in a CapNProto message.
data Address = Address
    { depthLimit :: !Word32
      -- ^ Remaining pointer depth; when this hits zero we get an error.
      -- See https://capnproto.org/encoding.html#security-considerations
      -- for reasoning
    , message :: !Message -- ^ The message
    , wordIdx :: !Word32 -- ^ word index
    , segIdx  :: !Word32 -- ^ segment index
    } deriving(Show)


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
        !Word32 -- ^ max legal index
    | WordsBoundError -- ^ bad index into word array
        !Word32 -- ^ index provided
        !Word32 -- ^ max legal index
    | PointerDepthExceeded
    | MessageSizeExceeded
    deriving(Show)


-- | @loadAddr addr@ returns the word at the word at @addr@
loadAddr :: Address -> Either BoundsError Word64
loadAddr addr = do
    checkBounds addr
    let (Message segs) = message addr
    let (Segment seg) = segs ! segIdx addr
    return (seg ! wordIdx addr)


-- | @getRootView msg maxDepth@ is a view of the root struct
-- pointer of the messag, with a depth limit of maxDepth.
getRootView :: Message -> Word32 -> Either BoundsError View
getRootView msg maxDepth = do
    -- TODO: check if this is a struct view, and if not, complain.
    getView (Address maxDepth msg 0 0)

getView :: Address -> Either BoundsError View
getView addr = do
    ptr <- parsePointer <$> loadAddr addr
    addr' <- followPtr addr ptr
    case ptr of
        Far _ _ _ -> getView addr'
        Struct _ dataSz ptrSz -> do
            let dataAddr = addr'
            ptrsAddr <- addrSeek dataAddr (fromIntegral dataSz)
            return $ StructView
                dataSz (\i -> atIndex dataAddr dataSz i >>= loadAddr)
                ptrSz  (\i -> atIndex ptrsAddr ptrSz  i >>= getView)
--        List off eltSz len ->


-- | @atIndex base sz idx@ indexes into the region of a segment starting at
-- address @base@, extending for @sz@ words. It returns the address of the
-- indexed element.
atIndex base sz idx = do
    checkFieldBounds sz idx
    addrSeek base (fromIntegral idx)
  where
    checkFieldBounds :: Word16 -> Word16 -> Either BoundsError ()
    checkFieldBounds cap idx =
        if (idx >= cap) then
            Left $ WordsBoundError (fromIntegral idx) (fromIntegral cap)
        else
            Right ()


-- | @addrSeek addr shift@ seeks
addrSeek :: Address -> Int32 -> Either BoundsError Address
addrSeek addr shift = do
    let ret = addr { wordIdx = fromIntegral $ fromIntegral (wordIdx addr) + shift }
    checkBounds ret
    return ret



defaultMaxMessageLen, defaultMaxPointerDepth :: Word32
-- 64 MiB; this is what the C++ implementation does.
defaultMaxMessageLen = 64 `shiftL` 20
defaultMaxPointerDepth = 64


checkBounds :: Address -> Either BoundsError ()
checkBounds (Address depth (Message segs) wordsIdx segIdx) = do
    when (depth == 0) $ Left PointerDepthExceeded
    let (_, segsMax) = bounds segs
    when (segIdx > segsMax) $ Left (SegmentsBoundError segIdx segsMax)
    let Segment words = segs ! segIdx
    let (_, wordsMax) = bounds words
    when (wordsIdx > wordsMax) $ Left (WordsBoundError wordsIdx wordsMax)
    Right ()


-- | @followPtr addr ptr@ follows @ptr@ relative to @addr@, returning
-- the new address. If @ptr@ points to a capability, returns @addr@.
-- XXX: This doesn't make a ton of sense; there isn't really a sensible
-- Address to return, since a capability pointer doesn't point to data.
followPtr :: Address -> Pointer -> Either BoundsError Address
followPtr addr ptr = case ptr of
    Struct off dataSz ptrsSz -> do
        let ret = addr { depthLimit = depthLimit addr - 1
                       , wordIdx = fromIntegral $
                            fromIntegral (wordIdx addr) + off + 1
                       }
        checkBounds ret
        return ret
    Capability _ -> Right addr



-- | @bitRange word lo hi@ is the unsigned integer represented by the
-- bits of @word@ in the range [lo, hi)
bitRange :: (Integral a => Word64 -> Int -> Int -> a)
bitRange word lo hi = fromIntegral $
    (word .&. ((1 `shiftL` hi) - 1)) `shiftR` lo


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
