{-# LANGUAGE DoAndIfThenElse #-}
module Data.CapNProto.LowLevel
    ( Message
    , BoundsError(..)
    , ElementSize(..)
    , View(..)
    , getRootView
    , defaultMaxMessageLen
    , defaultMaxPointerDepth
    )
where

-- Note on the comments in this file: most functions that return
-- @Either ParseError foo@ are documented as if they never fail;
-- unless otherwise stated the possibility of a ParseError is
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
        !(Word16 -> Either ParseError Word64) -- ^ get word at index
        !Word16 -- ^ Number of pointers
        !(Word16 -> Either ParseError View) -- ^ get view of pointer at index
    | ListView
        !Word32 -- ^ length of list
        !(Word32 -> Either ParseError View) -- ^ get element at index
    | CapabilityView
        !Word32 -- ^ capability index

instance Show View where
    show view = case view of
        StructView dataSz getData ptrSz getPtr -> concat
            [ "StructView "
            , show dataSz, " "
            , show ptrSz, " "
            , show $ getList dataSz getData
            , " "
            , show $ getList ptrSz getPtr
            ]
        ListView len get -> concat
            [ "ListView "
            , show len, " "
            , show $ getList len get
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
    = IndexBoundsError -- ^ bad index into data structure
        -- TODO: specify type, and fold Segments/Words into this.
        !Word32 -- ^ index provided
        !Word32 -- ^ number of elements in structure.
    | SegmentsBoundError -- ^ bad index into segments array
        !Word32 -- ^ index provided
        !Word32 -- ^ max legal index
    | WordsBoundError -- ^ bad index into word array
        !Word32 -- ^ index provided
        !Word32 -- ^ max legal index
    | PointerDepthExceeded
    | MessageSizeExceeded
    deriving(Show)


data ParseError
    = BoundsError !BoundsError
    | LengthInconsistency
        -- ^ List pointers with element size "InlineComposite" specify
        -- the size in two ways -- the element size in words * number of
        -- elements and the word count. This error indicates a disagreement,
        !Word32 -- ^ Specified length in words
        !Word32 -- ^ Specified size of an element
        !Word32 -- ^ Specified number of elemnets.
    | RootIsNotStruct !View -- The message's root struct pointer is not
                            -- actually a pointer to a struct.
    deriving(Show)


-- | @loadAddr addr@ returns the word at the word at @addr@
loadAddr :: Address -> Either ParseError Word64
loadAddr addr = do
    checkBounds addr
    let (Message segs) = message addr
    let (Segment seg) = segs ! segIdx addr
    return (seg ! wordIdx addr)


-- | @getRootView msg maxDepth@ is a view of the root struct
-- pointer of the messag, with a depth limit of maxDepth.
getRootView :: Message -> Word32 -> Either ParseError View
getRootView msg maxDepth = do
    -- TODO: check if this is a struct view, and if not, complain.
    view <- getView (Address maxDepth msg 0 0)
    case view of
        StructView _ _ _ _ -> Right view
        _ -> Left (RootIsNotStruct view)



getView :: Address -> Either ParseError View
getView addr = do
    ptr <- parsePointer <$> loadAddr addr
    addr' <- followPtr addr ptr
    makeView addr' ptr

makeView :: Address -> Pointer -> Either ParseError View
makeView addr ptr =
    case ptr of
        Far _ _ _ -> getView addr
        Capability cap -> return $ CapabilityView cap
        Struct _ dataSz ptrSz -> do
            let dataAddr = addr
            ptrsAddr <- addrSeek dataAddr (fromIntegral dataSz)
            return $ StructView
                dataSz (\i -> atIndex dataAddr dataSz i >>= loadAddr)
                ptrSz  (\i -> atIndex ptrsAddr ptrSz  i >>= getView)
        list@(List off eltSz eltCount) -> getListView addr off eltSz eltCount


getListView :: Address -> Int32 -> ElementSize -> Word32 -> Either ParseError View
getListView addr off InlineComposite wordCount = do
    eltTag <- parsePointer <$> loadAddr addr
    case eltTag of
        Struct eltCount dataSz ptrSz -> do
            let eltSize = fromIntegral dataSz + fromIntegral ptrSz
            let eltCount' = fromIntegral eltCount :: Word32
            if (eltSize * eltCount') /= wordCount then
                Left $ LengthInconsistency wordCount eltSize eltCount'
            else do
                baseAddr <- addrSeek addr 1
                return $ ListView eltCount' (\i ->
                    if i >= eltCount' then
                        Left $ BoundsError $ IndexBoundsError i eltCount'
                    else do
                        eltAddr <- addrSeek
                            baseAddr
                            (fromIntegral i * fromIntegral eltSize)
                        makeView eltAddr eltTag)


-- | @atIndex base sz idx@ indexes into the region of a segment starting at
-- address @base@, extending for @sz@ words. It returns the address of the
-- indexed element.
atIndex :: Address -> Word16 -> Word16 -> Either ParseError Address
atIndex base sz idx = do
    checkFieldBounds sz idx
    addrSeek base (fromIntegral idx)
  where
    checkFieldBounds :: Word16 -> Word16 -> Either ParseError ()
    checkFieldBounds cap idx =
        if (idx >= cap) then
            Left $ BoundsError $ WordsBoundError (fromIntegral idx)
                                                 (fromIntegral cap)
        else
            Right ()


-- | @decrDepthLimit addr@ is @addr@ with the depth limit decreased by one.
decrDepthLimit :: Address -> Either ParseError Address
decrDepthLimit addr = checkBounds $ addr { depthLimit = depthLimit addr - 1 }


-- | @addrSeek addr shift@ seeks @shift@ words from @addr@.
addrSeek :: Address -> Int32 -> Either ParseError Address
addrSeek addr shift = checkBounds $
    addr { wordIdx = fromIntegral $ fromIntegral (wordIdx addr) + shift }



defaultMaxMessageLen, defaultMaxPointerDepth :: Word32
-- 64 MiB; this is what the C++ implementation does.
defaultMaxMessageLen = 64 `shiftL` 20
defaultMaxPointerDepth = 64


-- | @checkBounds addr@ verifies that addr is within the legal boundaries
-- of its message. For convienience, in the case of success it returns
-- @addr@.
checkBounds :: Address -> Either ParseError Address
checkBounds addr@(Address depth (Message segs) wordsIdx segIdx) = do
    when (depth == 0) $ Left $ BoundsError $ PointerDepthExceeded
    let (_, segsMax) = bounds segs
    when (segIdx > segsMax) $ Left $ BoundsError (SegmentsBoundError segIdx segsMax)
    let Segment words = segs ! segIdx
    let (_, wordsMax) = bounds words
    when (wordsIdx > wordsMax) $ Left $ BoundsError (WordsBoundError wordsIdx wordsMax)
    Right addr


-- | @followPtr addr ptr@ follows @ptr@ relative to @addr@, returning
-- the new address. If @ptr@ points to a capability, returns
-- @decrDepthLimit addr@. XXX: This doesn't make a ton of sense; there
-- isn't really a sensible Address to return, since a capability pointer
-- doesn't point to data.
followPtr :: Address -> Pointer -> Either ParseError Address
followPtr addr ptr = decrDepthLimit =<< case ptr of
    Struct off dataSz ptrsSz -> addrSeek addr (off + 1)
    List off _ _ -> addrSeek addr (off + 1)
    Capability _ -> return addr



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
