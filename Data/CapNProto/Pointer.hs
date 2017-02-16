module Data.CapNProto.Pointer where

import Data.CapNProto.Bits
import Data.Bits
import Data.Int
import Data.Word

-- | A Ptr represents the information in a capnproto pointer.
data Ptr
    = StructPtr !Int32 !Word16 !Word16
        -- ^ @StructPtr off dataSz ptrSz@ is a pointer to a struct
        -- at offset @off@ in words from the end of the pointer, with
        -- a data section of size @dataSz@ words, and a pointer section
        -- of size @ptrSz@ words.
    | ListPtr !Int32 !EltSpec
        -- ^ @ListPtr off eltSpec@ is a pointer to a list starting at
        -- offset @off@ in words from the end of the pointer. @eltSpec@
        -- encodes the C and D fields in the encoding spec; see 'EltSpec'
        -- for details
    | FarPtr !Bool !Word32 !Word32
        -- ^ @FarPtr twoWords off segment@ is a far pointer, whose landing
        -- pad is:
        --
        -- * two words iff @twoWords@,
        -- * @off@ words from the start of the target segment, and
        -- * in segment id @segment@.
    | CapPtr !Word32
        -- ^ @CapPtr id@ is a pointer to the capability with the id @id@.
    deriving(Show, Eq)


-- | The element size field in a list pointer.
data ElementSize
    = Sz0
    | Sz1
    | Sz8
    | Sz16
    | Sz32
    | Sz64
    | SzPtr
    deriving(Show, Eq, Enum)

-- | A combination C and D fields in a list pointer, i.e. the element
--   size, and either the number of elements in the list, or the
--   total number of *words* in the list (if size is composite).
data EltSpec
    = EltNormal !ElementSize !Word32
        -- ^ @EltNormal size len@ is a normal (non-composite) element type
        -- (C /= 7). @size@ is the size of the elements, and @len@ is the
        -- number of elements in the list.
    | EltComposite !Int32
        -- ^ @EltComposite len@ is a composite element (C == 7). @len@ is the
        -- length of the list in words.
    deriving(Show, Eq)


-- | @parsePtr word@ parses word as a capnproto pointer.
parsePtr :: Word64 -> Ptr
parsePtr word =
    case bitRange word 0 2 of
        0 -> StructPtr
            (i30 (lo word))
            (bitRange word 32 48)
            (bitRange word 48 64)
        1 -> ListPtr
            (i30 (lo word))
            (parseEltSpec word)
        2 -> FarPtr
            (toEnum (bitRange word 2 3))
            (bitRange word 3 32)
            (bitRange word 32 64)
        3 -> CapPtr (bitRange word 32 64)

serializePtr :: Ptr -> Word64
serializePtr (StructPtr off dataSz ptrSz) =
    -- 0 .|.
    (fromLo (fromI30 off)) .|.
    (fromIntegral dataSz `shiftL` 32) .|.
    (fromIntegral ptrSz `shiftL` 48)
serializePtr (ListPtr off eltSpec) = -- eltSz numElts) =
    1 .|.
    (fromLo (fromI30 off)) .|.
    (serializeEltSpec eltSpec)
serializePtr (FarPtr twoWords off segId) =
    2 .|.
    (fromIntegral (fromEnum twoWords) `shiftL` 2) .|.
    (fromIntegral off `shiftL` 3) .|.
    (fromIntegral segId `shiftL` 32)
serializePtr (CapPtr index) =
    3 .|.
    -- (fromIntegral 0 `shiftL` 2) .|.
    (fromIntegral index `shiftL` 32)

parseEltSpec :: Word64 -> EltSpec
parseEltSpec word = case bitRange word 32 35 of
    7 -> EltComposite (i29 (hi word))
    sz -> EltNormal (toEnum sz) (bitRange word 35 64)

serializeEltSpec :: EltSpec -> Word64
serializeEltSpec (EltNormal sz len) =
    (fromIntegral (fromEnum sz) `shiftL` 32) .|.
    (fromIntegral len `shiftL` 35)
serializeEltSpec (EltComposite words) =
    (7 `shiftL` 32) .|.
    (fromHi (fromI29 words))


-- targetSize tries to determine the size in words of the object that its
-- argument points to, returning Nothing if it is unable. This allways works
-- for everything but far pointers (since the information is not available
-- without following the pointer) and capability pointers (since capabilities
-- do not have a defined size).
targetSize :: Ptr -> Maybe Int
targetSize (ListPtr _ (EltNormal sz len)) =
    case sz of
        Sz0 -> Just 0
        Sz1 -> compactedListSize len 64
        Sz8 -> compactedListSize len 8
        Sz16 -> compactedListSize len 4
        Sz32 -> compactedListSize len 2
        Sz64 -> Just (fromIntegral len)
        SzPtr -> Just (fromIntegral len)
targetSize (ListPtr _ (EltComposite size)) = Just (fromIntegral size)
targetSize (StructPtr _ dataSz ptrSz) = Just $ fromIntegral $ dataSz + ptrSz
targetSize (CapPtr _) = Nothing
targetSize (FarPtr _ _ _) = Nothing

-- Helper for targetSize
compactedListSize :: Word32 -> Int -> Maybe Int
compactedListSize len modulus
    | fromIntegral len `mod` modulus == 0 = Just $ fromIntegral len `div` modulus
    | otherwise = Just $ (fromIntegral len `div` modulus) + 1
