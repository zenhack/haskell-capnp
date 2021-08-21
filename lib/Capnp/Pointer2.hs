{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Capnp.Pointer2 where

import Capnp.Bits
import Data.Bits
import Data.Int
import Data.Word

newtype Ptr = Ptr Word64
    deriving(Show, Eq)

pattern StructPtr :: Int32 -> Word16 -> Word16 -> Ptr
pattern ListPtr :: Int32 -> EltSpec -> Ptr
pattern FarPtr :: Bool -> Word32 -> Word32 -> Ptr
pattern CapPtr :: Word32 -> Ptr
{-# COMPLETE StructPtr, ListPtr, FarPtr, CapPtr #-}


newtype StructPtr = StructPtr' Word64
pattern StructPtr o nw np <- (unpackTag -> (0, StructPtr' -> (unpackStruct -> (!o, !nw, !np))))
  where StructPtr o nw np = structPtr (packStruct o nw np)

newtype ListPtr = ListPtr' Word64
pattern ListPtr o e <- (unpackTag -> (1, ListPtr' -> unpackList -> (!o, !e))) where
    ListPtr o e = listPtr (packList o e)

newtype FarPtr = FarPtr' Word64
pattern FarPtr tw o s <- (unpackTag -> (2, FarPtr' -> (unpackFar -> (!tw, !o, !s)))) where
    FarPtr tw o s = farPtr (packFar tw o s)

newtype CapPtr = CapPtr' Word64
pattern CapPtr i <- (unpackTag -> (3, CapPtr' -> (unpackCap -> !i))) where
    CapPtr i = capPtr (packCap i)

newtype EltSpec = EltSpec' Word64
    deriving(Show, Eq)

pattern EltSpec :: ElementSize -> Word32 -> EltSpec
{-# COMPLETE EltSpec #-}

pattern EltSpec sz len <- (unpackEltSpec -> (!sz, !len)) where
    EltSpec !sz !len = packEltSpec sz len

-- | The element size field in a list pointer.
data ElementSize
    = Sz0
    | Sz1
    | Sz8
    | Sz16
    | Sz32
    | Sz64
    | SzPtr
    | SzComposite
    deriving(Show, Eq, Enum)

parsePtr :: Word64 -> Maybe Ptr
{-# INLINE parsePtr #-}
parsePtr 0 = Nothing
parsePtr v = Just (parsePtr' v)

parsePtr' :: Word64 -> Ptr
{-# INLINE parsePtr' #-}
parsePtr' v = Ptr v

-- | @'serializePtr' ptr@ serializes the pointer as a 'Word64', translating
-- 'Nothing' to a null pointer.
--
-- This also changes the offset of zero-sized struct pointers to -1, to avoid
-- them being interpreted as null.
serializePtr :: Maybe Ptr -> Word64
{-# INLINE serializePtr #-}
serializePtr Nothing                  = 0
serializePtr (Just (StructPtr _ 0 0)) = serializePtr' (StructPtr (-1) 0 0)
serializePtr (Just p)                 = serializePtr' p

serializePtr' :: Ptr -> Word64
{-# INLINE serializePtr' #-}
serializePtr' (Ptr v) = v

{-# INLINE unpackTag #-}
unpackTag :: Ptr -> (Word64, Word64)
unpackTag (Ptr w) =
    let !tag = w .&. 3
        !rest = w .&. complement 3
    in
    (tag, rest)


{-# INLINE unpackStruct #-}
unpackStruct :: StructPtr -> (Int32, Word16, Word16)
unpackStruct (StructPtr' w) =
    ( i30 (lo w)
    , bitRange w 32 48
    , bitRange w 48 64
    )

{-# INLINE unpackList #-}
unpackList :: ListPtr -> (Int32, EltSpec)
unpackList (ListPtr' w) =
    ( i30 (lo w)
    , parseEltSpec w
    )

{-# INLINE unpackFar #-}
unpackFar :: FarPtr -> (Bool, Word32, Word32)
unpackFar (FarPtr' w) =
    ( toEnum (bitRange w 2 3)
    , bitRange w 3 32
    , bitRange w 32 64
    )

{-# INLINE unpackCap #-}
unpackCap :: CapPtr -> Word32
unpackCap (CapPtr' w) = bitRange w 32 64

{-# INLINE structPtr #-}
structPtr :: StructPtr -> Ptr
structPtr (StructPtr' w) = Ptr (w .|. 0)

{-# INLINE packStruct #-}
packStruct :: Int32 -> Word16 -> Word16 -> StructPtr
packStruct off dataSz ptrSz = StructPtr' $
    fromLo (fromI30 off) .|.
    (fromIntegral dataSz `shiftL` 32) .|.
    (fromIntegral ptrSz `shiftL` 48)

{-# INLINE listPtr #-}
listPtr :: ListPtr -> Ptr
listPtr (ListPtr' w) = Ptr (w .|. 1)

{-# INLINE packList #-}
packList :: Int32 -> EltSpec -> ListPtr
packList off eltSpec = ListPtr' $
    fromLo (fromI30 off) .|.
    serializeEltSpec eltSpec

{-# INLINE farPtr #-}
farPtr :: FarPtr -> Ptr
farPtr (FarPtr' w) = Ptr (w .|. 2)

{-# INLINE packFar #-}
packFar :: Bool -> Word32 -> Word32 -> FarPtr
packFar twoWords off segId = FarPtr' $
    fromIntegral (fromEnum twoWords `shiftL` 2) .|.
    (fromIntegral off `shiftL` 3) .|.
    (fromIntegral segId `shiftL` 32)

{-# INLINE capPtr #-}
capPtr :: CapPtr -> Ptr
capPtr (CapPtr' w) = Ptr (w .|. 3)

{-# INLINE packCap #-}
packCap :: Word32 -> CapPtr
packCap index = CapPtr' (fromIntegral index `shiftL` 32)

{-# INLINE unpackEltSpec #-}
unpackEltSpec :: EltSpec -> (ElementSize, Word32)
unpackEltSpec (EltSpec' w) =
    ( toEnum (bitRange w 32 35)
    , bitRange w 35 64
    )

{-# INLINE packEltSpec #-}
packEltSpec :: ElementSize -> Word32 -> EltSpec
packEltSpec sz len = EltSpec' $
    (fromIntegral (fromEnum sz) `shiftL` 32) .|.
    (fromIntegral len `shiftL` 35)

{-# INLINE parseEltSpec #-}
parseEltSpec :: Word64 -> EltSpec
parseEltSpec = EltSpec'

{-# INLINE serializeEltSpec #-}
serializeEltSpec :: EltSpec -> Word64
serializeEltSpec (EltSpec' w) = fromIntegral w
