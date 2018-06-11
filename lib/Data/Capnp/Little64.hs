{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{- | Working with little-endian 64-bit values.

This module defines:

* a type 'Little64' for 64-bit little endian values, which is just a newtype
  wrapper around 'Word64'.
* instances of unboxed vectors for 'Little64', which store their values in
  little-endian byte order, regardless of cpu endianness.
-}
module Data.Capnp.Little64
    ( Little64
    , pack
    , unpack
    ) where

import Data.Word     (Word64)
import System.Endian (fromLE64, toLE64)

import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed         as UV

-- | 64-bit little-endian value.
newtype Little64 = Little64 Word64

-- | Convert a cpu-endian value to a little-endian one.
pack :: Word64 -> Little64
pack = Little64 . toLE64
{-# INLINE pack #-}

-- | Convert a little-endian value to a cpu-endian one.
unpack :: Little64 -> Word64
unpack (Little64 w) = fromLE64 w
{-# INLINE unpack #-}

newtype instance UV.MVector s Little64 = MVec (UV.MVector s Word64)
newtype instance UV.Vector Little64 = Vec (UV.Vector Word64)

instance UV.Unbox Little64

instance GMV.MVector UV.MVector Little64 where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeMove #-}
    {-# INLINE basicUnsafeGrow #-}

    basicLength (MVec vec) = GMV.basicLength vec
    basicUnsafeSlice lo hi (MVec vec) = MVec (GMV.basicUnsafeSlice lo hi vec)
    basicOverlaps (MVec v1) (MVec v2) = GMV.basicOverlaps v1 v2
    basicUnsafeNew len = MVec <$> GMV.basicUnsafeNew len
    basicInitialize (MVec vec) = GMV.basicInitialize vec
    basicUnsafeReplicate n (Little64 val) = MVec <$> GMV.basicUnsafeReplicate n val
    basicUnsafeRead (MVec vec) i = Little64 <$> GMV.basicUnsafeRead vec i
    basicUnsafeWrite (MVec vec) i (Little64 val) = GMV.basicUnsafeWrite vec i val
    basicClear (MVec vec) = GMV.basicClear vec
    basicSet (MVec vec) (Little64 val) = GMV.basicSet vec val
    basicUnsafeCopy (MVec v1) (MVec v2) = GMV.basicUnsafeCopy v1 v2
    basicUnsafeMove (MVec v1) (MVec v2) = GMV.basicUnsafeMove v1 v2
    basicUnsafeGrow (MVec vec) count = MVec <$> GMV.basicUnsafeGrow vec count

instance GV.Vector UV.Vector Little64 where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}

    basicUnsafeFreeze (MVec vec) = Vec <$> GV.basicUnsafeFreeze vec
    basicUnsafeThaw (Vec vec) = MVec <$> GV.basicUnsafeThaw vec
    basicLength (Vec vec) = GV.basicLength vec
    basicUnsafeSlice lo hi (Vec vec) = Vec (GV.basicUnsafeSlice lo hi vec)
    basicUnsafeIndexM (Vec vec) i = Little64 <$> GV.basicUnsafeIndexM vec i
    basicUnsafeCopy (MVec mv) (Vec v) = GV.basicUnsafeCopy mv v
    elemseq (Vec v) (Little64 e) = GV.elemseq v e
