{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.Capnp.Message.Mutable
    ( Message
    , WriteCtx(..)
    ) where

import Data.Word

import Control.Monad.Catch     (MonadThrow)
import Control.Monad.Primitive (PrimMonad, PrimState)
import System.Endian           (fromLE64, toLE64)

import qualified Data.Capnp.Message           as M
import qualified Data.Capnp.Message.Generic   as GM
import qualified Data.Vector.Mutable          as MV
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as SMV

-- | A 'Message' is a mutable capnproto message. The type parameter 's' is the
-- state token for the instance of 'PrimMonad' in which the message may be
-- modified.
--
-- Due to mutabilty, the implementations of 'toByteString' and 'fromByteString'
-- must make full copies, and so are O(n) in the length of the segment.
newtype Message s = Message (MV.MVector s (SMV.MVector s Word64))

-- | 'WriteCtx' is the context needed for most write operations.
type WriteCtx m s = (PrimMonad m, s ~ PrimState m, MonadThrow m)

instance WriteCtx m s => GM.Message m (Message s) where
    newtype Segment m (Message s) = Segment { segToVec :: SMV.MVector s Word64 }

    numWords (Segment vec) = pure $ SMV.length vec
    slice start len (Segment vec) = pure $ Segment (SMV.slice start len vec)
    read (Segment vec) i = fromLE64 <$> SMV.read vec i
    fromByteString bytes = do
        vec <- M.internalToWordVector <$> GM.fromByteString bytes
        Segment <$> SV.thaw vec
    toByteString (Segment vec) = do
        seg <- M.internalFromWordVector <$> SV.freeze vec
        GM.toByteString seg

    numSegs (Message vec) = pure $ MV.length vec
    internalGetSeg (Message vec) i = Segment <$> MV.read vec i

instance WriteCtx m s => GM.MMessage m (Message s) where
    write (Segment vec) i val =
        SMV.write vec i (toLE64 val)
    grow (Segment vec) amount = Segment <$> SMV.grow vec amount

    internalSetSeg (Message msg) i (Segment seg) = MV.write msg i seg
