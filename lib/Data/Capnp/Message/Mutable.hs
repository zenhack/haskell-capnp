{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.Capnp.Message.Mutable
    ( Message
    , Segment
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

newtype Message s = Message (MV.MVector s (Segment s))
newtype Segment s = Segment (SMV.MVector s Word64)

type WriteCtx m s = (PrimMonad m, s ~ PrimState m, MonadThrow m)

instance WriteCtx m s => GM.Segment m (Segment s) where
    segLen (Segment vec) = pure $ SMV.length vec
    slice start len (Segment vec) = pure $ Segment (SMV.slice start len vec)
    read (Segment vec) i = fromLE64 <$> SMV.read vec i
    fromByteString bytes = do
        M.Segment vec <- GM.fromByteString bytes
        Segment <$> SV.thaw vec
    toByteString (Segment vec) = do
        seg <- M.Segment <$> SV.freeze vec
        GM.toByteString seg

instance WriteCtx m s => GM.MSegment m (Segment s) where
    write (Segment vec) i val =
        SMV.write vec i (toLE64 val)
    grow (Segment vec) amount = Segment <$> SMV.grow vec amount

instance WriteCtx m s => GM.Message m (Message s) (Segment s) where
    msgLen (Message vec) = pure $ MV.length vec
    getSeg (Message vec) = MV.read vec

instance WriteCtx m s => GM.MMessage m (Message s) (Segment s) where
    setSeg (Message vec) = MV.write vec
