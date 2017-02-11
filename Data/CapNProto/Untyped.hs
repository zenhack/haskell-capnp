{-# LANGUAGE MultiParamTypeClasses #-}
module Data.CapNProto.Untyped where

import Prelude hiding (lookup, length)
import Data.Word
import Data.CapNProto.Address (WordAddr(..))
import Control.Monad.Catch (MonadThrow)

-- This is a WIP, still thinking about the design of this.
-- The idea is to have a series of types that abstract out
-- the "cursor".

data Struct l c m = Struct (m (l m Word64, l m (Ptr l c m)))

data Ptr l c m
    = StructPtr (m (Struct l c m))
    | CapPtr (m c)
--    | FarPtr (m (Ptr l c m))
    | ListPtr (m (ListPtr l c m))

data ListPtr l c m
    = List0  (l m ())
    | List1  (l m Bool)
    | List8  (l m Word8)
    | List16 (l m Word16)
    | List32 (l m Word32)
    | List64 (l m Word64)
    | ListP
        Bool -- true iff this is really a composite
        (l m (Ptr l c m))

class MonadThrow m => List l m e where
    length :: l m e -> m Int
    lookup :: Int -> l m e -> m e
    slice  :: Int -> Int -> l m e -> m (l m e)

class (MonadThrow m, List l m e) => MList l m e where
    write :: l m e -> Int -> e -> m ()

getWord :: (MonadThrow m, List msg m (seg m Word64), List seg m Word64)
    => WordAddr -> msg m (seg m Word64) -> m Word64
getWord (WordAt segIdx wordIdx) msg = do
    seg <- lookup segIdx msg
    lookup wordIdx seg
