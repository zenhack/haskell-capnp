{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Capnp.New.Basics
    ( Text
    , Data
    ) where

import qualified Capnp.Repr as R
import           Data.Word

data Text
data Data

type instance R.ReprFor Data = R.ReprFor (R.List Word8)
type instance R.ReprFor Text = R.ReprFor (R.List Word8)
