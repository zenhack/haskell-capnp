{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Capnp.New.Basics
    ( Text
    , Data
    , AnyPointer
    , AnyList
    , AnyStruct
    , Capability
    ) where

import qualified Capnp.Repr as R
import           Data.Word

data Text
data Data
data AnyPointer
data AnyList
data AnyStruct
data Capability

type instance R.ReprFor Data = R.ReprFor (R.List Word8)
type instance R.ReprFor Text = R.ReprFor (R.List Word8)
type instance R.ReprFor AnyPointer = 'R.Ptr 'Nothing
type instance R.ReprFor AnyList = 'R.Ptr ('Just ('R.List 'Nothing))
type instance R.ReprFor AnyStruct = 'R.Ptr ('Just 'R.Struct)
type instance R.ReprFor Capability = 'R.Ptr ('Just 'R.Cap)
