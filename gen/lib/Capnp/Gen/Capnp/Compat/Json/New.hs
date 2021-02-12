{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.Compat.Json.New where
import qualified Capnp.Repr as R
import qualified Capnp.Fields as F
import qualified GHC.OverloadedLabels as OL
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Value 
type instance (R.ReprFor Value) = (R.Ptr (Std_.Just R.Struct))
data Value'Field 
type instance (R.ReprFor Value'Field) = (R.Ptr (Std_.Just R.Struct))
data Value'Call 
type instance (R.ReprFor Value'Call) = (R.Ptr (Std_.Just R.Struct))
data FlattenOptions 
type instance (R.ReprFor FlattenOptions) = (R.Ptr (Std_.Just R.Struct))
data DiscriminatorOptions 
type instance (R.ReprFor DiscriminatorOptions) = (R.Ptr (Std_.Just R.Struct))