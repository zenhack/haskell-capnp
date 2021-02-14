{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.Compat.Json.New where
import qualified Capnp.Repr as R
import qualified Capnp.Fields as F
import qualified Capnp.New.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers.New as GH
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Value 
type instance (R.ReprFor Value) = (R.Ptr (Std_.Just R.Struct))
data Value'Field 
type instance (R.ReprFor Value'Field) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "name" (F.Field (Value'Field) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" (Value'Field) Basics.Text)
instance (OL.IsLabel "value" (F.Field (Value'Field) Value)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "value" (Value'Field) Value)
data Value'Call 
type instance (R.ReprFor Value'Call) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "function" (F.Field (Value'Call) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "function" (Value'Call) Basics.Text)
instance (OL.IsLabel "params" (F.Field (Value'Call) (R.List Value))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "params" (Value'Call) (R.List Value))
data FlattenOptions 
type instance (R.ReprFor FlattenOptions) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "prefix" (F.Field (FlattenOptions) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "prefix" (FlattenOptions) Basics.Text)
data DiscriminatorOptions 
type instance (R.ReprFor DiscriminatorOptions) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "name" (F.Field (DiscriminatorOptions) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" (DiscriminatorOptions) Basics.Text)
instance (OL.IsLabel "valueName" (F.Field (DiscriminatorOptions) Basics.Text)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "valueName" (DiscriminatorOptions) Basics.Text)