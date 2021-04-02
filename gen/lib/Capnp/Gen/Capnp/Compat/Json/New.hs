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
instance (F.HasUnion (Value)) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ (Value)
instance (OL.IsLabel "null" (F.Variant F.Slot (Value) ())) where
    fromLabel  = (F.Variant GH.voidField 0)
instance (F.HasVariant "null" F.Slot (Value) ())
instance (OL.IsLabel "boolean" (F.Variant F.Slot (Value) Std_.Bool)) where
    fromLabel  = (F.Variant (GH.dataField 16 0 1 0) 1)
instance (F.HasVariant "boolean" F.Slot (Value) Std_.Bool)
instance (OL.IsLabel "number" (F.Variant F.Slot (Value) Std_.Double)) where
    fromLabel  = (F.Variant (GH.dataField 0 1 64 0) 2)
instance (F.HasVariant "number" F.Slot (Value) Std_.Double)
instance (OL.IsLabel "string" (F.Variant F.Slot (Value) Basics.Text)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 3)
instance (F.HasVariant "string" F.Slot (Value) Basics.Text)
instance (OL.IsLabel "array" (F.Variant F.Slot (Value) (R.List Value))) where
    fromLabel  = (F.Variant (GH.ptrField 0) 4)
instance (F.HasVariant "array" F.Slot (Value) (R.List Value))
instance (OL.IsLabel "object" (F.Variant F.Slot (Value) (R.List Value'Field))) where
    fromLabel  = (F.Variant (GH.ptrField 0) 5)
instance (F.HasVariant "object" F.Slot (Value) (R.List Value'Field))
instance (OL.IsLabel "call" (F.Variant F.Slot (Value) Value'Call)) where
    fromLabel  = (F.Variant (GH.ptrField 0) 6)
instance (F.HasVariant "call" F.Slot (Value) Value'Call)
data Value'Field 
type instance (R.ReprFor Value'Field) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "name" (F.Field F.Slot (Value'Field) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" F.Slot (Value'Field) Basics.Text)
instance (OL.IsLabel "value" (F.Field F.Slot (Value'Field) Value)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "value" F.Slot (Value'Field) Value)
data Value'Call 
type instance (R.ReprFor Value'Call) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "function" (F.Field F.Slot (Value'Call) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "function" F.Slot (Value'Call) Basics.Text)
instance (OL.IsLabel "params" (F.Field F.Slot (Value'Call) (R.List Value))) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "params" F.Slot (Value'Call) (R.List Value))
data FlattenOptions 
type instance (R.ReprFor FlattenOptions) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "prefix" (F.Field F.Slot (FlattenOptions) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "prefix" F.Slot (FlattenOptions) Basics.Text)
data DiscriminatorOptions 
type instance (R.ReprFor DiscriminatorOptions) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "name" (F.Field F.Slot (DiscriminatorOptions) Basics.Text)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "name" F.Slot (DiscriminatorOptions) Basics.Text)
instance (OL.IsLabel "valueName" (F.Field F.Slot (DiscriminatorOptions) Basics.Text)) where
    fromLabel  = (GH.ptrField 1)
instance (F.HasField "valueName" F.Slot (DiscriminatorOptions) Basics.Text)