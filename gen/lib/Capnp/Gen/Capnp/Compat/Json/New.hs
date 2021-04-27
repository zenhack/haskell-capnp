{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
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
import qualified Capnp.New.Classes as C
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Value 
type instance (R.ReprFor Value) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Value) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate Value) where
    type AllocHint Value = ()
    new  = GH.newStruct
instance (F.HasUnion Value) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ Value
        = Value'null (R.Raw mut_ ())
        | Value'boolean (R.Raw mut_ Std_.Bool)
        | Value'number (R.Raw mut_ Std_.Double)
        | Value'string (R.Raw mut_ Basics.Text)
        | Value'array (R.Raw mut_ (R.List Value))
        | Value'object (R.Raw mut_ (R.List Value'Field))
        | Value'call (R.Raw mut_ Value'Call)
        | Value'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (Value'null <$> (GH.readVariant #null struct_))
        1 ->
            (Value'boolean <$> (GH.readVariant #boolean struct_))
        2 ->
            (Value'number <$> (GH.readVariant #number struct_))
        3 ->
            (Value'string <$> (GH.readVariant #string struct_))
        4 ->
            (Value'array <$> (GH.readVariant #array struct_))
        5 ->
            (Value'object <$> (GH.readVariant #object struct_))
        6 ->
            (Value'call <$> (GH.readVariant #call struct_))
        _ ->
            (Std_.pure (Value'unknown' tag_))
instance (F.HasVariant "null" F.Slot Value ()) where
    theVariant  = (F.Variant GH.voidField 0)
instance (F.HasVariant "boolean" F.Slot Value Std_.Bool) where
    theVariant  = (F.Variant (GH.dataField 16 0 1 0) 1)
instance (F.HasVariant "number" F.Slot Value Std_.Double) where
    theVariant  = (F.Variant (GH.dataField 0 1 64 0) 2)
instance (F.HasVariant "string" F.Slot Value Basics.Text) where
    theVariant  = (F.Variant (GH.ptrField 0) 3)
instance (F.HasVariant "array" F.Slot Value (R.List Value)) where
    theVariant  = (F.Variant (GH.ptrField 0) 4)
instance (F.HasVariant "object" F.Slot Value (R.List Value'Field)) where
    theVariant  = (F.Variant (GH.ptrField 0) 5)
instance (F.HasVariant "call" F.Slot Value Value'Call) where
    theVariant  = (F.Variant (GH.ptrField 0) 6)
data Value'Field 
type instance (R.ReprFor Value'Field) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Value'Field) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Value'Field) where
    type AllocHint Value'Field = ()
    new  = GH.newStruct
instance (F.HasField "name" F.Slot Value'Field Basics.Text) where
    theField  = (GH.ptrField 0)
instance (F.HasField "value" F.Slot Value'Field Value) where
    theField  = (GH.ptrField 1)
data Value'Call 
type instance (R.ReprFor Value'Call) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Value'Call) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Value'Call) where
    type AllocHint Value'Call = ()
    new  = GH.newStruct
instance (F.HasField "function" F.Slot Value'Call Basics.Text) where
    theField  = (GH.ptrField 0)
instance (F.HasField "params" F.Slot Value'Call (R.List Value)) where
    theField  = (GH.ptrField 1)
data FlattenOptions 
type instance (R.ReprFor FlattenOptions) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct FlattenOptions) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate FlattenOptions) where
    type AllocHint FlattenOptions = ()
    new  = GH.newStruct
instance (F.HasField "prefix" F.Slot FlattenOptions Basics.Text) where
    theField  = (GH.ptrField 0)
data DiscriminatorOptions 
type instance (R.ReprFor DiscriminatorOptions) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct DiscriminatorOptions) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate DiscriminatorOptions) where
    type AllocHint DiscriminatorOptions = ()
    new  = GH.newStruct
instance (F.HasField "name" F.Slot DiscriminatorOptions Basics.Text) where
    theField  = (GH.ptrField 0)
instance (F.HasField "valueName" F.Slot DiscriminatorOptions Basics.Text) where
    theField  = (GH.ptrField 1)