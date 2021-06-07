{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.Compat.Json.New where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.New.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers.New as GH
import qualified Capnp.New.Classes as C
import qualified GHC.Generics as Generics
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
data instance C.Parsed Value
    = Value 
        {union' :: (C.Parsed (GH.Which Value))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Value))
deriving instance (Std_.Eq (C.Parsed Value))
instance (GH.HasUnion Value) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich mut_ Value
        = RW_Value'null (R.Raw mut_ ())
        | RW_Value'boolean (R.Raw mut_ Std_.Bool)
        | RW_Value'number (R.Raw mut_ Std_.Double)
        | RW_Value'string (R.Raw mut_ Basics.Text)
        | RW_Value'array (R.Raw mut_ (R.List Value))
        | RW_Value'object (R.Raw mut_ (R.List Value'Field))
        | RW_Value'call (R.Raw mut_ Value'Call)
        | RW_Value'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Value'null <$> (GH.readVariant #null struct_))
        1 ->
            (RW_Value'boolean <$> (GH.readVariant #boolean struct_))
        2 ->
            (RW_Value'number <$> (GH.readVariant #number struct_))
        3 ->
            (RW_Value'string <$> (GH.readVariant #string struct_))
        4 ->
            (RW_Value'array <$> (GH.readVariant #array struct_))
        5 ->
            (RW_Value'object <$> (GH.readVariant #object struct_))
        6 ->
            (RW_Value'call <$> (GH.readVariant #call struct_))
        _ ->
            (Std_.pure (RW_Value'unknown' tag_))
    data Which Value
instance (GH.HasVariant "null" GH.Slot Value ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance (GH.HasVariant "boolean" GH.Slot Value Std_.Bool) where
    variantByLabel  = (GH.Variant (GH.dataField 16 0 1 0) 1)
instance (GH.HasVariant "number" GH.Slot Value Std_.Double) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 64 0) 2)
instance (GH.HasVariant "string" GH.Slot Value Basics.Text) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 3)
instance (GH.HasVariant "array" GH.Slot Value (R.List Value)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 4)
instance (GH.HasVariant "object" GH.Slot Value (R.List Value'Field)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 5)
instance (GH.HasVariant "call" GH.Slot Value Value'Call) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 6)
data instance C.Parsed (GH.Which Value)
    = Value'null 
    | Value'boolean (RP.Parsed Std_.Bool)
    | Value'number (RP.Parsed Std_.Double)
    | Value'string (RP.Parsed Basics.Text)
    | Value'array (RP.Parsed (R.List Value))
    | Value'object (RP.Parsed (R.List Value'Field))
    | Value'call (RP.Parsed Value'Call)
    | Value'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Value)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Value)))
data Value'Field 
type instance (R.ReprFor Value'Field) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Value'Field) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Value'Field) where
    type AllocHint Value'Field = ()
    new  = GH.newStruct
data instance C.Parsed Value'Field
    = Value'Field 
        {name :: (RP.Parsed Basics.Text)
        ,value :: (RP.Parsed Value)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Value'Field))
deriving instance (Std_.Eq (C.Parsed Value'Field))
instance (GH.HasField "name" GH.Slot Value'Field Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "value" GH.Slot Value'Field Value) where
    fieldByLabel  = (GH.ptrField 1)
data Value'Call 
type instance (R.ReprFor Value'Call) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct Value'Call) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Value'Call) where
    type AllocHint Value'Call = ()
    new  = GH.newStruct
data instance C.Parsed Value'Call
    = Value'Call 
        {function :: (RP.Parsed Basics.Text)
        ,params :: (RP.Parsed (R.List Value))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Value'Call))
deriving instance (Std_.Eq (C.Parsed Value'Call))
instance (GH.HasField "function" GH.Slot Value'Call Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "params" GH.Slot Value'Call (R.List Value)) where
    fieldByLabel  = (GH.ptrField 1)
data FlattenOptions 
type instance (R.ReprFor FlattenOptions) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct FlattenOptions) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate FlattenOptions) where
    type AllocHint FlattenOptions = ()
    new  = GH.newStruct
data instance C.Parsed FlattenOptions
    = FlattenOptions 
        {prefix :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed FlattenOptions))
deriving instance (Std_.Eq (C.Parsed FlattenOptions))
instance (GH.HasField "prefix" GH.Slot FlattenOptions Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data DiscriminatorOptions 
type instance (R.ReprFor DiscriminatorOptions) = (R.Ptr (Std_.Just R.Struct))
instance (C.TypedStruct DiscriminatorOptions) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate DiscriminatorOptions) where
    type AllocHint DiscriminatorOptions = ()
    new  = GH.newStruct
data instance C.Parsed DiscriminatorOptions
    = DiscriminatorOptions 
        {name :: (RP.Parsed Basics.Text)
        ,valueName :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiscriminatorOptions))
deriving instance (Std_.Eq (C.Parsed DiscriminatorOptions))
instance (GH.HasField "name" GH.Slot DiscriminatorOptions Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "valueName" GH.Slot DiscriminatorOptions Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)