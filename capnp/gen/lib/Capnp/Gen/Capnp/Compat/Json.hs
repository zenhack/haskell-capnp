{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.Compat.Json where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers as GH
import qualified Capnp.Classes as C
import qualified GHC.Generics as Generics
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Value 
type instance (R.ReprFor Value) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Value) where
    typeId  = 11815888814287216003
instance (C.TypedStruct Value) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate Value) where
    type AllocHint Value = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Value (C.Parsed Value))
instance (C.AllocateList Value) where
    type ListAllocHint Value = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Value (C.Parsed Value))
data instance C.Parsed Value
    = Value 
        {union' :: (C.Parsed (GH.Which Value))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Value))
deriving instance (Std_.Eq (C.Parsed Value))
instance (C.Parse Value (C.Parsed Value)) where
    parse raw_ = (Value <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Value (C.Parsed Value)) where
    marshalInto raw_ Value{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Value) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich Value mut_
        = RW_Value'null (R.Raw () mut_)
        | RW_Value'boolean (R.Raw Std_.Bool mut_)
        | RW_Value'number (R.Raw Std_.Double mut_)
        | RW_Value'string (R.Raw Basics.Text mut_)
        | RW_Value'array (R.Raw (R.List Value) mut_)
        | RW_Value'object (R.Raw (R.List Value'Field) mut_)
        | RW_Value'call (R.Raw Value'Call mut_)
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
instance (C.Parse (GH.Which Value) (C.Parsed (GH.Which Value))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Value'null _) ->
                (Std_.pure Value'null)
            (RW_Value'boolean rawArg_) ->
                (Value'boolean <$> (C.parse rawArg_))
            (RW_Value'number rawArg_) ->
                (Value'number <$> (C.parse rawArg_))
            (RW_Value'string rawArg_) ->
                (Value'string <$> (C.parse rawArg_))
            (RW_Value'array rawArg_) ->
                (Value'array <$> (C.parse rawArg_))
            (RW_Value'object rawArg_) ->
                (Value'object <$> (C.parse rawArg_))
            (RW_Value'call rawArg_) ->
                (Value'call <$> (C.parse rawArg_))
            (RW_Value'unknown' tag_) ->
                (Std_.pure (Value'unknown' tag_))
        )
instance (C.Marshal (GH.Which Value) (C.Parsed (GH.Which Value))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Value'null) ->
            (GH.encodeVariant #null () (GH.unionStruct raw_))
        (Value'boolean arg_) ->
            (GH.encodeVariant #boolean arg_ (GH.unionStruct raw_))
        (Value'number arg_) ->
            (GH.encodeVariant #number arg_ (GH.unionStruct raw_))
        (Value'string arg_) ->
            (GH.encodeVariant #string arg_ (GH.unionStruct raw_))
        (Value'array arg_) ->
            (GH.encodeVariant #array arg_ (GH.unionStruct raw_))
        (Value'object arg_) ->
            (GH.encodeVariant #object arg_ (GH.unionStruct raw_))
        (Value'call arg_) ->
            (GH.encodeVariant #call arg_ (GH.unionStruct raw_))
        (Value'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Value'Field 
type instance (R.ReprFor Value'Field) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Value'Field) where
    typeId  = 16361620220719570399
instance (C.TypedStruct Value'Field) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Value'Field) where
    type AllocHint Value'Field = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Value'Field (C.Parsed Value'Field))
instance (C.AllocateList Value'Field) where
    type ListAllocHint Value'Field = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Value'Field (C.Parsed Value'Field))
data instance C.Parsed Value'Field
    = Value'Field 
        {name :: (RP.Parsed Basics.Text)
        ,value :: (RP.Parsed Value)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Value'Field))
deriving instance (Std_.Eq (C.Parsed Value'Field))
instance (C.Parse Value'Field (C.Parsed Value'Field)) where
    parse raw_ = (Value'Field <$> (GH.parseField #name raw_)
                              <*> (GH.parseField #value raw_))
instance (C.Marshal Value'Field (C.Parsed Value'Field)) where
    marshalInto raw_ Value'Field{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #value value raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot Value'Field Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "value" GH.Slot Value'Field Value) where
    fieldByLabel  = (GH.ptrField 1)
data Value'Call 
type instance (R.ReprFor Value'Call) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Value'Call) where
    typeId  = 11590566612201717064
instance (C.TypedStruct Value'Call) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Value'Call) where
    type AllocHint Value'Call = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Value'Call (C.Parsed Value'Call))
instance (C.AllocateList Value'Call) where
    type ListAllocHint Value'Call = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Value'Call (C.Parsed Value'Call))
data instance C.Parsed Value'Call
    = Value'Call 
        {function :: (RP.Parsed Basics.Text)
        ,params :: (RP.Parsed (R.List Value))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Value'Call))
deriving instance (Std_.Eq (C.Parsed Value'Call))
instance (C.Parse Value'Call (C.Parsed Value'Call)) where
    parse raw_ = (Value'Call <$> (GH.parseField #function raw_)
                             <*> (GH.parseField #params raw_))
instance (C.Marshal Value'Call (C.Parsed Value'Call)) where
    marshalInto raw_ Value'Call{..} = (do
        (GH.encodeField #function function raw_)
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "function" GH.Slot Value'Call Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "params" GH.Slot Value'Call (R.List Value)) where
    fieldByLabel  = (GH.ptrField 1)
data FlattenOptions 
type instance (R.ReprFor FlattenOptions) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId FlattenOptions) where
    typeId  = 14186078402951440993
instance (C.TypedStruct FlattenOptions) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate FlattenOptions) where
    type AllocHint FlattenOptions = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc FlattenOptions (C.Parsed FlattenOptions))
instance (C.AllocateList FlattenOptions) where
    type ListAllocHint FlattenOptions = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc FlattenOptions (C.Parsed FlattenOptions))
data instance C.Parsed FlattenOptions
    = FlattenOptions 
        {prefix :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed FlattenOptions))
deriving instance (Std_.Eq (C.Parsed FlattenOptions))
instance (C.Parse FlattenOptions (C.Parsed FlattenOptions)) where
    parse raw_ = (FlattenOptions <$> (GH.parseField #prefix raw_))
instance (C.Marshal FlattenOptions (C.Parsed FlattenOptions)) where
    marshalInto raw_ FlattenOptions{..} = (do
        (GH.encodeField #prefix prefix raw_)
        (Std_.pure ())
        )
instance (GH.HasField "prefix" GH.Slot FlattenOptions Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data DiscriminatorOptions 
type instance (R.ReprFor DiscriminatorOptions) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiscriminatorOptions) where
    typeId  = 14049192395069608729
instance (C.TypedStruct DiscriminatorOptions) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate DiscriminatorOptions) where
    type AllocHint DiscriminatorOptions = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiscriminatorOptions (C.Parsed DiscriminatorOptions))
instance (C.AllocateList DiscriminatorOptions) where
    type ListAllocHint DiscriminatorOptions = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiscriminatorOptions (C.Parsed DiscriminatorOptions))
data instance C.Parsed DiscriminatorOptions
    = DiscriminatorOptions 
        {name :: (RP.Parsed Basics.Text)
        ,valueName :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiscriminatorOptions))
deriving instance (Std_.Eq (C.Parsed DiscriminatorOptions))
instance (C.Parse DiscriminatorOptions (C.Parsed DiscriminatorOptions)) where
    parse raw_ = (DiscriminatorOptions <$> (GH.parseField #name raw_)
                                       <*> (GH.parseField #valueName raw_))
instance (C.Marshal DiscriminatorOptions (C.Parsed DiscriminatorOptions)) where
    marshalInto raw_ DiscriminatorOptions{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #valueName valueName raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot DiscriminatorOptions Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "valueName" GH.Slot DiscriminatorOptions Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)