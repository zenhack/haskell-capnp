{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Capnp.Gen.Capnp.Compat.Json.Pure(Value(..)
                                       ,Value'Field(..)
                                       ,Value'Call(..)
                                       ,FlattenOptions(..)
                                       ,DiscriminatorOptions(..)) where
import qualified Capnp.GenHelpers.ReExports.Data.Vector as V
import qualified Capnp.GenHelpers.ReExports.Data.Text as T
import qualified Capnp.GenHelpers.ReExports.Data.ByteString as BS
import qualified Capnp.GenHelpers.ReExports.Data.Default as Default
import qualified GHC.Generics as Generics
import qualified Control.Monad.IO.Class as MonadIO
import qualified Capnp.Untyped.Pure as UntypedPure
import qualified Capnp.Untyped as Untyped
import qualified Capnp.Message as Message
import qualified Capnp.Classes as Classes
import qualified Capnp.Basics.Pure as BasicsPure
import qualified Capnp.GenHelpers.Pure as GenHelpersPure
import qualified Capnp.Gen.ById.X8ef99297a43a5e34
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Value 
    = Value'null 
    | Value'boolean Std_.Bool
    | Value'number Std_.Double
    | Value'string T.Text
    | Value'array (V.Vector Value)
    | Value'object (V.Vector Value'Field)
    | Value'call Value'Call
    | Value'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Value) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.Const Value) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Value) where
    type Cerial msg Value = (Capnp.Gen.ById.X8ef99297a43a5e34.Value msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.X8ef99297a43a5e34.get_Value' raw)
        case raw of
            (Capnp.Gen.ById.X8ef99297a43a5e34.Value'null) ->
                (Std_.pure Value'null)
            (Capnp.Gen.ById.X8ef99297a43a5e34.Value'boolean raw) ->
                (Std_.pure (Value'boolean raw))
            (Capnp.Gen.ById.X8ef99297a43a5e34.Value'number raw) ->
                (Std_.pure (Value'number raw))
            (Capnp.Gen.ById.X8ef99297a43a5e34.Value'string raw) ->
                (Value'string <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.X8ef99297a43a5e34.Value'array raw) ->
                (Value'array <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.X8ef99297a43a5e34.Value'object raw) ->
                (Value'object <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.X8ef99297a43a5e34.Value'call raw) ->
                (Value'call <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.X8ef99297a43a5e34.Value'unknown' tag) ->
                (Std_.pure (Value'unknown' tag))
        )
instance (Classes.Marshal s Value) where
    marshalInto raw_ value_ = case value_ of
        (Value'null) ->
            (Capnp.Gen.ById.X8ef99297a43a5e34.set_Value'null raw_)
        (Value'boolean arg_) ->
            (Capnp.Gen.ById.X8ef99297a43a5e34.set_Value'boolean raw_ arg_)
        (Value'number arg_) ->
            (Capnp.Gen.ById.X8ef99297a43a5e34.set_Value'number raw_ arg_)
        (Value'string arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_Value'string raw_))
        (Value'array arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_Value'array raw_))
        (Value'object arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_Value'object raw_))
        (Value'call arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_Value'call raw_))
        (Value'unknown' tag) ->
            (Capnp.Gen.ById.X8ef99297a43a5e34.set_Value'unknown' raw_ tag)
instance (Classes.Cerialize s Value)
instance (Classes.Cerialize s (V.Vector Value)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize s (V.Vector (V.Vector Value))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector Value)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector Value))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Value)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Value))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Value)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Value'Field 
    = Value'Field 
        {name :: T.Text
        ,value :: Value}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Value'Field) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.Const Value'Field) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Value'Field) where
    type Cerial msg Value'Field = (Capnp.Gen.ById.X8ef99297a43a5e34.Value'Field msg)
    decerialize raw = (Value'Field <$> ((Capnp.Gen.ById.X8ef99297a43a5e34.get_Value'Field'name raw) >>= Classes.decerialize)
                                   <*> ((Capnp.Gen.ById.X8ef99297a43a5e34.get_Value'Field'value raw) >>= Classes.decerialize))
instance (Classes.Marshal s Value'Field) where
    marshalInto raw_ value_ = case value_ of
        Value'Field{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) name) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_Value'Field'name raw_))
                ((Classes.cerialize (Untyped.message raw_) value) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_Value'Field'value raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize s Value'Field)
instance (Classes.Cerialize s (V.Vector Value'Field)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize s (V.Vector (V.Vector Value'Field))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector Value'Field)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector Value'Field))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Value'Field)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Value'Field))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Value'Field)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Value'Call 
    = Value'Call 
        {function :: T.Text
        ,params :: (V.Vector Value)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Value'Call) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.Const Value'Call) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Value'Call) where
    type Cerial msg Value'Call = (Capnp.Gen.ById.X8ef99297a43a5e34.Value'Call msg)
    decerialize raw = (Value'Call <$> ((Capnp.Gen.ById.X8ef99297a43a5e34.get_Value'Call'function raw) >>= Classes.decerialize)
                                  <*> ((Capnp.Gen.ById.X8ef99297a43a5e34.get_Value'Call'params raw) >>= Classes.decerialize))
instance (Classes.Marshal s Value'Call) where
    marshalInto raw_ value_ = case value_ of
        Value'Call{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) function) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_Value'Call'function raw_))
                ((Classes.cerialize (Untyped.message raw_) params) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_Value'Call'params raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize s Value'Call)
instance (Classes.Cerialize s (V.Vector Value'Call)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize s (V.Vector (V.Vector Value'Call))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector Value'Call)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector Value'Call))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Value'Call)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Value'Call))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Value'Call)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data FlattenOptions 
    = FlattenOptions 
        {prefix :: T.Text}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default FlattenOptions) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.Const FlattenOptions) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize FlattenOptions) where
    type Cerial msg FlattenOptions = (Capnp.Gen.ById.X8ef99297a43a5e34.FlattenOptions msg)
    decerialize raw = (FlattenOptions <$> ((Capnp.Gen.ById.X8ef99297a43a5e34.get_FlattenOptions'prefix raw) >>= Classes.decerialize))
instance (Classes.Marshal s FlattenOptions) where
    marshalInto raw_ value_ = case value_ of
        FlattenOptions{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) prefix) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_FlattenOptions'prefix raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize s FlattenOptions)
instance (Classes.Cerialize s (V.Vector FlattenOptions)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize s (V.Vector (V.Vector FlattenOptions))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector FlattenOptions)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector FlattenOptions))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector FlattenOptions)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector FlattenOptions))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector FlattenOptions)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data DiscriminatorOptions 
    = DiscriminatorOptions 
        {name :: T.Text
        ,valueName :: T.Text}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default DiscriminatorOptions) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.Const DiscriminatorOptions) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize DiscriminatorOptions) where
    type Cerial msg DiscriminatorOptions = (Capnp.Gen.ById.X8ef99297a43a5e34.DiscriminatorOptions msg)
    decerialize raw = (DiscriminatorOptions <$> ((Capnp.Gen.ById.X8ef99297a43a5e34.get_DiscriminatorOptions'name raw) >>= Classes.decerialize)
                                            <*> ((Capnp.Gen.ById.X8ef99297a43a5e34.get_DiscriminatorOptions'valueName raw) >>= Classes.decerialize))
instance (Classes.Marshal s DiscriminatorOptions) where
    marshalInto raw_ value_ = case value_ of
        DiscriminatorOptions{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) name) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_DiscriminatorOptions'name raw_))
                ((Classes.cerialize (Untyped.message raw_) valueName) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_DiscriminatorOptions'valueName raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize s DiscriminatorOptions)
instance (Classes.Cerialize s (V.Vector DiscriminatorOptions)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize s (V.Vector (V.Vector DiscriminatorOptions))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector DiscriminatorOptions)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector DiscriminatorOptions))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector DiscriminatorOptions)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector DiscriminatorOptions))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector DiscriminatorOptions)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec