{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Capnp.Gen.Capnp.Json.Pure(JsonValue(..)
                                ,JsonValue'Field(..)
                                ,JsonValue'Call(..)) where
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Default as Default
import qualified GHC.Generics as Generics
import qualified Control.Monad.IO.Class as MonadIO
import qualified Capnp.Untyped.Pure as UntypedPure
import qualified Capnp.Untyped as Untyped
import qualified Capnp.Message as Message
import qualified Capnp.Classes as Classes
import qualified Capnp.Basics.Pure as BasicsPure
import qualified Capnp.GenHelpers.Pure as GenHelpersPure
import qualified Capnp.Gen.ById.X8ef99297a43a5e34
import qualified Capnp.Gen.ById.Xbdf87d7bb8304e81
import qualified Capnp.Gen.ById.Xbdf87d7bb8304e81.Pure
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data JsonValue 
    = JsonValue'null 
    | JsonValue'boolean Std_.Bool
    | JsonValue'number Std_.Double
    | JsonValue'string T.Text
    | JsonValue'array (V.Vector JsonValue)
    | JsonValue'object (V.Vector JsonValue'Field)
    | JsonValue'call JsonValue'Call
    | JsonValue'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default JsonValue) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg JsonValue) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize JsonValue) where
    type Cerial msg JsonValue = (Capnp.Gen.ById.X8ef99297a43a5e34.JsonValue msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.X8ef99297a43a5e34.get_JsonValue' raw)
        case raw of
            (Capnp.Gen.ById.X8ef99297a43a5e34.JsonValue'null) ->
                (Std_.pure JsonValue'null)
            (Capnp.Gen.ById.X8ef99297a43a5e34.JsonValue'boolean raw) ->
                (Std_.pure (JsonValue'boolean raw))
            (Capnp.Gen.ById.X8ef99297a43a5e34.JsonValue'number raw) ->
                (Std_.pure (JsonValue'number raw))
            (Capnp.Gen.ById.X8ef99297a43a5e34.JsonValue'string raw) ->
                (JsonValue'string <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.X8ef99297a43a5e34.JsonValue'array raw) ->
                (JsonValue'array <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.X8ef99297a43a5e34.JsonValue'object raw) ->
                (JsonValue'object <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.X8ef99297a43a5e34.JsonValue'call raw) ->
                (JsonValue'call <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.X8ef99297a43a5e34.JsonValue'unknown' tag) ->
                (Std_.pure (JsonValue'unknown' tag))
        )
instance (Classes.Marshal JsonValue) where
    marshalInto raw_ value_ = case value_ of
        (JsonValue'null) ->
            ((Capnp.Gen.ById.X8ef99297a43a5e34.set_JsonValue'null raw_) ())
        (JsonValue'boolean arg_) ->
            (Capnp.Gen.ById.X8ef99297a43a5e34.set_JsonValue'boolean raw_ arg_)
        (JsonValue'number arg_) ->
            (Capnp.Gen.ById.X8ef99297a43a5e34.set_JsonValue'number raw_ arg_)
        (JsonValue'string arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_JsonValue'string raw_))
        (JsonValue'array arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_JsonValue'array raw_))
        (JsonValue'object arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_JsonValue'object raw_))
        (JsonValue'call arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_JsonValue'call raw_))
        (JsonValue'unknown' tag) ->
            (Capnp.Gen.ById.X8ef99297a43a5e34.set_JsonValue'unknown' raw_ tag)
instance (Classes.Cerialize JsonValue)
instance (Classes.Cerialize (V.Vector JsonValue)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector JsonValue))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector JsonValue)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector JsonValue))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JsonValue)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JsonValue))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JsonValue)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data JsonValue'Field 
    = JsonValue'Field 
        {name :: T.Text
        ,value :: JsonValue}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default JsonValue'Field) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg JsonValue'Field) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize JsonValue'Field) where
    type Cerial msg JsonValue'Field = (Capnp.Gen.ById.X8ef99297a43a5e34.JsonValue'Field msg)
    decerialize raw = (JsonValue'Field <$> ((Capnp.Gen.ById.X8ef99297a43a5e34.get_JsonValue'Field'name raw) >>= Classes.decerialize)
                                       <*> ((Capnp.Gen.ById.X8ef99297a43a5e34.get_JsonValue'Field'value raw) >>= Classes.decerialize))
instance (Classes.Marshal JsonValue'Field) where
    marshalInto raw_ value_ = case value_ of
        JsonValue'Field{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) name) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_JsonValue'Field'name raw_))
                ((Classes.cerialize (Untyped.message raw_) value) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_JsonValue'Field'value raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize JsonValue'Field)
instance (Classes.Cerialize (V.Vector JsonValue'Field)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector JsonValue'Field))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector JsonValue'Field)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector JsonValue'Field))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JsonValue'Field)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JsonValue'Field))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JsonValue'Field)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data JsonValue'Call 
    = JsonValue'Call 
        {function :: T.Text
        ,params :: (V.Vector JsonValue)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default JsonValue'Call) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg JsonValue'Call) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize JsonValue'Call) where
    type Cerial msg JsonValue'Call = (Capnp.Gen.ById.X8ef99297a43a5e34.JsonValue'Call msg)
    decerialize raw = (JsonValue'Call <$> ((Capnp.Gen.ById.X8ef99297a43a5e34.get_JsonValue'Call'function raw) >>= Classes.decerialize)
                                      <*> ((Capnp.Gen.ById.X8ef99297a43a5e34.get_JsonValue'Call'params raw) >>= Classes.decerialize))
instance (Classes.Marshal JsonValue'Call) where
    marshalInto raw_ value_ = case value_ of
        JsonValue'Call{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) function) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_JsonValue'Call'function raw_))
                ((Classes.cerialize (Untyped.message raw_) params) >>= (Capnp.Gen.ById.X8ef99297a43a5e34.set_JsonValue'Call'params raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize JsonValue'Call)
instance (Classes.Cerialize (V.Vector JsonValue'Call)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector JsonValue'Call))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector JsonValue'Call)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector JsonValue'Call))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JsonValue'Call)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JsonValue'Call))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector JsonValue'Call)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec