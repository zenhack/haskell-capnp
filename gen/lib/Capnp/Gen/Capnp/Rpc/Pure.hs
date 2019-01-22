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
module Capnp.Gen.Capnp.Rpc.Pure(Capnp.Gen.ById.Xb312981b2552a250.Exception'Type(..)
                               ,Message(..)
                               ,Bootstrap(..)
                               ,Call(..)
                               ,Call'sendResultsTo(..)
                               ,Return(..)
                               ,Return'(..)
                               ,Finish(..)
                               ,Resolve(..)
                               ,Resolve'(..)
                               ,Release(..)
                               ,Disembargo(..)
                               ,Disembargo'context(..)
                               ,Provide(..)
                               ,Accept(..)
                               ,Join(..)
                               ,MessageTarget(..)
                               ,Payload(..)
                               ,CapDescriptor(..)
                               ,PromisedAnswer(..)
                               ,PromisedAnswer'Op(..)
                               ,ThirdPartyCapDescriptor(..)
                               ,Exception(..)) where
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
import qualified Capnp.Gen.ById.Xb312981b2552a250
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Message 
    = Message'unimplemented Message
    | Message'abort Exception
    | Message'call Call
    | Message'return Return
    | Message'finish Finish
    | Message'resolve Resolve
    | Message'release Release
    | Message'obsoleteSave (Std_.Maybe UntypedPure.Ptr)
    | Message'bootstrap Bootstrap
    | Message'obsoleteDelete (Std_.Maybe UntypedPure.Ptr)
    | Message'provide Provide
    | Message'accept Accept
    | Message'join Join
    | Message'disembargo Disembargo
    | Message'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Message) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Message) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Message) where
    type Cerial msg Message = (Capnp.Gen.ById.Xb312981b2552a250.Message msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xb312981b2552a250.get_Message' raw)
        case raw of
            (Capnp.Gen.ById.Xb312981b2552a250.Message'unimplemented raw) ->
                (Message'unimplemented <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Message'abort raw) ->
                (Message'abort <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Message'call raw) ->
                (Message'call <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Message'return raw) ->
                (Message'return <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Message'finish raw) ->
                (Message'finish <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Message'resolve raw) ->
                (Message'resolve <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Message'release raw) ->
                (Message'release <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Message'obsoleteSave raw) ->
                (Message'obsoleteSave <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Message'bootstrap raw) ->
                (Message'bootstrap <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Message'obsoleteDelete raw) ->
                (Message'obsoleteDelete <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Message'provide raw) ->
                (Message'provide <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Message'accept raw) ->
                (Message'accept <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Message'join raw) ->
                (Message'join <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Message'disembargo raw) ->
                (Message'disembargo <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Message'unknown' tag) ->
                (Std_.pure (Message'unknown' tag))
        )
instance (Classes.Marshal Message) where
    marshalInto raw_ value_ = case value_ of
        (Message'unimplemented arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Message'unimplemented raw_))
        (Message'abort arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Message'abort raw_))
        (Message'call arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Message'call raw_))
        (Message'return arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Message'return raw_))
        (Message'finish arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Message'finish raw_))
        (Message'resolve arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Message'resolve raw_))
        (Message'release arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Message'release raw_))
        (Message'obsoleteSave arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Message'obsoleteSave raw_))
        (Message'bootstrap arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Message'bootstrap raw_))
        (Message'obsoleteDelete arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Message'obsoleteDelete raw_))
        (Message'provide arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Message'provide raw_))
        (Message'accept arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Message'accept raw_))
        (Message'join arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Message'join raw_))
        (Message'disembargo arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Message'disembargo raw_))
        (Message'unknown' tag) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_Message'unknown' raw_ tag)
instance (Classes.Cerialize Message)
instance (Classes.Cerialize (V.Vector Message)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Message))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Message)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Message))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Message)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Message))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Message)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Bootstrap 
    = Bootstrap 
        {questionId :: Std_.Word32
        ,deprecatedObjectId :: (Std_.Maybe UntypedPure.Ptr)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Bootstrap) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Bootstrap) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Bootstrap) where
    type Cerial msg Bootstrap = (Capnp.Gen.ById.Xb312981b2552a250.Bootstrap msg)
    decerialize raw = (Bootstrap <$> (Capnp.Gen.ById.Xb312981b2552a250.get_Bootstrap'questionId raw)
                                 <*> ((Capnp.Gen.ById.Xb312981b2552a250.get_Bootstrap'deprecatedObjectId raw) >>= Classes.decerialize))
instance (Classes.Marshal Bootstrap) where
    marshalInto raw_ value_ = case value_ of
        Bootstrap{..} ->
            (do
                (Capnp.Gen.ById.Xb312981b2552a250.set_Bootstrap'questionId raw_ questionId)
                ((Classes.cerialize (Untyped.message raw_) deprecatedObjectId) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Bootstrap'deprecatedObjectId raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Bootstrap)
instance (Classes.Cerialize (V.Vector Bootstrap)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Bootstrap))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Bootstrap)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Bootstrap))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Bootstrap)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Bootstrap))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Bootstrap)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Call 
    = Call 
        {questionId :: Std_.Word32
        ,target :: MessageTarget
        ,interfaceId :: Std_.Word64
        ,methodId :: Std_.Word16
        ,params :: Payload
        ,sendResultsTo :: Call'sendResultsTo
        ,allowThirdPartyTailCall :: Std_.Bool}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Call) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Call) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Call) where
    type Cerial msg Call = (Capnp.Gen.ById.Xb312981b2552a250.Call msg)
    decerialize raw = (Call <$> (Capnp.Gen.ById.Xb312981b2552a250.get_Call'questionId raw)
                            <*> ((Capnp.Gen.ById.Xb312981b2552a250.get_Call'target raw) >>= Classes.decerialize)
                            <*> (Capnp.Gen.ById.Xb312981b2552a250.get_Call'interfaceId raw)
                            <*> (Capnp.Gen.ById.Xb312981b2552a250.get_Call'methodId raw)
                            <*> ((Capnp.Gen.ById.Xb312981b2552a250.get_Call'params raw) >>= Classes.decerialize)
                            <*> ((Capnp.Gen.ById.Xb312981b2552a250.get_Call'sendResultsTo raw) >>= Classes.decerialize)
                            <*> (Capnp.Gen.ById.Xb312981b2552a250.get_Call'allowThirdPartyTailCall raw))
instance (Classes.Marshal Call) where
    marshalInto raw_ value_ = case value_ of
        Call{..} ->
            (do
                (Capnp.Gen.ById.Xb312981b2552a250.set_Call'questionId raw_ questionId)
                ((Classes.cerialize (Untyped.message raw_) target) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Call'target raw_))
                (Capnp.Gen.ById.Xb312981b2552a250.set_Call'interfaceId raw_ interfaceId)
                (Capnp.Gen.ById.Xb312981b2552a250.set_Call'methodId raw_ methodId)
                ((Classes.cerialize (Untyped.message raw_) params) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Call'params raw_))
                (do
                    raw_ <- (Capnp.Gen.ById.Xb312981b2552a250.get_Call'sendResultsTo raw_)
                    (Classes.marshalInto raw_ sendResultsTo)
                    )
                (Capnp.Gen.ById.Xb312981b2552a250.set_Call'allowThirdPartyTailCall raw_ allowThirdPartyTailCall)
                (Std_.pure ())
                )
instance (Classes.Cerialize Call)
instance (Classes.Cerialize (V.Vector Call)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Call))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Call)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Call))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Call)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Call))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Call)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Call'sendResultsTo 
    = Call'sendResultsTo'caller 
    | Call'sendResultsTo'yourself 
    | Call'sendResultsTo'thirdParty (Std_.Maybe UntypedPure.Ptr)
    | Call'sendResultsTo'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Call'sendResultsTo) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Call'sendResultsTo) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Call'sendResultsTo) where
    type Cerial msg Call'sendResultsTo = (Capnp.Gen.ById.Xb312981b2552a250.Call'sendResultsTo msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xb312981b2552a250.get_Call'sendResultsTo' raw)
        case raw of
            (Capnp.Gen.ById.Xb312981b2552a250.Call'sendResultsTo'caller) ->
                (Std_.pure Call'sendResultsTo'caller)
            (Capnp.Gen.ById.Xb312981b2552a250.Call'sendResultsTo'yourself) ->
                (Std_.pure Call'sendResultsTo'yourself)
            (Capnp.Gen.ById.Xb312981b2552a250.Call'sendResultsTo'thirdParty raw) ->
                (Call'sendResultsTo'thirdParty <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Call'sendResultsTo'unknown' tag) ->
                (Std_.pure (Call'sendResultsTo'unknown' tag))
        )
instance (Classes.Marshal Call'sendResultsTo) where
    marshalInto raw_ value_ = case value_ of
        (Call'sendResultsTo'caller) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_Call'sendResultsTo'caller raw_)
        (Call'sendResultsTo'yourself) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_Call'sendResultsTo'yourself raw_)
        (Call'sendResultsTo'thirdParty arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Call'sendResultsTo'thirdParty raw_))
        (Call'sendResultsTo'unknown' tag) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_Call'sendResultsTo'unknown' raw_ tag)
data Return 
    = Return 
        {answerId :: Std_.Word32
        ,releaseParamCaps :: Std_.Bool
        ,union' :: Return'}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Return) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Return) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Return) where
    type Cerial msg Return = (Capnp.Gen.ById.Xb312981b2552a250.Return msg)
    decerialize raw = (Return <$> (Capnp.Gen.ById.Xb312981b2552a250.get_Return'answerId raw)
                              <*> (Capnp.Gen.ById.Xb312981b2552a250.get_Return'releaseParamCaps raw)
                              <*> (Classes.decerialize raw))
instance (Classes.Marshal Return) where
    marshalInto raw_ value_ = case value_ of
        Return{..} ->
            (do
                (Capnp.Gen.ById.Xb312981b2552a250.set_Return'answerId raw_ answerId)
                (Capnp.Gen.ById.Xb312981b2552a250.set_Return'releaseParamCaps raw_ releaseParamCaps)
                (do
                    (Classes.marshalInto raw_ union')
                    )
                (Std_.pure ())
                )
instance (Classes.Cerialize Return)
instance (Classes.Cerialize (V.Vector Return)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Return))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Return)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Return))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Return)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Return))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Return)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Return' 
    = Return'results Payload
    | Return'exception Exception
    | Return'canceled 
    | Return'resultsSentElsewhere 
    | Return'takeFromOtherQuestion Std_.Word32
    | Return'acceptFromThirdParty (Std_.Maybe UntypedPure.Ptr)
    | Return'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Return') where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Return') where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Return') where
    type Cerial msg Return' = (Capnp.Gen.ById.Xb312981b2552a250.Return msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xb312981b2552a250.get_Return' raw)
        case raw of
            (Capnp.Gen.ById.Xb312981b2552a250.Return'results raw) ->
                (Return'results <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Return'exception raw) ->
                (Return'exception <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Return'canceled) ->
                (Std_.pure Return'canceled)
            (Capnp.Gen.ById.Xb312981b2552a250.Return'resultsSentElsewhere) ->
                (Std_.pure Return'resultsSentElsewhere)
            (Capnp.Gen.ById.Xb312981b2552a250.Return'takeFromOtherQuestion raw) ->
                (Std_.pure (Return'takeFromOtherQuestion raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Return'acceptFromThirdParty raw) ->
                (Return'acceptFromThirdParty <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Return'unknown' tag) ->
                (Std_.pure (Return'unknown' tag))
        )
instance (Classes.Marshal Return') where
    marshalInto raw_ value_ = case value_ of
        (Return'results arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Return'results raw_))
        (Return'exception arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Return'exception raw_))
        (Return'canceled) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_Return'canceled raw_)
        (Return'resultsSentElsewhere) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_Return'resultsSentElsewhere raw_)
        (Return'takeFromOtherQuestion arg_) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_Return'takeFromOtherQuestion raw_ arg_)
        (Return'acceptFromThirdParty arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Return'acceptFromThirdParty raw_))
        (Return'unknown' tag) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_Return'unknown' raw_ tag)
data Finish 
    = Finish 
        {questionId :: Std_.Word32
        ,releaseResultCaps :: Std_.Bool}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Finish) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Finish) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Finish) where
    type Cerial msg Finish = (Capnp.Gen.ById.Xb312981b2552a250.Finish msg)
    decerialize raw = (Finish <$> (Capnp.Gen.ById.Xb312981b2552a250.get_Finish'questionId raw)
                              <*> (Capnp.Gen.ById.Xb312981b2552a250.get_Finish'releaseResultCaps raw))
instance (Classes.Marshal Finish) where
    marshalInto raw_ value_ = case value_ of
        Finish{..} ->
            (do
                (Capnp.Gen.ById.Xb312981b2552a250.set_Finish'questionId raw_ questionId)
                (Capnp.Gen.ById.Xb312981b2552a250.set_Finish'releaseResultCaps raw_ releaseResultCaps)
                (Std_.pure ())
                )
instance (Classes.Cerialize Finish)
instance (Classes.Cerialize (V.Vector Finish)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Finish))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Finish)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Finish))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Finish)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Finish))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Finish)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Resolve 
    = Resolve 
        {promiseId :: Std_.Word32
        ,union' :: Resolve'}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Resolve) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Resolve) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Resolve) where
    type Cerial msg Resolve = (Capnp.Gen.ById.Xb312981b2552a250.Resolve msg)
    decerialize raw = (Resolve <$> (Capnp.Gen.ById.Xb312981b2552a250.get_Resolve'promiseId raw)
                               <*> (Classes.decerialize raw))
instance (Classes.Marshal Resolve) where
    marshalInto raw_ value_ = case value_ of
        Resolve{..} ->
            (do
                (Capnp.Gen.ById.Xb312981b2552a250.set_Resolve'promiseId raw_ promiseId)
                (do
                    (Classes.marshalInto raw_ union')
                    )
                (Std_.pure ())
                )
instance (Classes.Cerialize Resolve)
instance (Classes.Cerialize (V.Vector Resolve)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Resolve))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Resolve)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Resolve))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Resolve)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Resolve))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Resolve)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Resolve' 
    = Resolve'cap CapDescriptor
    | Resolve'exception Exception
    | Resolve'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Resolve') where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Resolve') where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Resolve') where
    type Cerial msg Resolve' = (Capnp.Gen.ById.Xb312981b2552a250.Resolve msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xb312981b2552a250.get_Resolve' raw)
        case raw of
            (Capnp.Gen.ById.Xb312981b2552a250.Resolve'cap raw) ->
                (Resolve'cap <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Resolve'exception raw) ->
                (Resolve'exception <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Resolve'unknown' tag) ->
                (Std_.pure (Resolve'unknown' tag))
        )
instance (Classes.Marshal Resolve') where
    marshalInto raw_ value_ = case value_ of
        (Resolve'cap arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Resolve'cap raw_))
        (Resolve'exception arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Resolve'exception raw_))
        (Resolve'unknown' tag) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_Resolve'unknown' raw_ tag)
data Release 
    = Release 
        {id :: Std_.Word32
        ,referenceCount :: Std_.Word32}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Release) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Release) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Release) where
    type Cerial msg Release = (Capnp.Gen.ById.Xb312981b2552a250.Release msg)
    decerialize raw = (Release <$> (Capnp.Gen.ById.Xb312981b2552a250.get_Release'id raw)
                               <*> (Capnp.Gen.ById.Xb312981b2552a250.get_Release'referenceCount raw))
instance (Classes.Marshal Release) where
    marshalInto raw_ value_ = case value_ of
        Release{..} ->
            (do
                (Capnp.Gen.ById.Xb312981b2552a250.set_Release'id raw_ id)
                (Capnp.Gen.ById.Xb312981b2552a250.set_Release'referenceCount raw_ referenceCount)
                (Std_.pure ())
                )
instance (Classes.Cerialize Release)
instance (Classes.Cerialize (V.Vector Release)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Release))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Release)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Release))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Release)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Release))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Release)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Disembargo 
    = Disembargo 
        {target :: MessageTarget
        ,context :: Disembargo'context}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Disembargo) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Disembargo) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Disembargo) where
    type Cerial msg Disembargo = (Capnp.Gen.ById.Xb312981b2552a250.Disembargo msg)
    decerialize raw = (Disembargo <$> ((Capnp.Gen.ById.Xb312981b2552a250.get_Disembargo'target raw) >>= Classes.decerialize)
                                  <*> ((Capnp.Gen.ById.Xb312981b2552a250.get_Disembargo'context raw) >>= Classes.decerialize))
instance (Classes.Marshal Disembargo) where
    marshalInto raw_ value_ = case value_ of
        Disembargo{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) target) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Disembargo'target raw_))
                (do
                    raw_ <- (Capnp.Gen.ById.Xb312981b2552a250.get_Disembargo'context raw_)
                    (Classes.marshalInto raw_ context)
                    )
                (Std_.pure ())
                )
instance (Classes.Cerialize Disembargo)
instance (Classes.Cerialize (V.Vector Disembargo)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Disembargo))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Disembargo)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Disembargo))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Disembargo)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Disembargo))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Disembargo)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Disembargo'context 
    = Disembargo'context'senderLoopback Std_.Word32
    | Disembargo'context'receiverLoopback Std_.Word32
    | Disembargo'context'accept 
    | Disembargo'context'provide Std_.Word32
    | Disembargo'context'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Disembargo'context) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Disembargo'context) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Disembargo'context) where
    type Cerial msg Disembargo'context = (Capnp.Gen.ById.Xb312981b2552a250.Disembargo'context msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xb312981b2552a250.get_Disembargo'context' raw)
        case raw of
            (Capnp.Gen.ById.Xb312981b2552a250.Disembargo'context'senderLoopback raw) ->
                (Std_.pure (Disembargo'context'senderLoopback raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Disembargo'context'receiverLoopback raw) ->
                (Std_.pure (Disembargo'context'receiverLoopback raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Disembargo'context'accept) ->
                (Std_.pure Disembargo'context'accept)
            (Capnp.Gen.ById.Xb312981b2552a250.Disembargo'context'provide raw) ->
                (Std_.pure (Disembargo'context'provide raw))
            (Capnp.Gen.ById.Xb312981b2552a250.Disembargo'context'unknown' tag) ->
                (Std_.pure (Disembargo'context'unknown' tag))
        )
instance (Classes.Marshal Disembargo'context) where
    marshalInto raw_ value_ = case value_ of
        (Disembargo'context'senderLoopback arg_) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_Disembargo'context'senderLoopback raw_ arg_)
        (Disembargo'context'receiverLoopback arg_) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_Disembargo'context'receiverLoopback raw_ arg_)
        (Disembargo'context'accept) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_Disembargo'context'accept raw_)
        (Disembargo'context'provide arg_) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_Disembargo'context'provide raw_ arg_)
        (Disembargo'context'unknown' tag) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_Disembargo'context'unknown' raw_ tag)
data Provide 
    = Provide 
        {questionId :: Std_.Word32
        ,target :: MessageTarget
        ,recipient :: (Std_.Maybe UntypedPure.Ptr)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Provide) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Provide) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Provide) where
    type Cerial msg Provide = (Capnp.Gen.ById.Xb312981b2552a250.Provide msg)
    decerialize raw = (Provide <$> (Capnp.Gen.ById.Xb312981b2552a250.get_Provide'questionId raw)
                               <*> ((Capnp.Gen.ById.Xb312981b2552a250.get_Provide'target raw) >>= Classes.decerialize)
                               <*> ((Capnp.Gen.ById.Xb312981b2552a250.get_Provide'recipient raw) >>= Classes.decerialize))
instance (Classes.Marshal Provide) where
    marshalInto raw_ value_ = case value_ of
        Provide{..} ->
            (do
                (Capnp.Gen.ById.Xb312981b2552a250.set_Provide'questionId raw_ questionId)
                ((Classes.cerialize (Untyped.message raw_) target) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Provide'target raw_))
                ((Classes.cerialize (Untyped.message raw_) recipient) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Provide'recipient raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Provide)
instance (Classes.Cerialize (V.Vector Provide)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Provide))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Provide)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Provide))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Provide)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Provide))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Provide)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Accept 
    = Accept 
        {questionId :: Std_.Word32
        ,provision :: (Std_.Maybe UntypedPure.Ptr)
        ,embargo :: Std_.Bool}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Accept) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Accept) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Accept) where
    type Cerial msg Accept = (Capnp.Gen.ById.Xb312981b2552a250.Accept msg)
    decerialize raw = (Accept <$> (Capnp.Gen.ById.Xb312981b2552a250.get_Accept'questionId raw)
                              <*> ((Capnp.Gen.ById.Xb312981b2552a250.get_Accept'provision raw) >>= Classes.decerialize)
                              <*> (Capnp.Gen.ById.Xb312981b2552a250.get_Accept'embargo raw))
instance (Classes.Marshal Accept) where
    marshalInto raw_ value_ = case value_ of
        Accept{..} ->
            (do
                (Capnp.Gen.ById.Xb312981b2552a250.set_Accept'questionId raw_ questionId)
                ((Classes.cerialize (Untyped.message raw_) provision) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Accept'provision raw_))
                (Capnp.Gen.ById.Xb312981b2552a250.set_Accept'embargo raw_ embargo)
                (Std_.pure ())
                )
instance (Classes.Cerialize Accept)
instance (Classes.Cerialize (V.Vector Accept)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Accept))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Accept)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Accept))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Accept)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Accept))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Accept)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Join 
    = Join 
        {questionId :: Std_.Word32
        ,target :: MessageTarget
        ,keyPart :: (Std_.Maybe UntypedPure.Ptr)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Join) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Join) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Join) where
    type Cerial msg Join = (Capnp.Gen.ById.Xb312981b2552a250.Join msg)
    decerialize raw = (Join <$> (Capnp.Gen.ById.Xb312981b2552a250.get_Join'questionId raw)
                            <*> ((Capnp.Gen.ById.Xb312981b2552a250.get_Join'target raw) >>= Classes.decerialize)
                            <*> ((Capnp.Gen.ById.Xb312981b2552a250.get_Join'keyPart raw) >>= Classes.decerialize))
instance (Classes.Marshal Join) where
    marshalInto raw_ value_ = case value_ of
        Join{..} ->
            (do
                (Capnp.Gen.ById.Xb312981b2552a250.set_Join'questionId raw_ questionId)
                ((Classes.cerialize (Untyped.message raw_) target) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Join'target raw_))
                ((Classes.cerialize (Untyped.message raw_) keyPart) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Join'keyPart raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Join)
instance (Classes.Cerialize (V.Vector Join)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Join))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Join)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Join))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Join)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Join))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Join)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data MessageTarget 
    = MessageTarget'importedCap Std_.Word32
    | MessageTarget'promisedAnswer PromisedAnswer
    | MessageTarget'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default MessageTarget) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg MessageTarget) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize MessageTarget) where
    type Cerial msg MessageTarget = (Capnp.Gen.ById.Xb312981b2552a250.MessageTarget msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xb312981b2552a250.get_MessageTarget' raw)
        case raw of
            (Capnp.Gen.ById.Xb312981b2552a250.MessageTarget'importedCap raw) ->
                (Std_.pure (MessageTarget'importedCap raw))
            (Capnp.Gen.ById.Xb312981b2552a250.MessageTarget'promisedAnswer raw) ->
                (MessageTarget'promisedAnswer <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.MessageTarget'unknown' tag) ->
                (Std_.pure (MessageTarget'unknown' tag))
        )
instance (Classes.Marshal MessageTarget) where
    marshalInto raw_ value_ = case value_ of
        (MessageTarget'importedCap arg_) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_MessageTarget'importedCap raw_ arg_)
        (MessageTarget'promisedAnswer arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_MessageTarget'promisedAnswer raw_))
        (MessageTarget'unknown' tag) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_MessageTarget'unknown' raw_ tag)
instance (Classes.Cerialize MessageTarget)
instance (Classes.Cerialize (V.Vector MessageTarget)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector MessageTarget))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector MessageTarget)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector MessageTarget))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector MessageTarget)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector MessageTarget))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector MessageTarget)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Payload 
    = Payload 
        {content :: (Std_.Maybe UntypedPure.Ptr)
        ,capTable :: (V.Vector CapDescriptor)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Payload) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Payload) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Payload) where
    type Cerial msg Payload = (Capnp.Gen.ById.Xb312981b2552a250.Payload msg)
    decerialize raw = (Payload <$> ((Capnp.Gen.ById.Xb312981b2552a250.get_Payload'content raw) >>= Classes.decerialize)
                               <*> ((Capnp.Gen.ById.Xb312981b2552a250.get_Payload'capTable raw) >>= Classes.decerialize))
instance (Classes.Marshal Payload) where
    marshalInto raw_ value_ = case value_ of
        Payload{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) content) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Payload'content raw_))
                ((Classes.cerialize (Untyped.message raw_) capTable) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Payload'capTable raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Payload)
instance (Classes.Cerialize (V.Vector Payload)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Payload))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Payload)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Payload))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Payload)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Payload))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Payload)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data CapDescriptor 
    = CapDescriptor'none 
    | CapDescriptor'senderHosted Std_.Word32
    | CapDescriptor'senderPromise Std_.Word32
    | CapDescriptor'receiverHosted Std_.Word32
    | CapDescriptor'receiverAnswer PromisedAnswer
    | CapDescriptor'thirdPartyHosted ThirdPartyCapDescriptor
    | CapDescriptor'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default CapDescriptor) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg CapDescriptor) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize CapDescriptor) where
    type Cerial msg CapDescriptor = (Capnp.Gen.ById.Xb312981b2552a250.CapDescriptor msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xb312981b2552a250.get_CapDescriptor' raw)
        case raw of
            (Capnp.Gen.ById.Xb312981b2552a250.CapDescriptor'none) ->
                (Std_.pure CapDescriptor'none)
            (Capnp.Gen.ById.Xb312981b2552a250.CapDescriptor'senderHosted raw) ->
                (Std_.pure (CapDescriptor'senderHosted raw))
            (Capnp.Gen.ById.Xb312981b2552a250.CapDescriptor'senderPromise raw) ->
                (Std_.pure (CapDescriptor'senderPromise raw))
            (Capnp.Gen.ById.Xb312981b2552a250.CapDescriptor'receiverHosted raw) ->
                (Std_.pure (CapDescriptor'receiverHosted raw))
            (Capnp.Gen.ById.Xb312981b2552a250.CapDescriptor'receiverAnswer raw) ->
                (CapDescriptor'receiverAnswer <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.CapDescriptor'thirdPartyHosted raw) ->
                (CapDescriptor'thirdPartyHosted <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xb312981b2552a250.CapDescriptor'unknown' tag) ->
                (Std_.pure (CapDescriptor'unknown' tag))
        )
instance (Classes.Marshal CapDescriptor) where
    marshalInto raw_ value_ = case value_ of
        (CapDescriptor'none) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_CapDescriptor'none raw_)
        (CapDescriptor'senderHosted arg_) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_CapDescriptor'senderHosted raw_ arg_)
        (CapDescriptor'senderPromise arg_) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_CapDescriptor'senderPromise raw_ arg_)
        (CapDescriptor'receiverHosted arg_) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_CapDescriptor'receiverHosted raw_ arg_)
        (CapDescriptor'receiverAnswer arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_CapDescriptor'receiverAnswer raw_))
        (CapDescriptor'thirdPartyHosted arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_CapDescriptor'thirdPartyHosted raw_))
        (CapDescriptor'unknown' tag) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_CapDescriptor'unknown' raw_ tag)
instance (Classes.Cerialize CapDescriptor)
instance (Classes.Cerialize (V.Vector CapDescriptor)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector CapDescriptor))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector CapDescriptor)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector CapDescriptor))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CapDescriptor)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CapDescriptor))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CapDescriptor)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data PromisedAnswer 
    = PromisedAnswer 
        {questionId :: Std_.Word32
        ,transform :: (V.Vector PromisedAnswer'Op)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default PromisedAnswer) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg PromisedAnswer) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize PromisedAnswer) where
    type Cerial msg PromisedAnswer = (Capnp.Gen.ById.Xb312981b2552a250.PromisedAnswer msg)
    decerialize raw = (PromisedAnswer <$> (Capnp.Gen.ById.Xb312981b2552a250.get_PromisedAnswer'questionId raw)
                                      <*> ((Capnp.Gen.ById.Xb312981b2552a250.get_PromisedAnswer'transform raw) >>= Classes.decerialize))
instance (Classes.Marshal PromisedAnswer) where
    marshalInto raw_ value_ = case value_ of
        PromisedAnswer{..} ->
            (do
                (Capnp.Gen.ById.Xb312981b2552a250.set_PromisedAnswer'questionId raw_ questionId)
                ((Classes.cerialize (Untyped.message raw_) transform) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_PromisedAnswer'transform raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize PromisedAnswer)
instance (Classes.Cerialize (V.Vector PromisedAnswer)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector PromisedAnswer))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector PromisedAnswer)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector PromisedAnswer))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector PromisedAnswer)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector PromisedAnswer))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector PromisedAnswer)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data PromisedAnswer'Op 
    = PromisedAnswer'Op'noop 
    | PromisedAnswer'Op'getPointerField Std_.Word16
    | PromisedAnswer'Op'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default PromisedAnswer'Op) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg PromisedAnswer'Op) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize PromisedAnswer'Op) where
    type Cerial msg PromisedAnswer'Op = (Capnp.Gen.ById.Xb312981b2552a250.PromisedAnswer'Op msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xb312981b2552a250.get_PromisedAnswer'Op' raw)
        case raw of
            (Capnp.Gen.ById.Xb312981b2552a250.PromisedAnswer'Op'noop) ->
                (Std_.pure PromisedAnswer'Op'noop)
            (Capnp.Gen.ById.Xb312981b2552a250.PromisedAnswer'Op'getPointerField raw) ->
                (Std_.pure (PromisedAnswer'Op'getPointerField raw))
            (Capnp.Gen.ById.Xb312981b2552a250.PromisedAnswer'Op'unknown' tag) ->
                (Std_.pure (PromisedAnswer'Op'unknown' tag))
        )
instance (Classes.Marshal PromisedAnswer'Op) where
    marshalInto raw_ value_ = case value_ of
        (PromisedAnswer'Op'noop) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_PromisedAnswer'Op'noop raw_)
        (PromisedAnswer'Op'getPointerField arg_) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_PromisedAnswer'Op'getPointerField raw_ arg_)
        (PromisedAnswer'Op'unknown' tag) ->
            (Capnp.Gen.ById.Xb312981b2552a250.set_PromisedAnswer'Op'unknown' raw_ tag)
instance (Classes.Cerialize PromisedAnswer'Op)
instance (Classes.Cerialize (V.Vector PromisedAnswer'Op)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector PromisedAnswer'Op))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector PromisedAnswer'Op)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector PromisedAnswer'Op))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector PromisedAnswer'Op)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector PromisedAnswer'Op))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector PromisedAnswer'Op)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data ThirdPartyCapDescriptor 
    = ThirdPartyCapDescriptor 
        {id :: (Std_.Maybe UntypedPure.Ptr)
        ,vineId :: Std_.Word32}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default ThirdPartyCapDescriptor) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg ThirdPartyCapDescriptor) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize ThirdPartyCapDescriptor) where
    type Cerial msg ThirdPartyCapDescriptor = (Capnp.Gen.ById.Xb312981b2552a250.ThirdPartyCapDescriptor msg)
    decerialize raw = (ThirdPartyCapDescriptor <$> ((Capnp.Gen.ById.Xb312981b2552a250.get_ThirdPartyCapDescriptor'id raw) >>= Classes.decerialize)
                                               <*> (Capnp.Gen.ById.Xb312981b2552a250.get_ThirdPartyCapDescriptor'vineId raw))
instance (Classes.Marshal ThirdPartyCapDescriptor) where
    marshalInto raw_ value_ = case value_ of
        ThirdPartyCapDescriptor{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) id) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_ThirdPartyCapDescriptor'id raw_))
                (Capnp.Gen.ById.Xb312981b2552a250.set_ThirdPartyCapDescriptor'vineId raw_ vineId)
                (Std_.pure ())
                )
instance (Classes.Cerialize ThirdPartyCapDescriptor)
instance (Classes.Cerialize (V.Vector ThirdPartyCapDescriptor)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector ThirdPartyCapDescriptor))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector ThirdPartyCapDescriptor)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector ThirdPartyCapDescriptor))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ThirdPartyCapDescriptor)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ThirdPartyCapDescriptor))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector ThirdPartyCapDescriptor)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Exception 
    = Exception 
        {reason :: T.Text
        ,obsoleteIsCallersFault :: Std_.Bool
        ,obsoleteDurability :: Std_.Word16
        ,type_ :: Capnp.Gen.ById.Xb312981b2552a250.Exception'Type}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Exception) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Exception) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Exception) where
    type Cerial msg Exception = (Capnp.Gen.ById.Xb312981b2552a250.Exception msg)
    decerialize raw = (Exception <$> ((Capnp.Gen.ById.Xb312981b2552a250.get_Exception'reason raw) >>= Classes.decerialize)
                                 <*> (Capnp.Gen.ById.Xb312981b2552a250.get_Exception'obsoleteIsCallersFault raw)
                                 <*> (Capnp.Gen.ById.Xb312981b2552a250.get_Exception'obsoleteDurability raw)
                                 <*> (Capnp.Gen.ById.Xb312981b2552a250.get_Exception'type_ raw))
instance (Classes.Marshal Exception) where
    marshalInto raw_ value_ = case value_ of
        Exception{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) reason) >>= (Capnp.Gen.ById.Xb312981b2552a250.set_Exception'reason raw_))
                (Capnp.Gen.ById.Xb312981b2552a250.set_Exception'obsoleteIsCallersFault raw_ obsoleteIsCallersFault)
                (Capnp.Gen.ById.Xb312981b2552a250.set_Exception'obsoleteDurability raw_ obsoleteDurability)
                (Capnp.Gen.ById.Xb312981b2552a250.set_Exception'type_ raw_ type_)
                (Std_.pure ())
                )
instance (Classes.Cerialize Exception)
instance (Classes.Cerialize (V.Vector Exception)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Exception))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Exception)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Exception))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Exception)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Exception))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Exception)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Decerialize Capnp.Gen.ById.Xb312981b2552a250.Exception'Type) where
    type Cerial msg Capnp.Gen.ById.Xb312981b2552a250.Exception'Type = Capnp.Gen.ById.Xb312981b2552a250.Exception'Type
    decerialize  = Std_.pure
instance (Classes.Cerialize Capnp.Gen.ById.Xb312981b2552a250.Exception'Type) where
    cerialize _ = Std_.pure
instance (Classes.Cerialize (V.Vector Capnp.Gen.ById.Xb312981b2552a250.Exception'Type)) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector Capnp.Gen.ById.Xb312981b2552a250.Exception'Type))) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Capnp.Gen.ById.Xb312981b2552a250.Exception'Type)))) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Capnp.Gen.ById.Xb312981b2552a250.Exception'Type))))) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Capnp.Gen.ById.Xb312981b2552a250.Exception'Type)))))) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Capnp.Gen.ById.Xb312981b2552a250.Exception'Type))))))) where
    cerialize  = Classes.cerializeBasicVec