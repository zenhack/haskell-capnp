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
module Capnp.Gen.Capnp.Persistent.Pure(Persistent(..)
                                      ,Persistent'server_(..)
                                      ,export_Persistent
                                      ,Persistent'SaveParams(..)
                                      ,Persistent'SaveResults(..)
                                      ,RealmGateway(..)
                                      ,RealmGateway'server_(..)
                                      ,export_RealmGateway
                                      ,RealmGateway'import'params(..)
                                      ,RealmGateway'export'params(..)) where
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
import qualified Capnp.Rpc.Untyped as Rpc
import qualified Capnp.Rpc.Server as Server
import qualified Capnp.GenHelpers.Rpc as RpcHelpers
import qualified Capnp.GenHelpers.ReExports.Control.Concurrent.STM as STM
import qualified Capnp.GenHelpers.ReExports.Control.Monad.STM.Class as STM
import qualified Capnp.GenHelpers.ReExports.Supervisors as Supervisors
import qualified Capnp.Gen.ById.Xb8630836983feed7
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
newtype Persistent 
    = Persistent Message.Client
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
class ((MonadIO.MonadIO m)
      ,(Server.Server m cap)) => (Persistent'server_ m cap) where
    {-# MINIMAL persistent'save #-}
    persistent'save :: cap -> (Server.MethodHandler m Persistent'SaveParams Persistent'SaveResults)
    persistent'save _ = Server.methodUnimplemented
export_Persistent :: ((Persistent'server_ Std_.IO a)
                     ,(STM.MonadSTM m)) => Supervisors.Supervisor -> a -> (m Persistent)
export_Persistent sup_ server_ = (STM.liftSTM (Persistent <$> (Rpc.export sup_ Server.ServerOps{handleCast = (Server.unwrap server_)
                                                                                               ,handleStop = (Server.shutdown server_)
                                                                                               ,handleCall = (\interfaceId_ methodId_ -> case interfaceId_ of
                                                                                                   14468694717054801553 ->
                                                                                                       case methodId_ of
                                                                                                           0 ->
                                                                                                               (Server.toUntypedHandler (persistent'save server_))
                                                                                                           _ ->
                                                                                                               Server.methodUnimplemented
                                                                                                   _ ->
                                                                                                       Server.methodUnimplemented)})))
instance (Rpc.IsClient Persistent) where
    fromClient  = Persistent
    toClient (Persistent client) = client
instance (Classes.FromPtr msg Persistent) where
    fromPtr  = RpcHelpers.isClientFromPtr
instance (Classes.ToPtr s Persistent) where
    toPtr  = RpcHelpers.isClientToPtr
instance (Classes.Decerialize Persistent) where
    type Cerial msg Persistent = (Capnp.Gen.ById.Xb8630836983feed7.Persistent msg)
    decerialize (Capnp.Gen.ById.Xb8630836983feed7.Persistent'newtype_ maybeCap) = case maybeCap of
        (Std_.Nothing) ->
            (Std_.pure (Persistent Message.nullClient))
        (Std_.Just cap) ->
            (Persistent <$> (Untyped.getClient cap))
instance (Classes.Cerialize Persistent) where
    cerialize msg (Persistent client) = (Capnp.Gen.ById.Xb8630836983feed7.Persistent'newtype_ <$> (Std_.Just <$> (Untyped.appendCap msg client)))
instance (Server.Server Std_.IO Persistent)
instance (Persistent'server_ Std_.IO Persistent) where
    persistent'save (Persistent client) = (Rpc.clientMethodHandler 14468694717054801553 0 client)
data Persistent'SaveParams 
    = Persistent'SaveParams 
        {sealFor :: (Std_.Maybe UntypedPure.Ptr)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Persistent'SaveParams) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Persistent'SaveParams) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Persistent'SaveParams) where
    type Cerial msg Persistent'SaveParams = (Capnp.Gen.ById.Xb8630836983feed7.Persistent'SaveParams msg)
    decerialize raw = (Persistent'SaveParams <$> ((Capnp.Gen.ById.Xb8630836983feed7.get_Persistent'SaveParams'sealFor raw) >>= Classes.decerialize))
instance (Classes.Marshal Persistent'SaveParams) where
    marshalInto raw_ value_ = case value_ of
        Persistent'SaveParams{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) sealFor) >>= (Capnp.Gen.ById.Xb8630836983feed7.set_Persistent'SaveParams'sealFor raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Persistent'SaveParams)
instance (Classes.Cerialize (V.Vector Persistent'SaveParams)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Persistent'SaveParams))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Persistent'SaveParams)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Persistent'SaveParams))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Persistent'SaveParams)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Persistent'SaveParams))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Persistent'SaveParams)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Persistent'SaveResults 
    = Persistent'SaveResults 
        {sturdyRef :: (Std_.Maybe UntypedPure.Ptr)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Persistent'SaveResults) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Persistent'SaveResults) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Persistent'SaveResults) where
    type Cerial msg Persistent'SaveResults = (Capnp.Gen.ById.Xb8630836983feed7.Persistent'SaveResults msg)
    decerialize raw = (Persistent'SaveResults <$> ((Capnp.Gen.ById.Xb8630836983feed7.get_Persistent'SaveResults'sturdyRef raw) >>= Classes.decerialize))
instance (Classes.Marshal Persistent'SaveResults) where
    marshalInto raw_ value_ = case value_ of
        Persistent'SaveResults{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) sturdyRef) >>= (Capnp.Gen.ById.Xb8630836983feed7.set_Persistent'SaveResults'sturdyRef raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Persistent'SaveResults)
instance (Classes.Cerialize (V.Vector Persistent'SaveResults)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Persistent'SaveResults))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Persistent'SaveResults)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Persistent'SaveResults))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Persistent'SaveResults)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Persistent'SaveResults))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Persistent'SaveResults)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
newtype RealmGateway 
    = RealmGateway Message.Client
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
class ((MonadIO.MonadIO m)
      ,(Server.Server m cap)) => (RealmGateway'server_ m cap) where
    {-# MINIMAL realmGateway'import_,realmGateway'export #-}
    realmGateway'import_ :: cap -> (Server.MethodHandler m RealmGateway'import'params Persistent'SaveResults)
    realmGateway'import_ _ = Server.methodUnimplemented
    realmGateway'export :: cap -> (Server.MethodHandler m RealmGateway'export'params Persistent'SaveResults)
    realmGateway'export _ = Server.methodUnimplemented
export_RealmGateway :: ((RealmGateway'server_ Std_.IO a)
                       ,(STM.MonadSTM m)) => Supervisors.Supervisor -> a -> (m RealmGateway)
export_RealmGateway sup_ server_ = (STM.liftSTM (RealmGateway <$> (Rpc.export sup_ Server.ServerOps{handleCast = (Server.unwrap server_)
                                                                                                   ,handleStop = (Server.shutdown server_)
                                                                                                   ,handleCall = (\interfaceId_ methodId_ -> case interfaceId_ of
                                                                                                       9583422979879616212 ->
                                                                                                           case methodId_ of
                                                                                                               0 ->
                                                                                                                   (Server.toUntypedHandler (realmGateway'import_ server_))
                                                                                                               1 ->
                                                                                                                   (Server.toUntypedHandler (realmGateway'export server_))
                                                                                                               _ ->
                                                                                                                   Server.methodUnimplemented
                                                                                                       _ ->
                                                                                                           Server.methodUnimplemented)})))
instance (Rpc.IsClient RealmGateway) where
    fromClient  = RealmGateway
    toClient (RealmGateway client) = client
instance (Classes.FromPtr msg RealmGateway) where
    fromPtr  = RpcHelpers.isClientFromPtr
instance (Classes.ToPtr s RealmGateway) where
    toPtr  = RpcHelpers.isClientToPtr
instance (Classes.Decerialize RealmGateway) where
    type Cerial msg RealmGateway = (Capnp.Gen.ById.Xb8630836983feed7.RealmGateway msg)
    decerialize (Capnp.Gen.ById.Xb8630836983feed7.RealmGateway'newtype_ maybeCap) = case maybeCap of
        (Std_.Nothing) ->
            (Std_.pure (RealmGateway Message.nullClient))
        (Std_.Just cap) ->
            (RealmGateway <$> (Untyped.getClient cap))
instance (Classes.Cerialize RealmGateway) where
    cerialize msg (RealmGateway client) = (Capnp.Gen.ById.Xb8630836983feed7.RealmGateway'newtype_ <$> (Std_.Just <$> (Untyped.appendCap msg client)))
instance (Server.Server Std_.IO RealmGateway)
instance (RealmGateway'server_ Std_.IO RealmGateway) where
    realmGateway'import_ (RealmGateway client) = (Rpc.clientMethodHandler 9583422979879616212 0 client)
    realmGateway'export (RealmGateway client) = (Rpc.clientMethodHandler 9583422979879616212 1 client)
data RealmGateway'import'params 
    = RealmGateway'import'params 
        {cap :: Persistent
        ,params :: Persistent'SaveParams}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default RealmGateway'import'params) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg RealmGateway'import'params) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize RealmGateway'import'params) where
    type Cerial msg RealmGateway'import'params = (Capnp.Gen.ById.Xb8630836983feed7.RealmGateway'import'params msg)
    decerialize raw = (RealmGateway'import'params <$> ((Capnp.Gen.ById.Xb8630836983feed7.get_RealmGateway'import'params'cap raw) >>= Classes.decerialize)
                                                  <*> ((Capnp.Gen.ById.Xb8630836983feed7.get_RealmGateway'import'params'params raw) >>= Classes.decerialize))
instance (Classes.Marshal RealmGateway'import'params) where
    marshalInto raw_ value_ = case value_ of
        RealmGateway'import'params{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) cap) >>= (Capnp.Gen.ById.Xb8630836983feed7.set_RealmGateway'import'params'cap raw_))
                ((Classes.cerialize (Untyped.message raw_) params) >>= (Capnp.Gen.ById.Xb8630836983feed7.set_RealmGateway'import'params'params raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize RealmGateway'import'params)
instance (Classes.Cerialize (V.Vector RealmGateway'import'params)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector RealmGateway'import'params))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector RealmGateway'import'params)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector RealmGateway'import'params))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector RealmGateway'import'params)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector RealmGateway'import'params))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector RealmGateway'import'params)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data RealmGateway'export'params 
    = RealmGateway'export'params 
        {cap :: Persistent
        ,params :: Persistent'SaveParams}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default RealmGateway'export'params) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg RealmGateway'export'params) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize RealmGateway'export'params) where
    type Cerial msg RealmGateway'export'params = (Capnp.Gen.ById.Xb8630836983feed7.RealmGateway'export'params msg)
    decerialize raw = (RealmGateway'export'params <$> ((Capnp.Gen.ById.Xb8630836983feed7.get_RealmGateway'export'params'cap raw) >>= Classes.decerialize)
                                                  <*> ((Capnp.Gen.ById.Xb8630836983feed7.get_RealmGateway'export'params'params raw) >>= Classes.decerialize))
instance (Classes.Marshal RealmGateway'export'params) where
    marshalInto raw_ value_ = case value_ of
        RealmGateway'export'params{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) cap) >>= (Capnp.Gen.ById.Xb8630836983feed7.set_RealmGateway'export'params'cap raw_))
                ((Classes.cerialize (Untyped.message raw_) params) >>= (Capnp.Gen.ById.Xb8630836983feed7.set_RealmGateway'export'params'params raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize RealmGateway'export'params)
instance (Classes.Cerialize (V.Vector RealmGateway'export'params)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector RealmGateway'export'params))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector RealmGateway'export'params)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector RealmGateway'export'params))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector RealmGateway'export'params)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector RealmGateway'export'params))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector RealmGateway'export'params)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec