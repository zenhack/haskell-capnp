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
newtype Persistent sturdyRef owner
    = Persistent Message.Client
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
class ((MonadIO.MonadIO m)
      ,(Server.Server m cap)) => (Persistent'server_ m cap sturdyRef owner) | cap -> sturdyRef, cap -> owner where
    {-# MINIMAL persistent'save #-}
    persistent'save :: cap -> (Server.MethodHandler m (Persistent'SaveParams sturdyRef owner) (Persistent'SaveResults sturdyRef owner))
    persistent'save _ = Server.methodUnimplemented
export_Persistent :: ((STM.MonadSTM m)
                     ,(Persistent'server_ Std_.IO cap sturdyRef owner)) => Supervisors.Supervisor -> cap -> (m (Persistent sturdyRef owner))
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
instance (Rpc.IsClient (Persistent sturdyRef owner)) where
    fromClient  = Persistent
    toClient (Persistent client) = client
instance (Classes.FromPtr msg (Persistent sturdyRef owner)) where
    fromPtr  = RpcHelpers.isClientFromPtr
instance (Classes.ToPtr s (Persistent sturdyRef owner)) where
    toPtr  = RpcHelpers.isClientToPtr
instance (Classes.Decerialize (Persistent sturdyRef owner)) where
    type Cerial msg (Persistent sturdyRef owner) = (Capnp.Gen.ById.Xb8630836983feed7.Persistent (Classes.Cerial msg sturdyRef) (Classes.Cerial msg owner) msg)
    decerialize (Capnp.Gen.ById.Xb8630836983feed7.Persistent'newtype_ maybeCap) = case maybeCap of
        (Std_.Nothing) ->
            (Std_.pure (Persistent Message.nullClient))
        (Std_.Just cap) ->
            (Persistent <$> (Untyped.getClient cap))
instance (Classes.Cerialize s (Persistent sturdyRef owner)) where
    cerialize msg (Persistent client) = (Capnp.Gen.ById.Xb8630836983feed7.Persistent'newtype_ <$> (Std_.Just <$> (Untyped.appendCap msg client)))
instance (Server.Server Std_.IO (Persistent sturdyRef owner))
instance (Persistent'server_ Std_.IO (Persistent sturdyRef owner) sturdyRef owner) where
    persistent'save (Persistent client) = (Rpc.clientMethodHandler 14468694717054801553 0 client)
data Persistent'SaveParams sturdyRef owner
    = Persistent'SaveParams 
        {sealFor :: owner}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))) => (Default.Default (Persistent'SaveParams sturdyRef owner)) where
    def  = GenHelpersPure.defaultStruct
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))) => (Classes.FromStruct Message.Const (Persistent'SaveParams sturdyRef owner)) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))) => (Classes.Decerialize (Persistent'SaveParams sturdyRef owner)) where
    type Cerial msg (Persistent'SaveParams sturdyRef owner) = (Capnp.Gen.ById.Xb8630836983feed7.Persistent'SaveParams (Classes.Cerial msg sturdyRef) (Classes.Cerial msg owner) msg)
    decerialize raw = (Persistent'SaveParams <$> ((Capnp.Gen.ById.Xb8630836983feed7.get_Persistent'SaveParams'sealFor raw) >>= Classes.decerialize))
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Marshal s (Persistent'SaveParams sturdyRef owner)) where
    marshalInto raw_ value_ = case value_ of
        Persistent'SaveParams{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) sealFor) >>= (Capnp.Gen.ById.Xb8630836983feed7.set_Persistent'SaveParams'sealFor raw_))
                (Std_.pure ())
                )
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (Persistent'SaveParams sturdyRef owner))
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (V.Vector (Persistent'SaveParams sturdyRef owner))) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (V.Vector (V.Vector (Persistent'SaveParams sturdyRef owner)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (Persistent'SaveParams sturdyRef owner))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (Persistent'SaveParams sturdyRef owner)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (Persistent'SaveParams sturdyRef owner))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (Persistent'SaveParams sturdyRef owner)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (Persistent'SaveParams sturdyRef owner))))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Persistent'SaveResults sturdyRef owner
    = Persistent'SaveResults 
        {sturdyRef :: sturdyRef}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))) => (Default.Default (Persistent'SaveResults sturdyRef owner)) where
    def  = GenHelpersPure.defaultStruct
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))) => (Classes.FromStruct Message.Const (Persistent'SaveResults sturdyRef owner)) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))) => (Classes.Decerialize (Persistent'SaveResults sturdyRef owner)) where
    type Cerial msg (Persistent'SaveResults sturdyRef owner) = (Capnp.Gen.ById.Xb8630836983feed7.Persistent'SaveResults (Classes.Cerial msg sturdyRef) (Classes.Cerial msg owner) msg)
    decerialize raw = (Persistent'SaveResults <$> ((Capnp.Gen.ById.Xb8630836983feed7.get_Persistent'SaveResults'sturdyRef raw) >>= Classes.decerialize))
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Marshal s (Persistent'SaveResults sturdyRef owner)) where
    marshalInto raw_ value_ = case value_ of
        Persistent'SaveResults{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) sturdyRef) >>= (Capnp.Gen.ById.Xb8630836983feed7.set_Persistent'SaveResults'sturdyRef raw_))
                (Std_.pure ())
                )
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (Persistent'SaveResults sturdyRef owner))
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (V.Vector (Persistent'SaveResults sturdyRef owner))) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (V.Vector (V.Vector (Persistent'SaveResults sturdyRef owner)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (Persistent'SaveResults sturdyRef owner))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (Persistent'SaveResults sturdyRef owner)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (Persistent'SaveResults sturdyRef owner))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (Persistent'SaveResults sturdyRef owner)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize sturdyRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const sturdyRef))
         ,(Classes.Decerialize owner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const owner))
         ,(Classes.Cerialize s sturdyRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) sturdyRef))
         ,(Classes.Cerialize s owner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) owner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (Persistent'SaveResults sturdyRef owner))))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
newtype RealmGateway internalRef externalRef internalOwner externalOwner
    = RealmGateway Message.Client
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
class ((MonadIO.MonadIO m)
      ,(Server.Server m cap)) => (RealmGateway'server_ m cap internalRef externalRef internalOwner externalOwner) | cap -> internalRef, cap -> externalRef, cap -> internalOwner, cap -> externalOwner where
    {-# MINIMAL realmGateway'import_,realmGateway'export #-}
    realmGateway'import_ :: cap -> (Server.MethodHandler m (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveResults internalRef internalOwner))
    realmGateway'import_ _ = Server.methodUnimplemented
    realmGateway'export :: cap -> (Server.MethodHandler m (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveResults externalRef externalOwner))
    realmGateway'export _ = Server.methodUnimplemented
export_RealmGateway :: ((STM.MonadSTM m)
                       ,(RealmGateway'server_ Std_.IO cap internalRef externalRef internalOwner externalOwner)) => Supervisors.Supervisor -> cap -> (m (RealmGateway internalRef externalRef internalOwner externalOwner))
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
instance (Rpc.IsClient (RealmGateway internalRef externalRef internalOwner externalOwner)) where
    fromClient  = RealmGateway
    toClient (RealmGateway client) = client
instance (Classes.FromPtr msg (RealmGateway internalRef externalRef internalOwner externalOwner)) where
    fromPtr  = RpcHelpers.isClientFromPtr
instance (Classes.ToPtr s (RealmGateway internalRef externalRef internalOwner externalOwner)) where
    toPtr  = RpcHelpers.isClientToPtr
instance (Classes.Decerialize (RealmGateway internalRef externalRef internalOwner externalOwner)) where
    type Cerial msg (RealmGateway internalRef externalRef internalOwner externalOwner) = (Capnp.Gen.ById.Xb8630836983feed7.RealmGateway (Classes.Cerial msg internalRef) (Classes.Cerial msg externalRef) (Classes.Cerial msg internalOwner) (Classes.Cerial msg externalOwner) msg)
    decerialize (Capnp.Gen.ById.Xb8630836983feed7.RealmGateway'newtype_ maybeCap) = case maybeCap of
        (Std_.Nothing) ->
            (Std_.pure (RealmGateway Message.nullClient))
        (Std_.Just cap) ->
            (RealmGateway <$> (Untyped.getClient cap))
instance (Classes.Cerialize s (RealmGateway internalRef externalRef internalOwner externalOwner)) where
    cerialize msg (RealmGateway client) = (Capnp.Gen.ById.Xb8630836983feed7.RealmGateway'newtype_ <$> (Std_.Just <$> (Untyped.appendCap msg client)))
instance (Server.Server Std_.IO (RealmGateway internalRef externalRef internalOwner externalOwner))
instance (RealmGateway'server_ Std_.IO (RealmGateway internalRef externalRef internalOwner externalOwner) internalRef externalRef internalOwner externalOwner) where
    realmGateway'import_ (RealmGateway client) = (Rpc.clientMethodHandler 9583422979879616212 0 client)
    realmGateway'export (RealmGateway client) = (Rpc.clientMethodHandler 9583422979879616212 1 client)
data RealmGateway'import'params internalRef externalRef internalOwner externalOwner
    = RealmGateway'import'params 
        {cap :: (Persistent externalRef externalOwner)
        ,params :: (Persistent'SaveParams internalRef internalOwner)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))) => (Default.Default (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)) where
    def  = GenHelpersPure.defaultStruct
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))) => (Classes.FromStruct Message.Const (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))) => (Classes.Decerialize (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)) where
    type Cerial msg (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) = (Capnp.Gen.ById.Xb8630836983feed7.RealmGateway'import'params (Classes.Cerial msg internalRef) (Classes.Cerial msg externalRef) (Classes.Cerial msg internalOwner) (Classes.Cerial msg externalOwner) msg)
    decerialize raw = (RealmGateway'import'params <$> ((Capnp.Gen.ById.Xb8630836983feed7.get_RealmGateway'import'params'cap raw) >>= Classes.decerialize)
                                                  <*> ((Capnp.Gen.ById.Xb8630836983feed7.get_RealmGateway'import'params'params raw) >>= Classes.decerialize))
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Marshal s (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)) where
    marshalInto raw_ value_ = case value_ of
        RealmGateway'import'params{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) cap) >>= (Capnp.Gen.ById.Xb8630836983feed7.set_RealmGateway'import'params'cap raw_))
                ((Classes.cerialize (Untyped.message raw_) params) >>= (Capnp.Gen.ById.Xb8630836983feed7.set_RealmGateway'import'params'params raw_))
                (Std_.pure ())
                )
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (RealmGateway'import'params internalRef externalRef internalOwner externalOwner))
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (V.Vector (RealmGateway'import'params internalRef externalRef internalOwner externalOwner))) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (V.Vector (V.Vector (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (RealmGateway'import'params internalRef externalRef internalOwner externalOwner))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (RealmGateway'import'params internalRef externalRef internalOwner externalOwner))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (RealmGateway'import'params internalRef externalRef internalOwner externalOwner))))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data RealmGateway'export'params internalRef externalRef internalOwner externalOwner
    = RealmGateway'export'params 
        {cap :: (Persistent internalRef internalOwner)
        ,params :: (Persistent'SaveParams externalRef externalOwner)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))) => (Default.Default (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)) where
    def  = GenHelpersPure.defaultStruct
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))) => (Classes.FromStruct Message.Const (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))) => (Classes.Decerialize (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)) where
    type Cerial msg (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) = (Capnp.Gen.ById.Xb8630836983feed7.RealmGateway'export'params (Classes.Cerial msg internalRef) (Classes.Cerial msg externalRef) (Classes.Cerial msg internalOwner) (Classes.Cerial msg externalOwner) msg)
    decerialize raw = (RealmGateway'export'params <$> ((Capnp.Gen.ById.Xb8630836983feed7.get_RealmGateway'export'params'cap raw) >>= Classes.decerialize)
                                                  <*> ((Capnp.Gen.ById.Xb8630836983feed7.get_RealmGateway'export'params'params raw) >>= Classes.decerialize))
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Marshal s (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)) where
    marshalInto raw_ value_ = case value_ of
        RealmGateway'export'params{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) cap) >>= (Capnp.Gen.ById.Xb8630836983feed7.set_RealmGateway'export'params'cap raw_))
                ((Classes.cerialize (Untyped.message raw_) params) >>= (Capnp.Gen.ById.Xb8630836983feed7.set_RealmGateway'export'params'params raw_))
                (Std_.pure ())
                )
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (RealmGateway'export'params internalRef externalRef internalOwner externalOwner))
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (V.Vector (RealmGateway'export'params internalRef externalRef internalOwner externalOwner))) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (V.Vector (V.Vector (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (RealmGateway'export'params internalRef externalRef internalOwner externalOwner))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (RealmGateway'export'params internalRef externalRef internalOwner externalOwner))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance ((Classes.Decerialize internalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalRef))
         ,(Classes.Decerialize externalRef)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalRef))
         ,(Classes.Decerialize internalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const internalOwner))
         ,(Classes.Decerialize externalOwner)
         ,(Classes.FromPtr Message.Const (Classes.Cerial Message.Const externalOwner))
         ,(Classes.Cerialize s internalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalRef))
         ,(Classes.Cerialize s externalRef)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalRef))
         ,(Classes.Cerialize s internalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) internalOwner))
         ,(Classes.Cerialize s externalOwner)
         ,(Classes.ToPtr s (Classes.Cerial (Message.Mut s) externalOwner))) => (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (RealmGateway'export'params internalRef externalRef internalOwner externalOwner))))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec