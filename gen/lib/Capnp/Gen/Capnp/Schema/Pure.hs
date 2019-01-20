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
module Capnp.Gen.Capnp.Schema.Pure(Capnp.Gen.ById.Xa93fc509624c72d9.ElementSize(..)
                                  ,Node(..)
                                  ,Node'(..)
                                  ,Node'Parameter(..)
                                  ,Node'NestedNode(..)
                                  ,Node'SourceInfo(..)
                                  ,Node'SourceInfo'Member(..)
                                  ,Field(..)
                                  ,Field'(..)
                                  ,Field'ordinal(..)
                                  ,Capnp.Gen.ById.Xa93fc509624c72d9.field'noDiscriminant
                                  ,Enumerant(..)
                                  ,Superclass(..)
                                  ,Method(..)
                                  ,Type(..)
                                  ,Type'anyPointer(..)
                                  ,Type'anyPointer'unconstrained(..)
                                  ,Brand(..)
                                  ,Brand'Scope(..)
                                  ,Brand'Scope'(..)
                                  ,Brand'Binding(..)
                                  ,Value(..)
                                  ,Annotation(..)
                                  ,CapnpVersion(..)
                                  ,CodeGeneratorRequest(..)
                                  ,CodeGeneratorRequest'RequestedFile(..)
                                  ,CodeGeneratorRequest'RequestedFile'Import(..)) where
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
import qualified Capnp.Gen.ById.Xa93fc509624c72d9
import qualified Capnp.Gen.ById.Xbdf87d7bb8304e81
import qualified Capnp.Gen.ById.Xbdf87d7bb8304e81.Pure
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Node 
    = Node 
        {id :: Std_.Word64
        ,displayName :: T.Text
        ,displayNamePrefixLength :: Std_.Word32
        ,scopeId :: Std_.Word64
        ,nestedNodes :: (V.Vector Node'NestedNode)
        ,annotations :: (V.Vector Annotation)
        ,parameters :: (V.Vector Node'Parameter)
        ,isGeneric :: Std_.Bool
        ,union' :: Node'}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Node) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Node) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Node) where
    type Cerial msg Node = (Capnp.Gen.ById.Xa93fc509624c72d9.Node msg)
    decerialize raw = (Node <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'id raw)
                            <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'displayName raw) >>= Classes.decerialize)
                            <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'displayNamePrefixLength raw)
                            <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'scopeId raw)
                            <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'nestedNodes raw) >>= Classes.decerialize)
                            <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'annotations raw) >>= Classes.decerialize)
                            <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'parameters raw) >>= Classes.decerialize)
                            <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'isGeneric raw)
                            <*> (Classes.decerialize raw))
instance (Classes.Marshal Node) where
    marshalInto raw_ value_ = case value_ of
        Node{..} ->
            (do
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'id raw_ id)
                ((Classes.cerialize (Untyped.message raw_) displayName) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'displayName raw_))
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'displayNamePrefixLength raw_ displayNamePrefixLength)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'scopeId raw_ scopeId)
                ((Classes.cerialize (Untyped.message raw_) nestedNodes) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'nestedNodes raw_))
                ((Classes.cerialize (Untyped.message raw_) annotations) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotations raw_))
                ((Classes.cerialize (Untyped.message raw_) parameters) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'parameters raw_))
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'isGeneric raw_ isGeneric)
                (do
                    (Classes.marshalInto raw_ union')
                    )
                (Std_.pure ())
                )
instance (Classes.Cerialize Node)
instance (Classes.Cerialize (V.Vector Node)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Node))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Node)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Node))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Node' 
    = Node'file 
    | Node'struct 
        {dataWordCount :: Std_.Word16
        ,pointerCount :: Std_.Word16
        ,preferredListEncoding :: Capnp.Gen.ById.Xa93fc509624c72d9.ElementSize
        ,isGroup :: Std_.Bool
        ,discriminantCount :: Std_.Word16
        ,discriminantOffset :: Std_.Word32
        ,fields :: (V.Vector Field)}
    | Node'enum 
        {enumerants :: (V.Vector Enumerant)}
    | Node'interface 
        {methods :: (V.Vector Method)
        ,superclasses :: (V.Vector Superclass)}
    | Node'const 
        {type_ :: Type
        ,value :: Value}
    | Node'annotation 
        {type_ :: Type
        ,targetsFile :: Std_.Bool
        ,targetsConst :: Std_.Bool
        ,targetsEnum :: Std_.Bool
        ,targetsEnumerant :: Std_.Bool
        ,targetsStruct :: Std_.Bool
        ,targetsField :: Std_.Bool
        ,targetsUnion :: Std_.Bool
        ,targetsGroup :: Std_.Bool
        ,targetsInterface :: Std_.Bool
        ,targetsMethod :: Std_.Bool
        ,targetsParam :: Std_.Bool
        ,targetsAnnotation :: Std_.Bool}
    | Node'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Node') where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Node') where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Node') where
    type Cerial msg Node' = (Capnp.Gen.ById.Xa93fc509624c72d9.Node msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node' raw)
        case raw of
            (Capnp.Gen.ById.Xa93fc509624c72d9.Node'file) ->
                (Std_.pure Node'file)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Node'struct raw) ->
                (Node'struct <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'struct'dataWordCount raw)
                             <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'struct'pointerCount raw)
                             <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'struct'preferredListEncoding raw)
                             <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'struct'isGroup raw)
                             <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'struct'discriminantCount raw)
                             <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'struct'discriminantOffset raw)
                             <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'struct'fields raw) >>= Classes.decerialize))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Node'enum raw) ->
                (Node'enum <$> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'enum'enumerants raw) >>= Classes.decerialize))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Node'interface raw) ->
                (Node'interface <$> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'interface'methods raw) >>= Classes.decerialize)
                                <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'interface'superclasses raw) >>= Classes.decerialize))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Node'const raw) ->
                (Node'const <$> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'const'type_ raw) >>= Classes.decerialize)
                            <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'const'value raw) >>= Classes.decerialize))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Node'annotation raw) ->
                (Node'annotation <$> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'annotation'type_ raw) >>= Classes.decerialize)
                                 <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'annotation'targetsFile raw)
                                 <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'annotation'targetsConst raw)
                                 <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'annotation'targetsEnum raw)
                                 <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'annotation'targetsEnumerant raw)
                                 <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'annotation'targetsStruct raw)
                                 <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'annotation'targetsField raw)
                                 <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'annotation'targetsUnion raw)
                                 <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'annotation'targetsGroup raw)
                                 <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'annotation'targetsInterface raw)
                                 <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'annotation'targetsMethod raw)
                                 <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'annotation'targetsParam raw)
                                 <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'annotation'targetsAnnotation raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Node'unknown' tag) ->
                (Std_.pure (Node'unknown' tag))
        )
instance (Classes.Marshal Node') where
    marshalInto raw_ value_ = case value_ of
        (Node'file) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'file raw_)
        Node'struct{..} ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'struct raw_)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'struct'dataWordCount raw_ dataWordCount)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'struct'pointerCount raw_ pointerCount)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'struct'preferredListEncoding raw_ preferredListEncoding)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'struct'isGroup raw_ isGroup)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'struct'discriminantCount raw_ discriminantCount)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'struct'discriminantOffset raw_ discriminantOffset)
                ((Classes.cerialize (Untyped.message raw_) fields) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'struct'fields raw_))
                (Std_.pure ())
                )
        Node'enum{..} ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'enum raw_)
                ((Classes.cerialize (Untyped.message raw_) enumerants) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'enum'enumerants raw_))
                (Std_.pure ())
                )
        Node'interface{..} ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'interface raw_)
                ((Classes.cerialize (Untyped.message raw_) methods) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'interface'methods raw_))
                ((Classes.cerialize (Untyped.message raw_) superclasses) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'interface'superclasses raw_))
                (Std_.pure ())
                )
        Node'const{..} ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'const raw_)
                ((Classes.cerialize (Untyped.message raw_) type_) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'const'type_ raw_))
                ((Classes.cerialize (Untyped.message raw_) value) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'const'value raw_))
                (Std_.pure ())
                )
        Node'annotation{..} ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotation raw_)
                ((Classes.cerialize (Untyped.message raw_) type_) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotation'type_ raw_))
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotation'targetsFile raw_ targetsFile)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotation'targetsConst raw_ targetsConst)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotation'targetsEnum raw_ targetsEnum)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotation'targetsEnumerant raw_ targetsEnumerant)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotation'targetsStruct raw_ targetsStruct)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotation'targetsField raw_ targetsField)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotation'targetsUnion raw_ targetsUnion)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotation'targetsGroup raw_ targetsGroup)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotation'targetsInterface raw_ targetsInterface)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotation'targetsMethod raw_ targetsMethod)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotation'targetsParam raw_ targetsParam)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'annotation'targetsAnnotation raw_ targetsAnnotation)
                (Std_.pure ())
                )
        (Node'unknown' tag) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'unknown' raw_ tag)
data Node'Parameter 
    = Node'Parameter 
        {name :: T.Text}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Node'Parameter) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Node'Parameter) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Node'Parameter) where
    type Cerial msg Node'Parameter = (Capnp.Gen.ById.Xa93fc509624c72d9.Node'Parameter msg)
    decerialize raw = (Node'Parameter <$> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'Parameter'name raw) >>= Classes.decerialize))
instance (Classes.Marshal Node'Parameter) where
    marshalInto raw_ value_ = case value_ of
        Node'Parameter{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) name) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'Parameter'name raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Node'Parameter)
instance (Classes.Cerialize (V.Vector Node'Parameter)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Node'Parameter))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Node'Parameter)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Node'Parameter))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node'Parameter)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node'Parameter))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node'Parameter)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Node'NestedNode 
    = Node'NestedNode 
        {name :: T.Text
        ,id :: Std_.Word64}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Node'NestedNode) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Node'NestedNode) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Node'NestedNode) where
    type Cerial msg Node'NestedNode = (Capnp.Gen.ById.Xa93fc509624c72d9.Node'NestedNode msg)
    decerialize raw = (Node'NestedNode <$> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'NestedNode'name raw) >>= Classes.decerialize)
                                       <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'NestedNode'id raw))
instance (Classes.Marshal Node'NestedNode) where
    marshalInto raw_ value_ = case value_ of
        Node'NestedNode{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) name) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'NestedNode'name raw_))
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'NestedNode'id raw_ id)
                (Std_.pure ())
                )
instance (Classes.Cerialize Node'NestedNode)
instance (Classes.Cerialize (V.Vector Node'NestedNode)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Node'NestedNode))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Node'NestedNode)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Node'NestedNode))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node'NestedNode)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node'NestedNode))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node'NestedNode)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Node'SourceInfo 
    = Node'SourceInfo 
        {id :: Std_.Word64
        ,docComment :: T.Text
        ,members :: (V.Vector Node'SourceInfo'Member)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Node'SourceInfo) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Node'SourceInfo) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Node'SourceInfo) where
    type Cerial msg Node'SourceInfo = (Capnp.Gen.ById.Xa93fc509624c72d9.Node'SourceInfo msg)
    decerialize raw = (Node'SourceInfo <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'SourceInfo'id raw)
                                       <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'SourceInfo'docComment raw) >>= Classes.decerialize)
                                       <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'SourceInfo'members raw) >>= Classes.decerialize))
instance (Classes.Marshal Node'SourceInfo) where
    marshalInto raw_ value_ = case value_ of
        Node'SourceInfo{..} ->
            (do
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'SourceInfo'id raw_ id)
                ((Classes.cerialize (Untyped.message raw_) docComment) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'SourceInfo'docComment raw_))
                ((Classes.cerialize (Untyped.message raw_) members) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'SourceInfo'members raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Node'SourceInfo)
instance (Classes.Cerialize (V.Vector Node'SourceInfo)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Node'SourceInfo))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Node'SourceInfo)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Node'SourceInfo))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node'SourceInfo)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node'SourceInfo))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node'SourceInfo)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Node'SourceInfo'Member 
    = Node'SourceInfo'Member 
        {docComment :: T.Text}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Node'SourceInfo'Member) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Node'SourceInfo'Member) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Node'SourceInfo'Member) where
    type Cerial msg Node'SourceInfo'Member = (Capnp.Gen.ById.Xa93fc509624c72d9.Node'SourceInfo'Member msg)
    decerialize raw = (Node'SourceInfo'Member <$> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Node'SourceInfo'Member'docComment raw) >>= Classes.decerialize))
instance (Classes.Marshal Node'SourceInfo'Member) where
    marshalInto raw_ value_ = case value_ of
        Node'SourceInfo'Member{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) docComment) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Node'SourceInfo'Member'docComment raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Node'SourceInfo'Member)
instance (Classes.Cerialize (V.Vector Node'SourceInfo'Member)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Node'SourceInfo'Member))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Node'SourceInfo'Member)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Node'SourceInfo'Member))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node'SourceInfo'Member)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node'SourceInfo'Member))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Node'SourceInfo'Member)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Field 
    = Field 
        {name :: T.Text
        ,codeOrder :: Std_.Word16
        ,annotations :: (V.Vector Annotation)
        ,discriminantValue :: Std_.Word16
        ,ordinal :: Field'ordinal
        ,union' :: Field'}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Field) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Field) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Field) where
    type Cerial msg Field = (Capnp.Gen.ById.Xa93fc509624c72d9.Field msg)
    decerialize raw = (Field <$> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Field'name raw) >>= Classes.decerialize)
                             <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Field'codeOrder raw)
                             <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Field'annotations raw) >>= Classes.decerialize)
                             <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Field'discriminantValue raw)
                             <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Field'ordinal raw) >>= Classes.decerialize)
                             <*> (Classes.decerialize raw))
instance (Classes.Marshal Field) where
    marshalInto raw_ value_ = case value_ of
        Field{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) name) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'name raw_))
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'codeOrder raw_ codeOrder)
                ((Classes.cerialize (Untyped.message raw_) annotations) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'annotations raw_))
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'discriminantValue raw_ discriminantValue)
                (do
                    raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.get_Field'ordinal raw_)
                    (Classes.marshalInto raw_ ordinal)
                    )
                (do
                    (Classes.marshalInto raw_ union')
                    )
                (Std_.pure ())
                )
instance (Classes.Cerialize Field)
instance (Classes.Cerialize (V.Vector Field)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Field))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Field)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Field))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Field)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Field))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Field)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Field' 
    = Field'slot 
        {offset :: Std_.Word32
        ,type_ :: Type
        ,defaultValue :: Value
        ,hadExplicitDefault :: Std_.Bool}
    | Field'group 
        {typeId :: Std_.Word64}
    | Field'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Field') where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Field') where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Field') where
    type Cerial msg Field' = (Capnp.Gen.ById.Xa93fc509624c72d9.Field msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xa93fc509624c72d9.get_Field' raw)
        case raw of
            (Capnp.Gen.ById.Xa93fc509624c72d9.Field'slot raw) ->
                (Field'slot <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Field'slot'offset raw)
                            <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Field'slot'type_ raw) >>= Classes.decerialize)
                            <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Field'slot'defaultValue raw) >>= Classes.decerialize)
                            <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Field'slot'hadExplicitDefault raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Field'group raw) ->
                (Field'group <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Field'group'typeId raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Field'unknown' tag) ->
                (Std_.pure (Field'unknown' tag))
        )
instance (Classes.Marshal Field') where
    marshalInto raw_ value_ = case value_ of
        Field'slot{..} ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'slot raw_)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'slot'offset raw_ offset)
                ((Classes.cerialize (Untyped.message raw_) type_) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'slot'type_ raw_))
                ((Classes.cerialize (Untyped.message raw_) defaultValue) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'slot'defaultValue raw_))
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'slot'hadExplicitDefault raw_ hadExplicitDefault)
                (Std_.pure ())
                )
        Field'group{..} ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'group raw_)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'group'typeId raw_ typeId)
                (Std_.pure ())
                )
        (Field'unknown' tag) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'unknown' raw_ tag)
data Field'ordinal 
    = Field'ordinal'implicit 
    | Field'ordinal'explicit Std_.Word16
    | Field'ordinal'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Field'ordinal) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Field'ordinal) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Field'ordinal) where
    type Cerial msg Field'ordinal = (Capnp.Gen.ById.Xa93fc509624c72d9.Field'ordinal msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xa93fc509624c72d9.get_Field'ordinal' raw)
        case raw of
            (Capnp.Gen.ById.Xa93fc509624c72d9.Field'ordinal'implicit) ->
                (Std_.pure Field'ordinal'implicit)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Field'ordinal'explicit raw) ->
                (Std_.pure (Field'ordinal'explicit raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Field'ordinal'unknown' tag) ->
                (Std_.pure (Field'ordinal'unknown' tag))
        )
instance (Classes.Marshal Field'ordinal) where
    marshalInto raw_ value_ = case value_ of
        (Field'ordinal'implicit) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'ordinal'implicit raw_)
        (Field'ordinal'explicit arg_) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'ordinal'explicit raw_ arg_)
        (Field'ordinal'unknown' tag) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Field'ordinal'unknown' raw_ tag)
data Enumerant 
    = Enumerant 
        {name :: T.Text
        ,codeOrder :: Std_.Word16
        ,annotations :: (V.Vector Annotation)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Enumerant) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Enumerant) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Enumerant) where
    type Cerial msg Enumerant = (Capnp.Gen.ById.Xa93fc509624c72d9.Enumerant msg)
    decerialize raw = (Enumerant <$> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Enumerant'name raw) >>= Classes.decerialize)
                                 <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Enumerant'codeOrder raw)
                                 <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Enumerant'annotations raw) >>= Classes.decerialize))
instance (Classes.Marshal Enumerant) where
    marshalInto raw_ value_ = case value_ of
        Enumerant{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) name) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Enumerant'name raw_))
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Enumerant'codeOrder raw_ codeOrder)
                ((Classes.cerialize (Untyped.message raw_) annotations) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Enumerant'annotations raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Enumerant)
instance (Classes.Cerialize (V.Vector Enumerant)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Enumerant))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Enumerant)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Enumerant))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Enumerant)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Enumerant))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Enumerant)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Superclass 
    = Superclass 
        {id :: Std_.Word64
        ,brand :: Brand}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Superclass) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Superclass) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Superclass) where
    type Cerial msg Superclass = (Capnp.Gen.ById.Xa93fc509624c72d9.Superclass msg)
    decerialize raw = (Superclass <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Superclass'id raw)
                                  <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Superclass'brand raw) >>= Classes.decerialize))
instance (Classes.Marshal Superclass) where
    marshalInto raw_ value_ = case value_ of
        Superclass{..} ->
            (do
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Superclass'id raw_ id)
                ((Classes.cerialize (Untyped.message raw_) brand) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Superclass'brand raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Superclass)
instance (Classes.Cerialize (V.Vector Superclass)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Superclass))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Superclass)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Superclass))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Superclass)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Superclass))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Superclass)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Method 
    = Method 
        {name :: T.Text
        ,codeOrder :: Std_.Word16
        ,paramStructType :: Std_.Word64
        ,resultStructType :: Std_.Word64
        ,annotations :: (V.Vector Annotation)
        ,paramBrand :: Brand
        ,resultBrand :: Brand
        ,implicitParameters :: (V.Vector Node'Parameter)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Method) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Method) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Method) where
    type Cerial msg Method = (Capnp.Gen.ById.Xa93fc509624c72d9.Method msg)
    decerialize raw = (Method <$> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Method'name raw) >>= Classes.decerialize)
                              <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Method'codeOrder raw)
                              <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Method'paramStructType raw)
                              <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Method'resultStructType raw)
                              <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Method'annotations raw) >>= Classes.decerialize)
                              <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Method'paramBrand raw) >>= Classes.decerialize)
                              <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Method'resultBrand raw) >>= Classes.decerialize)
                              <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Method'implicitParameters raw) >>= Classes.decerialize))
instance (Classes.Marshal Method) where
    marshalInto raw_ value_ = case value_ of
        Method{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) name) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Method'name raw_))
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Method'codeOrder raw_ codeOrder)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Method'paramStructType raw_ paramStructType)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Method'resultStructType raw_ resultStructType)
                ((Classes.cerialize (Untyped.message raw_) annotations) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Method'annotations raw_))
                ((Classes.cerialize (Untyped.message raw_) paramBrand) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Method'paramBrand raw_))
                ((Classes.cerialize (Untyped.message raw_) resultBrand) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Method'resultBrand raw_))
                ((Classes.cerialize (Untyped.message raw_) implicitParameters) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Method'implicitParameters raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Method)
instance (Classes.Cerialize (V.Vector Method)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Method))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Method)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Method))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Method)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Method))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Method)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Type 
    = Type'void 
    | Type'bool 
    | Type'int8 
    | Type'int16 
    | Type'int32 
    | Type'int64 
    | Type'uint8 
    | Type'uint16 
    | Type'uint32 
    | Type'uint64 
    | Type'float32 
    | Type'float64 
    | Type'text 
    | Type'data_ 
    | Type'list 
        {elementType :: Type}
    | Type'enum 
        {typeId :: Std_.Word64
        ,brand :: Brand}
    | Type'struct 
        {typeId :: Std_.Word64
        ,brand :: Brand}
    | Type'interface 
        {typeId :: Std_.Word64
        ,brand :: Brand}
    | Type'anyPointer Type'anyPointer
    | Type'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Type) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Type) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Type) where
    type Cerial msg Type = (Capnp.Gen.ById.Xa93fc509624c72d9.Type msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xa93fc509624c72d9.get_Type' raw)
        case raw of
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'void) ->
                (Std_.pure Type'void)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'bool) ->
                (Std_.pure Type'bool)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'int8) ->
                (Std_.pure Type'int8)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'int16) ->
                (Std_.pure Type'int16)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'int32) ->
                (Std_.pure Type'int32)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'int64) ->
                (Std_.pure Type'int64)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'uint8) ->
                (Std_.pure Type'uint8)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'uint16) ->
                (Std_.pure Type'uint16)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'uint32) ->
                (Std_.pure Type'uint32)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'uint64) ->
                (Std_.pure Type'uint64)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'float32) ->
                (Std_.pure Type'float32)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'float64) ->
                (Std_.pure Type'float64)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'text) ->
                (Std_.pure Type'text)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'data_) ->
                (Std_.pure Type'data_)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'list raw) ->
                (Type'list <$> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Type'list'elementType raw) >>= Classes.decerialize))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'enum raw) ->
                (Type'enum <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Type'enum'typeId raw)
                           <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Type'enum'brand raw) >>= Classes.decerialize))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'struct raw) ->
                (Type'struct <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Type'struct'typeId raw)
                             <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Type'struct'brand raw) >>= Classes.decerialize))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'interface raw) ->
                (Type'interface <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Type'interface'typeId raw)
                                <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Type'interface'brand raw) >>= Classes.decerialize))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'anyPointer raw) ->
                (Type'anyPointer <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'unknown' tag) ->
                (Std_.pure (Type'unknown' tag))
        )
instance (Classes.Marshal Type) where
    marshalInto raw_ value_ = case value_ of
        (Type'void) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'void raw_)
        (Type'bool) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'bool raw_)
        (Type'int8) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'int8 raw_)
        (Type'int16) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'int16 raw_)
        (Type'int32) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'int32 raw_)
        (Type'int64) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'int64 raw_)
        (Type'uint8) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'uint8 raw_)
        (Type'uint16) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'uint16 raw_)
        (Type'uint32) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'uint32 raw_)
        (Type'uint64) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'uint64 raw_)
        (Type'float32) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'float32 raw_)
        (Type'float64) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'float64 raw_)
        (Type'text) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'text raw_)
        (Type'data_) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'data_ raw_)
        Type'list{..} ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'list raw_)
                ((Classes.cerialize (Untyped.message raw_) elementType) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'list'elementType raw_))
                (Std_.pure ())
                )
        Type'enum{..} ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'enum raw_)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'enum'typeId raw_ typeId)
                ((Classes.cerialize (Untyped.message raw_) brand) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'enum'brand raw_))
                (Std_.pure ())
                )
        Type'struct{..} ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'struct raw_)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'struct'typeId raw_ typeId)
                ((Classes.cerialize (Untyped.message raw_) brand) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'struct'brand raw_))
                (Std_.pure ())
                )
        Type'interface{..} ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'interface raw_)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'interface'typeId raw_ typeId)
                ((Classes.cerialize (Untyped.message raw_) brand) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'interface'brand raw_))
                (Std_.pure ())
                )
        (Type'anyPointer arg_) ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'anyPointer raw_)
                (Classes.marshalInto raw_ arg_)
                )
        (Type'unknown' tag) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'unknown' raw_ tag)
instance (Classes.Cerialize Type)
instance (Classes.Cerialize (V.Vector Type)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Type))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Type)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Type))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Type)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Type))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Type)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Type'anyPointer 
    = Type'anyPointer'unconstrained Type'anyPointer'unconstrained
    | Type'anyPointer'parameter 
        {scopeId :: Std_.Word64
        ,parameterIndex :: Std_.Word16}
    | Type'anyPointer'implicitMethodParameter 
        {parameterIndex :: Std_.Word16}
    | Type'anyPointer'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Type'anyPointer) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Type'anyPointer) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Type'anyPointer) where
    type Cerial msg Type'anyPointer = (Capnp.Gen.ById.Xa93fc509624c72d9.Type'anyPointer msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xa93fc509624c72d9.get_Type'anyPointer' raw)
        case raw of
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'anyPointer'unconstrained raw) ->
                (Type'anyPointer'unconstrained <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'anyPointer'parameter raw) ->
                (Type'anyPointer'parameter <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Type'anyPointer'parameter'scopeId raw)
                                           <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Type'anyPointer'parameter'parameterIndex raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'anyPointer'implicitMethodParameter raw) ->
                (Type'anyPointer'implicitMethodParameter <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Type'anyPointer'implicitMethodParameter'parameterIndex raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'anyPointer'unknown' tag) ->
                (Std_.pure (Type'anyPointer'unknown' tag))
        )
instance (Classes.Marshal Type'anyPointer) where
    marshalInto raw_ value_ = case value_ of
        (Type'anyPointer'unconstrained arg_) ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'anyPointer'unconstrained raw_)
                (Classes.marshalInto raw_ arg_)
                )
        Type'anyPointer'parameter{..} ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'anyPointer'parameter raw_)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'anyPointer'parameter'scopeId raw_ scopeId)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'anyPointer'parameter'parameterIndex raw_ parameterIndex)
                (Std_.pure ())
                )
        Type'anyPointer'implicitMethodParameter{..} ->
            (do
                raw_ <- (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'anyPointer'implicitMethodParameter raw_)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'anyPointer'implicitMethodParameter'parameterIndex raw_ parameterIndex)
                (Std_.pure ())
                )
        (Type'anyPointer'unknown' tag) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'anyPointer'unknown' raw_ tag)
data Type'anyPointer'unconstrained 
    = Type'anyPointer'unconstrained'anyKind 
    | Type'anyPointer'unconstrained'struct 
    | Type'anyPointer'unconstrained'list 
    | Type'anyPointer'unconstrained'capability 
    | Type'anyPointer'unconstrained'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Type'anyPointer'unconstrained) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Type'anyPointer'unconstrained) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Type'anyPointer'unconstrained) where
    type Cerial msg Type'anyPointer'unconstrained = (Capnp.Gen.ById.Xa93fc509624c72d9.Type'anyPointer'unconstrained msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xa93fc509624c72d9.get_Type'anyPointer'unconstrained' raw)
        case raw of
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'anyPointer'unconstrained'anyKind) ->
                (Std_.pure Type'anyPointer'unconstrained'anyKind)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'anyPointer'unconstrained'struct) ->
                (Std_.pure Type'anyPointer'unconstrained'struct)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'anyPointer'unconstrained'list) ->
                (Std_.pure Type'anyPointer'unconstrained'list)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'anyPointer'unconstrained'capability) ->
                (Std_.pure Type'anyPointer'unconstrained'capability)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Type'anyPointer'unconstrained'unknown' tag) ->
                (Std_.pure (Type'anyPointer'unconstrained'unknown' tag))
        )
instance (Classes.Marshal Type'anyPointer'unconstrained) where
    marshalInto raw_ value_ = case value_ of
        (Type'anyPointer'unconstrained'anyKind) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'anyPointer'unconstrained'anyKind raw_)
        (Type'anyPointer'unconstrained'struct) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'anyPointer'unconstrained'struct raw_)
        (Type'anyPointer'unconstrained'list) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'anyPointer'unconstrained'list raw_)
        (Type'anyPointer'unconstrained'capability) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'anyPointer'unconstrained'capability raw_)
        (Type'anyPointer'unconstrained'unknown' tag) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Type'anyPointer'unconstrained'unknown' raw_ tag)
data Brand 
    = Brand 
        {scopes :: (V.Vector Brand'Scope)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Brand) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Brand) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Brand) where
    type Cerial msg Brand = (Capnp.Gen.ById.Xa93fc509624c72d9.Brand msg)
    decerialize raw = (Brand <$> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Brand'scopes raw) >>= Classes.decerialize))
instance (Classes.Marshal Brand) where
    marshalInto raw_ value_ = case value_ of
        Brand{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) scopes) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Brand'scopes raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Brand)
instance (Classes.Cerialize (V.Vector Brand)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Brand))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Brand)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Brand))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Brand)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Brand))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Brand)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Brand'Scope 
    = Brand'Scope 
        {scopeId :: Std_.Word64
        ,union' :: Brand'Scope'}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Brand'Scope) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Brand'Scope) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Brand'Scope) where
    type Cerial msg Brand'Scope = (Capnp.Gen.ById.Xa93fc509624c72d9.Brand'Scope msg)
    decerialize raw = (Brand'Scope <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Brand'Scope'scopeId raw)
                                   <*> (Classes.decerialize raw))
instance (Classes.Marshal Brand'Scope) where
    marshalInto raw_ value_ = case value_ of
        Brand'Scope{..} ->
            (do
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Brand'Scope'scopeId raw_ scopeId)
                (do
                    (Classes.marshalInto raw_ union')
                    )
                (Std_.pure ())
                )
instance (Classes.Cerialize Brand'Scope)
instance (Classes.Cerialize (V.Vector Brand'Scope)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Brand'Scope))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Brand'Scope)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Brand'Scope))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Brand'Scope)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Brand'Scope))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Brand'Scope)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Brand'Scope' 
    = Brand'Scope'bind (V.Vector Brand'Binding)
    | Brand'Scope'inherit 
    | Brand'Scope'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Brand'Scope') where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Brand'Scope') where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Brand'Scope') where
    type Cerial msg Brand'Scope' = (Capnp.Gen.ById.Xa93fc509624c72d9.Brand'Scope msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xa93fc509624c72d9.get_Brand'Scope' raw)
        case raw of
            (Capnp.Gen.ById.Xa93fc509624c72d9.Brand'Scope'bind raw) ->
                (Brand'Scope'bind <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Brand'Scope'inherit) ->
                (Std_.pure Brand'Scope'inherit)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Brand'Scope'unknown' tag) ->
                (Std_.pure (Brand'Scope'unknown' tag))
        )
instance (Classes.Marshal Brand'Scope') where
    marshalInto raw_ value_ = case value_ of
        (Brand'Scope'bind arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Brand'Scope'bind raw_))
        (Brand'Scope'inherit) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Brand'Scope'inherit raw_)
        (Brand'Scope'unknown' tag) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Brand'Scope'unknown' raw_ tag)
data Brand'Binding 
    = Brand'Binding'unbound 
    | Brand'Binding'type_ Type
    | Brand'Binding'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Brand'Binding) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Brand'Binding) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Brand'Binding) where
    type Cerial msg Brand'Binding = (Capnp.Gen.ById.Xa93fc509624c72d9.Brand'Binding msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xa93fc509624c72d9.get_Brand'Binding' raw)
        case raw of
            (Capnp.Gen.ById.Xa93fc509624c72d9.Brand'Binding'unbound) ->
                (Std_.pure Brand'Binding'unbound)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Brand'Binding'type_ raw) ->
                (Brand'Binding'type_ <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Brand'Binding'unknown' tag) ->
                (Std_.pure (Brand'Binding'unknown' tag))
        )
instance (Classes.Marshal Brand'Binding) where
    marshalInto raw_ value_ = case value_ of
        (Brand'Binding'unbound) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Brand'Binding'unbound raw_)
        (Brand'Binding'type_ arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Brand'Binding'type_ raw_))
        (Brand'Binding'unknown' tag) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Brand'Binding'unknown' raw_ tag)
instance (Classes.Cerialize Brand'Binding)
instance (Classes.Cerialize (V.Vector Brand'Binding)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Brand'Binding))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Brand'Binding)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Brand'Binding))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Brand'Binding)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Brand'Binding))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Brand'Binding)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Value 
    = Value'void 
    | Value'bool Std_.Bool
    | Value'int8 Std_.Int8
    | Value'int16 Std_.Int16
    | Value'int32 Std_.Int32
    | Value'int64 Std_.Int64
    | Value'uint8 Std_.Word8
    | Value'uint16 Std_.Word16
    | Value'uint32 Std_.Word32
    | Value'uint64 Std_.Word64
    | Value'float32 Std_.Float
    | Value'float64 Std_.Double
    | Value'text T.Text
    | Value'data_ BS.ByteString
    | Value'list (Std_.Maybe UntypedPure.Ptr)
    | Value'enum Std_.Word16
    | Value'struct (Std_.Maybe UntypedPure.Ptr)
    | Value'interface 
    | Value'anyPointer (Std_.Maybe UntypedPure.Ptr)
    | Value'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Value) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Value) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Value) where
    type Cerial msg Value = (Capnp.Gen.ById.Xa93fc509624c72d9.Value msg)
    decerialize raw = (do
        raw <- (Capnp.Gen.ById.Xa93fc509624c72d9.get_Value' raw)
        case raw of
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'void) ->
                (Std_.pure Value'void)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'bool raw) ->
                (Std_.pure (Value'bool raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'int8 raw) ->
                (Std_.pure (Value'int8 raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'int16 raw) ->
                (Std_.pure (Value'int16 raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'int32 raw) ->
                (Std_.pure (Value'int32 raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'int64 raw) ->
                (Std_.pure (Value'int64 raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'uint8 raw) ->
                (Std_.pure (Value'uint8 raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'uint16 raw) ->
                (Std_.pure (Value'uint16 raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'uint32 raw) ->
                (Std_.pure (Value'uint32 raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'uint64 raw) ->
                (Std_.pure (Value'uint64 raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'float32 raw) ->
                (Std_.pure (Value'float32 raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'float64 raw) ->
                (Std_.pure (Value'float64 raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'text raw) ->
                (Value'text <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'data_ raw) ->
                (Value'data_ <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'list raw) ->
                (Value'list <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'enum raw) ->
                (Std_.pure (Value'enum raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'struct raw) ->
                (Value'struct <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'interface) ->
                (Std_.pure Value'interface)
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'anyPointer raw) ->
                (Value'anyPointer <$> (Classes.decerialize raw))
            (Capnp.Gen.ById.Xa93fc509624c72d9.Value'unknown' tag) ->
                (Std_.pure (Value'unknown' tag))
        )
instance (Classes.Marshal Value) where
    marshalInto raw_ value_ = case value_ of
        (Value'void) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'void raw_)
        (Value'bool arg_) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'bool raw_ arg_)
        (Value'int8 arg_) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'int8 raw_ arg_)
        (Value'int16 arg_) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'int16 raw_ arg_)
        (Value'int32 arg_) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'int32 raw_ arg_)
        (Value'int64 arg_) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'int64 raw_ arg_)
        (Value'uint8 arg_) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'uint8 raw_ arg_)
        (Value'uint16 arg_) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'uint16 raw_ arg_)
        (Value'uint32 arg_) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'uint32 raw_ arg_)
        (Value'uint64 arg_) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'uint64 raw_ arg_)
        (Value'float32 arg_) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'float32 raw_ arg_)
        (Value'float64 arg_) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'float64 raw_ arg_)
        (Value'text arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'text raw_))
        (Value'data_ arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'data_ raw_))
        (Value'list arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'list raw_))
        (Value'enum arg_) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'enum raw_ arg_)
        (Value'struct arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'struct raw_))
        (Value'interface) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'interface raw_)
        (Value'anyPointer arg_) ->
            ((Classes.cerialize (Untyped.message raw_) arg_) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'anyPointer raw_))
        (Value'unknown' tag) ->
            (Capnp.Gen.ById.Xa93fc509624c72d9.set_Value'unknown' raw_ tag)
instance (Classes.Cerialize Value)
instance (Classes.Cerialize (V.Vector Value)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Value))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Value)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Value))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Value)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Value))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Value)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data Annotation 
    = Annotation 
        {id :: Std_.Word64
        ,value :: Value
        ,brand :: Brand}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default Annotation) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg Annotation) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize Annotation) where
    type Cerial msg Annotation = (Capnp.Gen.ById.Xa93fc509624c72d9.Annotation msg)
    decerialize raw = (Annotation <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_Annotation'id raw)
                                  <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Annotation'value raw) >>= Classes.decerialize)
                                  <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_Annotation'brand raw) >>= Classes.decerialize))
instance (Classes.Marshal Annotation) where
    marshalInto raw_ value_ = case value_ of
        Annotation{..} ->
            (do
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_Annotation'id raw_ id)
                ((Classes.cerialize (Untyped.message raw_) value) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Annotation'value raw_))
                ((Classes.cerialize (Untyped.message raw_) brand) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_Annotation'brand raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize Annotation)
instance (Classes.Cerialize (V.Vector Annotation)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector Annotation))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Annotation)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Annotation))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Annotation)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Annotation))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Annotation)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data CapnpVersion 
    = CapnpVersion 
        {major :: Std_.Word16
        ,minor :: Std_.Word8
        ,micro :: Std_.Word8}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default CapnpVersion) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg CapnpVersion) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize CapnpVersion) where
    type Cerial msg CapnpVersion = (Capnp.Gen.ById.Xa93fc509624c72d9.CapnpVersion msg)
    decerialize raw = (CapnpVersion <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_CapnpVersion'major raw)
                                    <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_CapnpVersion'minor raw)
                                    <*> (Capnp.Gen.ById.Xa93fc509624c72d9.get_CapnpVersion'micro raw))
instance (Classes.Marshal CapnpVersion) where
    marshalInto raw_ value_ = case value_ of
        CapnpVersion{..} ->
            (do
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_CapnpVersion'major raw_ major)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_CapnpVersion'minor raw_ minor)
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_CapnpVersion'micro raw_ micro)
                (Std_.pure ())
                )
instance (Classes.Cerialize CapnpVersion)
instance (Classes.Cerialize (V.Vector CapnpVersion)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector CapnpVersion))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector CapnpVersion)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector CapnpVersion))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CapnpVersion)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CapnpVersion))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CapnpVersion)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data CodeGeneratorRequest 
    = CodeGeneratorRequest 
        {nodes :: (V.Vector Node)
        ,requestedFiles :: (V.Vector CodeGeneratorRequest'RequestedFile)
        ,capnpVersion :: CapnpVersion
        ,sourceInfo :: (V.Vector Node'SourceInfo)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default CodeGeneratorRequest) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg CodeGeneratorRequest) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize CodeGeneratorRequest) where
    type Cerial msg CodeGeneratorRequest = (Capnp.Gen.ById.Xa93fc509624c72d9.CodeGeneratorRequest msg)
    decerialize raw = (CodeGeneratorRequest <$> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'nodes raw) >>= Classes.decerialize)
                                            <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'requestedFiles raw) >>= Classes.decerialize)
                                            <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'capnpVersion raw) >>= Classes.decerialize)
                                            <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'sourceInfo raw) >>= Classes.decerialize))
instance (Classes.Marshal CodeGeneratorRequest) where
    marshalInto raw_ value_ = case value_ of
        CodeGeneratorRequest{..} ->
            (do
                ((Classes.cerialize (Untyped.message raw_) nodes) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_CodeGeneratorRequest'nodes raw_))
                ((Classes.cerialize (Untyped.message raw_) requestedFiles) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_CodeGeneratorRequest'requestedFiles raw_))
                ((Classes.cerialize (Untyped.message raw_) capnpVersion) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_CodeGeneratorRequest'capnpVersion raw_))
                ((Classes.cerialize (Untyped.message raw_) sourceInfo) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_CodeGeneratorRequest'sourceInfo raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize CodeGeneratorRequest)
instance (Classes.Cerialize (V.Vector CodeGeneratorRequest)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector CodeGeneratorRequest))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector CodeGeneratorRequest)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector CodeGeneratorRequest))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CodeGeneratorRequest)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CodeGeneratorRequest))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CodeGeneratorRequest)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data CodeGeneratorRequest'RequestedFile 
    = CodeGeneratorRequest'RequestedFile 
        {id :: Std_.Word64
        ,filename :: T.Text
        ,imports :: (V.Vector CodeGeneratorRequest'RequestedFile'Import)}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default CodeGeneratorRequest'RequestedFile) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg CodeGeneratorRequest'RequestedFile) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize CodeGeneratorRequest'RequestedFile) where
    type Cerial msg CodeGeneratorRequest'RequestedFile = (Capnp.Gen.ById.Xa93fc509624c72d9.CodeGeneratorRequest'RequestedFile msg)
    decerialize raw = (CodeGeneratorRequest'RequestedFile <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'RequestedFile'id raw)
                                                          <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'RequestedFile'filename raw) >>= Classes.decerialize)
                                                          <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'RequestedFile'imports raw) >>= Classes.decerialize))
instance (Classes.Marshal CodeGeneratorRequest'RequestedFile) where
    marshalInto raw_ value_ = case value_ of
        CodeGeneratorRequest'RequestedFile{..} ->
            (do
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_CodeGeneratorRequest'RequestedFile'id raw_ id)
                ((Classes.cerialize (Untyped.message raw_) filename) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_CodeGeneratorRequest'RequestedFile'filename raw_))
                ((Classes.cerialize (Untyped.message raw_) imports) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_CodeGeneratorRequest'RequestedFile'imports raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize CodeGeneratorRequest'RequestedFile)
instance (Classes.Cerialize (V.Vector CodeGeneratorRequest'RequestedFile)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector CodeGeneratorRequest'RequestedFile))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector CodeGeneratorRequest'RequestedFile)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector CodeGeneratorRequest'RequestedFile))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CodeGeneratorRequest'RequestedFile)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CodeGeneratorRequest'RequestedFile))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CodeGeneratorRequest'RequestedFile)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
data CodeGeneratorRequest'RequestedFile'Import 
    = CodeGeneratorRequest'RequestedFile'Import 
        {id :: Std_.Word64
        ,name :: T.Text}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default CodeGeneratorRequest'RequestedFile'Import) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg CodeGeneratorRequest'RequestedFile'Import) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize CodeGeneratorRequest'RequestedFile'Import) where
    type Cerial msg CodeGeneratorRequest'RequestedFile'Import = (Capnp.Gen.ById.Xa93fc509624c72d9.CodeGeneratorRequest'RequestedFile'Import msg)
    decerialize raw = (CodeGeneratorRequest'RequestedFile'Import <$> (Capnp.Gen.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'RequestedFile'Import'id raw)
                                                                 <*> ((Capnp.Gen.ById.Xa93fc509624c72d9.get_CodeGeneratorRequest'RequestedFile'Import'name raw) >>= Classes.decerialize))
instance (Classes.Marshal CodeGeneratorRequest'RequestedFile'Import) where
    marshalInto raw_ value_ = case value_ of
        CodeGeneratorRequest'RequestedFile'Import{..} ->
            (do
                (Capnp.Gen.ById.Xa93fc509624c72d9.set_CodeGeneratorRequest'RequestedFile'Import'id raw_ id)
                ((Classes.cerialize (Untyped.message raw_) name) >>= (Capnp.Gen.ById.Xa93fc509624c72d9.set_CodeGeneratorRequest'RequestedFile'Import'name raw_))
                (Std_.pure ())
                )
instance (Classes.Cerialize CodeGeneratorRequest'RequestedFile'Import)
instance (Classes.Cerialize (V.Vector CodeGeneratorRequest'RequestedFile'Import)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize (V.Vector (V.Vector CodeGeneratorRequest'RequestedFile'Import))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector CodeGeneratorRequest'RequestedFile'Import)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector CodeGeneratorRequest'RequestedFile'Import))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CodeGeneratorRequest'RequestedFile'Import)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CodeGeneratorRequest'RequestedFile'Import))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector CodeGeneratorRequest'RequestedFile'Import)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Decerialize Capnp.Gen.ById.Xa93fc509624c72d9.ElementSize) where
    type Cerial msg Capnp.Gen.ById.Xa93fc509624c72d9.ElementSize = Capnp.Gen.ById.Xa93fc509624c72d9.ElementSize
    decerialize  = Std_.pure
instance (Classes.Cerialize Capnp.Gen.ById.Xa93fc509624c72d9.ElementSize) where
    cerialize _ = Std_.pure
instance (Classes.Cerialize (V.Vector Capnp.Gen.ById.Xa93fc509624c72d9.ElementSize)) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector Capnp.Gen.ById.Xa93fc509624c72d9.ElementSize))) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector Capnp.Gen.ById.Xa93fc509624c72d9.ElementSize)))) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector Capnp.Gen.ById.Xa93fc509624c72d9.ElementSize))))) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Capnp.Gen.ById.Xa93fc509624c72d9.ElementSize)))))) where
    cerialize  = Classes.cerializeBasicVec
instance (Classes.Cerialize (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector Capnp.Gen.ById.Xa93fc509624c72d9.ElementSize))))))) where
    cerialize  = Classes.cerializeBasicVec