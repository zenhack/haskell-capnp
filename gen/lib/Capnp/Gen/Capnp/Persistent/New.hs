{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
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
module Capnp.Gen.Capnp.Persistent.New where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.New.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers.New as GH
import qualified Capnp.New.Classes as C
import qualified GHC.Generics as Generics
import qualified Capnp.GenHelpers.ReExports.Data.ByteString as BS
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Persistent sturdyRef owner
type instance (R.ReprFor (Persistent sturdyRef owner)) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId (Persistent sturdyRef owner)) where
    typeId  = 14468694717054801553
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.Parse (Persistent sturdyRef owner) (GH.Client (Persistent sturdyRef owner))) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (GH.Export (Persistent sturdyRef owner)) where
    type Server (Persistent sturdyRef owner) = (Persistent'server_ sturdyRef owner)
    serverToCallHandler _ s_ = (GH.buildCallHandler [(C.typeId @((Persistent sturdyRef owner))
                                                     ,[(GH.toUntypedMethodHandler (persistent'save @(sturdyRef) @(owner) s_))])])
class (Persistent'server_ sturdyRef owner s_) where
    persistent'save :: s_ -> (GH.MethodHandler (Persistent'SaveParams sturdyRef owner) (Persistent'SaveResults sturdyRef owner))
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (GH.HasMethod "save" (Persistent sturdyRef owner) (Persistent'SaveParams sturdyRef owner) (Persistent'SaveResults sturdyRef owner)) where
    methodByLabel  = (GH.Method 14468694717054801553 0)
data Persistent'SaveParams sturdyRef owner
type instance (R.ReprFor (Persistent'SaveParams sturdyRef owner)) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId (Persistent'SaveParams sturdyRef owner)) where
    typeId  = 17829674341603767205
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.TypedStruct (Persistent'SaveParams sturdyRef owner)) where
    numStructWords  = 0
    numStructPtrs  = 1
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.Allocate (Persistent'SaveParams sturdyRef owner)) where
    type AllocHint (Persistent'SaveParams sturdyRef owner) = ()
    new _ = C.newTypedStruct
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.EstimateAlloc (Persistent'SaveParams sturdyRef owner) (C.Parsed (Persistent'SaveParams sturdyRef owner)))
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.AllocateList (Persistent'SaveParams sturdyRef owner)) where
    type ListAllocHint (Persistent'SaveParams sturdyRef owner) = Std_.Int
    newList  = C.newTypedStructList
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.EstimateListAlloc (Persistent'SaveParams sturdyRef owner) (C.Parsed (Persistent'SaveParams sturdyRef owner)))
data instance C.Parsed (Persistent'SaveParams sturdyRef owner)
    = Persistent'SaveParams 
        {sealFor :: (RP.Parsed owner)}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed sturdyRef))
                  ,(Std_.Show (RP.Parsed owner))) => (Std_.Show (C.Parsed (Persistent'SaveParams sturdyRef owner)))
deriving instance ((Std_.Eq (RP.Parsed sturdyRef))
                  ,(Std_.Eq (RP.Parsed owner))) => (Std_.Eq (C.Parsed (Persistent'SaveParams sturdyRef owner)))
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.Parse (Persistent'SaveParams sturdyRef owner) (C.Parsed (Persistent'SaveParams sturdyRef owner))) where
    parse raw_ = (Persistent'SaveParams <$> (GH.parseField #sealFor raw_))
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.Marshal (Persistent'SaveParams sturdyRef owner) (C.Parsed (Persistent'SaveParams sturdyRef owner))) where
    marshalInto raw_ Persistent'SaveParams{..} = (do
        (GH.encodeField #sealFor sealFor raw_)
        (Std_.pure ())
        )
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (GH.HasField "sealFor" GH.Slot (Persistent'SaveParams sturdyRef owner) owner) where
    fieldByLabel  = (GH.ptrField 0)
data Persistent'SaveResults sturdyRef owner
type instance (R.ReprFor (Persistent'SaveResults sturdyRef owner)) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId (Persistent'SaveResults sturdyRef owner)) where
    typeId  = 13215893102637674431
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.TypedStruct (Persistent'SaveResults sturdyRef owner)) where
    numStructWords  = 0
    numStructPtrs  = 1
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.Allocate (Persistent'SaveResults sturdyRef owner)) where
    type AllocHint (Persistent'SaveResults sturdyRef owner) = ()
    new _ = C.newTypedStruct
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.EstimateAlloc (Persistent'SaveResults sturdyRef owner) (C.Parsed (Persistent'SaveResults sturdyRef owner)))
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.AllocateList (Persistent'SaveResults sturdyRef owner)) where
    type ListAllocHint (Persistent'SaveResults sturdyRef owner) = Std_.Int
    newList  = C.newTypedStructList
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.EstimateListAlloc (Persistent'SaveResults sturdyRef owner) (C.Parsed (Persistent'SaveResults sturdyRef owner)))
data instance C.Parsed (Persistent'SaveResults sturdyRef owner)
    = Persistent'SaveResults 
        {sturdyRef :: (RP.Parsed sturdyRef)}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed sturdyRef))
                  ,(Std_.Show (RP.Parsed owner))) => (Std_.Show (C.Parsed (Persistent'SaveResults sturdyRef owner)))
deriving instance ((Std_.Eq (RP.Parsed sturdyRef))
                  ,(Std_.Eq (RP.Parsed owner))) => (Std_.Eq (C.Parsed (Persistent'SaveResults sturdyRef owner)))
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.Parse (Persistent'SaveResults sturdyRef owner) (C.Parsed (Persistent'SaveResults sturdyRef owner))) where
    parse raw_ = (Persistent'SaveResults <$> (GH.parseField #sturdyRef raw_))
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (C.Marshal (Persistent'SaveResults sturdyRef owner) (C.Parsed (Persistent'SaveResults sturdyRef owner))) where
    marshalInto raw_ Persistent'SaveResults{..} = (do
        (GH.encodeField #sturdyRef sturdyRef raw_)
        (Std_.pure ())
        )
instance ((GH.TypeParam sturdyRef)
         ,(GH.TypeParam owner)) => (GH.HasField "sturdyRef" GH.Slot (Persistent'SaveResults sturdyRef owner) sturdyRef) where
    fieldByLabel  = (GH.ptrField 0)
data RealmGateway internalRef externalRef internalOwner externalOwner
type instance (R.ReprFor (RealmGateway internalRef externalRef internalOwner externalOwner)) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId (RealmGateway internalRef externalRef internalOwner externalOwner)) where
    typeId  = 9583422979879616212
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.Parse (RealmGateway internalRef externalRef internalOwner externalOwner) (GH.Client (RealmGateway internalRef externalRef internalOwner externalOwner))) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (GH.Export (RealmGateway internalRef externalRef internalOwner externalOwner)) where
    type Server (RealmGateway internalRef externalRef internalOwner externalOwner) = (RealmGateway'server_ internalRef externalRef internalOwner externalOwner)
    serverToCallHandler _ s_ = (GH.buildCallHandler [(C.typeId @((RealmGateway internalRef externalRef internalOwner externalOwner))
                                                     ,[(GH.toUntypedMethodHandler (realmGateway'import_ @(internalRef) @(externalRef) @(internalOwner) @(externalOwner) s_))
                                                      ,(GH.toUntypedMethodHandler (realmGateway'export @(internalRef) @(externalRef) @(internalOwner) @(externalOwner) s_))])])
class (RealmGateway'server_ internalRef externalRef internalOwner externalOwner s_) where
    realmGateway'import_ :: s_ -> (GH.MethodHandler (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveResults internalRef internalOwner))
    realmGateway'export :: s_ -> (GH.MethodHandler (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveResults externalRef externalOwner))
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (GH.HasMethod "import_" (RealmGateway internalRef externalRef internalOwner externalOwner) (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveResults internalRef internalOwner)) where
    methodByLabel  = (GH.Method 9583422979879616212 0)
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (GH.HasMethod "export" (RealmGateway internalRef externalRef internalOwner externalOwner) (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveResults externalRef externalOwner)) where
    methodByLabel  = (GH.Method 9583422979879616212 1)
data RealmGateway'import'params internalRef externalRef internalOwner externalOwner
type instance (R.ReprFor (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)) where
    typeId  = 17348653140467603277
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.TypedStruct (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)) where
    numStructWords  = 0
    numStructPtrs  = 2
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.Allocate (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)) where
    type AllocHint (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) = ()
    new _ = C.newTypedStruct
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.EstimateAlloc (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (C.Parsed (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)))
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.AllocateList (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)) where
    type ListAllocHint (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) = Std_.Int
    newList  = C.newTypedStructList
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.EstimateListAlloc (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (C.Parsed (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)))
data instance C.Parsed (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)
    = RealmGateway'import'params 
        {cap :: (RP.Parsed (Persistent externalRef externalOwner))
        ,params :: (RP.Parsed (Persistent'SaveParams internalRef internalOwner))}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed internalRef))
                  ,(Std_.Show (RP.Parsed externalRef))
                  ,(Std_.Show (RP.Parsed internalOwner))
                  ,(Std_.Show (RP.Parsed externalOwner))) => (Std_.Show (C.Parsed (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)))
deriving instance ((Std_.Eq (RP.Parsed internalRef))
                  ,(Std_.Eq (RP.Parsed externalRef))
                  ,(Std_.Eq (RP.Parsed internalOwner))
                  ,(Std_.Eq (RP.Parsed externalOwner))) => (Std_.Eq (C.Parsed (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)))
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.Parse (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (C.Parsed (RealmGateway'import'params internalRef externalRef internalOwner externalOwner))) where
    parse raw_ = (RealmGateway'import'params <$> (GH.parseField #cap raw_)
                                             <*> (GH.parseField #params raw_))
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.Marshal (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (C.Parsed (RealmGateway'import'params internalRef externalRef internalOwner externalOwner))) where
    marshalInto raw_ RealmGateway'import'params{..} = (do
        (GH.encodeField #cap cap raw_)
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (GH.HasField "cap" GH.Slot (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (Persistent externalRef externalOwner)) where
    fieldByLabel  = (GH.ptrField 0)
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (GH.HasField "params" GH.Slot (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveParams internalRef internalOwner)) where
    fieldByLabel  = (GH.ptrField 1)
data RealmGateway'export'params internalRef externalRef internalOwner externalOwner
type instance (R.ReprFor (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)) where
    typeId  = 17055027933458834346
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.TypedStruct (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)) where
    numStructWords  = 0
    numStructPtrs  = 2
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.Allocate (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)) where
    type AllocHint (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) = ()
    new _ = C.newTypedStruct
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.EstimateAlloc (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (C.Parsed (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)))
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.AllocateList (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)) where
    type ListAllocHint (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) = Std_.Int
    newList  = C.newTypedStructList
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.EstimateListAlloc (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (C.Parsed (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)))
data instance C.Parsed (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)
    = RealmGateway'export'params 
        {cap :: (RP.Parsed (Persistent internalRef internalOwner))
        ,params :: (RP.Parsed (Persistent'SaveParams externalRef externalOwner))}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed internalRef))
                  ,(Std_.Show (RP.Parsed externalRef))
                  ,(Std_.Show (RP.Parsed internalOwner))
                  ,(Std_.Show (RP.Parsed externalOwner))) => (Std_.Show (C.Parsed (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)))
deriving instance ((Std_.Eq (RP.Parsed internalRef))
                  ,(Std_.Eq (RP.Parsed externalRef))
                  ,(Std_.Eq (RP.Parsed internalOwner))
                  ,(Std_.Eq (RP.Parsed externalOwner))) => (Std_.Eq (C.Parsed (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)))
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.Parse (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (C.Parsed (RealmGateway'export'params internalRef externalRef internalOwner externalOwner))) where
    parse raw_ = (RealmGateway'export'params <$> (GH.parseField #cap raw_)
                                             <*> (GH.parseField #params raw_))
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (C.Marshal (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (C.Parsed (RealmGateway'export'params internalRef externalRef internalOwner externalOwner))) where
    marshalInto raw_ RealmGateway'export'params{..} = (do
        (GH.encodeField #cap cap raw_)
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (GH.HasField "cap" GH.Slot (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (Persistent internalRef internalOwner)) where
    fieldByLabel  = (GH.ptrField 0)
instance ((GH.TypeParam internalRef)
         ,(GH.TypeParam externalRef)
         ,(GH.TypeParam internalOwner)
         ,(GH.TypeParam externalOwner)) => (GH.HasField "params" GH.Slot (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveParams externalRef externalOwner)) where
    fieldByLabel  = (GH.ptrField 1)