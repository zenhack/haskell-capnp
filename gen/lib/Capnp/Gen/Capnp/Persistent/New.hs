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
module Capnp.Gen.Capnp.Persistent.New where
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
data Persistent sturdyRef owner
type instance (R.ReprFor (Persistent sturdyRef owner)) = (R.Ptr (Std_.Just R.Cap))
instance ((GH.TypeParam sturdyRef pr_1)
         ,(GH.TypeParam owner pr_2)) => (GH.HasMethod "save" (Persistent sturdyRef owner) (Persistent'SaveParams sturdyRef owner) (Persistent'SaveResults sturdyRef owner)) where
    methodByLabel  = (GH.Method 14468694717054801553 0)
data Persistent'SaveParams sturdyRef owner
type instance (R.ReprFor (Persistent'SaveParams sturdyRef owner)) = (R.Ptr (Std_.Just R.Struct))
instance ((GH.TypeParam sturdyRef pr_1)
         ,(GH.TypeParam owner pr_2)) => (C.TypedStruct (Persistent'SaveParams sturdyRef owner)) where
    numStructWords  = 0
    numStructPtrs  = 1
instance ((GH.TypeParam sturdyRef pr_1)
         ,(GH.TypeParam owner pr_2)) => (C.Allocate (Persistent'SaveParams sturdyRef owner)) where
    type AllocHint (Persistent'SaveParams sturdyRef owner) = ()
    new _ = C.newTypedStruct
data instance C.Parsed (Persistent'SaveParams sturdyRef owner)
    = Persistent'SaveParams 
        {sealFor :: (RP.Parsed owner)}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed sturdyRef))
                  ,(Std_.Show (RP.Parsed owner))) => (Std_.Show (C.Parsed (Persistent'SaveParams sturdyRef owner)))
deriving instance ((Std_.Eq (RP.Parsed sturdyRef))
                  ,(Std_.Eq (RP.Parsed owner))) => (Std_.Eq (C.Parsed (Persistent'SaveParams sturdyRef owner)))
instance ((GH.TypeParam sturdyRef pr_1)
         ,(GH.TypeParam owner pr_2)) => (GH.HasField "sealFor" GH.Slot (Persistent'SaveParams sturdyRef owner) owner) where
    fieldByLabel  = (GH.ptrField 0)
data Persistent'SaveResults sturdyRef owner
type instance (R.ReprFor (Persistent'SaveResults sturdyRef owner)) = (R.Ptr (Std_.Just R.Struct))
instance ((GH.TypeParam sturdyRef pr_1)
         ,(GH.TypeParam owner pr_2)) => (C.TypedStruct (Persistent'SaveResults sturdyRef owner)) where
    numStructWords  = 0
    numStructPtrs  = 1
instance ((GH.TypeParam sturdyRef pr_1)
         ,(GH.TypeParam owner pr_2)) => (C.Allocate (Persistent'SaveResults sturdyRef owner)) where
    type AllocHint (Persistent'SaveResults sturdyRef owner) = ()
    new _ = C.newTypedStruct
data instance C.Parsed (Persistent'SaveResults sturdyRef owner)
    = Persistent'SaveResults 
        {sturdyRef :: (RP.Parsed sturdyRef)}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed sturdyRef))
                  ,(Std_.Show (RP.Parsed owner))) => (Std_.Show (C.Parsed (Persistent'SaveResults sturdyRef owner)))
deriving instance ((Std_.Eq (RP.Parsed sturdyRef))
                  ,(Std_.Eq (RP.Parsed owner))) => (Std_.Eq (C.Parsed (Persistent'SaveResults sturdyRef owner)))
instance ((GH.TypeParam sturdyRef pr_1)
         ,(GH.TypeParam owner pr_2)) => (GH.HasField "sturdyRef" GH.Slot (Persistent'SaveResults sturdyRef owner) sturdyRef) where
    fieldByLabel  = (GH.ptrField 0)
data RealmGateway internalRef externalRef internalOwner externalOwner
type instance (R.ReprFor (RealmGateway internalRef externalRef internalOwner externalOwner)) = (R.Ptr (Std_.Just R.Cap))
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (GH.HasMethod "import_" (RealmGateway internalRef externalRef internalOwner externalOwner) (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveResults internalRef internalOwner)) where
    methodByLabel  = (GH.Method 9583422979879616212 0)
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (GH.HasMethod "export" (RealmGateway internalRef externalRef internalOwner externalOwner) (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveResults externalRef externalOwner)) where
    methodByLabel  = (GH.Method 9583422979879616212 1)
data RealmGateway'import'params internalRef externalRef internalOwner externalOwner
type instance (R.ReprFor (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)) = (R.Ptr (Std_.Just R.Struct))
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (C.TypedStruct (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)) where
    numStructWords  = 0
    numStructPtrs  = 2
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (C.Allocate (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)) where
    type AllocHint (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) = ()
    new _ = C.newTypedStruct
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
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (GH.HasField "cap" GH.Slot (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (Persistent externalRef externalOwner)) where
    fieldByLabel  = (GH.ptrField 0)
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (GH.HasField "params" GH.Slot (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveParams internalRef internalOwner)) where
    fieldByLabel  = (GH.ptrField 1)
data RealmGateway'export'params internalRef externalRef internalOwner externalOwner
type instance (R.ReprFor (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)) = (R.Ptr (Std_.Just R.Struct))
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (C.TypedStruct (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)) where
    numStructWords  = 0
    numStructPtrs  = 2
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (C.Allocate (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)) where
    type AllocHint (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) = ()
    new _ = C.newTypedStruct
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
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (GH.HasField "cap" GH.Slot (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (Persistent internalRef internalOwner)) where
    fieldByLabel  = (GH.ptrField 0)
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (GH.HasField "params" GH.Slot (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveParams externalRef externalOwner)) where
    fieldByLabel  = (GH.ptrField 1)