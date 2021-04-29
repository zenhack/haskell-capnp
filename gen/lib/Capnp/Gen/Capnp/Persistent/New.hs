{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.Persistent.New where
import qualified Capnp.Repr as R
import qualified Capnp.New.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers.New as GH
import qualified Capnp.New.Classes as C
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Persistent sturdyRef owner
type instance (R.ReprFor (Persistent sturdyRef owner)) = (R.Ptr (Std_.Just R.Cap))
data Persistent'SaveParams sturdyRef owner
type instance (R.ReprFor (Persistent'SaveParams sturdyRef owner)) = (R.Ptr (Std_.Just R.Struct))
instance ((GH.TypeParam sturdyRef pr_1)
         ,(GH.TypeParam owner pr_2)) => (C.TypedStruct (Persistent'SaveParams sturdyRef owner)) where
    numStructWords  = 0
    numStructPtrs  = 1
instance ((GH.TypeParam sturdyRef pr_1)
         ,(GH.TypeParam owner pr_2)) => (C.Allocate (Persistent'SaveParams sturdyRef owner)) where
    type AllocHint (Persistent'SaveParams sturdyRef owner) = ()
    new  = GH.newStruct
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
    new  = GH.newStruct
instance ((GH.TypeParam sturdyRef pr_1)
         ,(GH.TypeParam owner pr_2)) => (GH.HasField "sturdyRef" GH.Slot (Persistent'SaveResults sturdyRef owner) sturdyRef) where
    fieldByLabel  = (GH.ptrField 0)
data RealmGateway internalRef externalRef internalOwner externalOwner
type instance (R.ReprFor (RealmGateway internalRef externalRef internalOwner externalOwner)) = (R.Ptr (Std_.Just R.Cap))
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
    new  = GH.newStruct
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
    new  = GH.newStruct
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