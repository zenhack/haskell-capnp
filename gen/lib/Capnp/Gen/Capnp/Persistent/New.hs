{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.Persistent.New where
import qualified Capnp.Repr as R
import qualified Capnp.Fields as F
import qualified Capnp.New.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers.New as GH
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Persistent sturdyRef owner
type instance (R.ReprFor (Persistent sturdyRef owner)) = (R.Ptr (Std_.Just R.Cap))
data Persistent'SaveParams sturdyRef owner
type instance (R.ReprFor (Persistent'SaveParams sturdyRef owner)) = (R.Ptr (Std_.Just R.Struct))
instance ((GH.TypeParam sturdyRef pr_1)
         ,(GH.TypeParam owner pr_2)) => (OL.IsLabel "sealFor" (F.Field (Persistent'SaveParams sturdyRef owner) owner)) where
    fromLabel  = (GH.ptrField 0)
instance ((GH.TypeParam sturdyRef pr_1)
         ,(GH.TypeParam owner pr_2)) => (F.HasField "sealFor" (Persistent'SaveParams sturdyRef owner) owner)
data Persistent'SaveResults sturdyRef owner
type instance (R.ReprFor (Persistent'SaveResults sturdyRef owner)) = (R.Ptr (Std_.Just R.Struct))
instance ((GH.TypeParam sturdyRef pr_1)
         ,(GH.TypeParam owner pr_2)) => (OL.IsLabel "sturdyRef" (F.Field (Persistent'SaveResults sturdyRef owner) sturdyRef)) where
    fromLabel  = (GH.ptrField 0)
instance ((GH.TypeParam sturdyRef pr_1)
         ,(GH.TypeParam owner pr_2)) => (F.HasField "sturdyRef" (Persistent'SaveResults sturdyRef owner) sturdyRef)
data RealmGateway internalRef externalRef internalOwner externalOwner
type instance (R.ReprFor (RealmGateway internalRef externalRef internalOwner externalOwner)) = (R.Ptr (Std_.Just R.Cap))
data RealmGateway'import'params internalRef externalRef internalOwner externalOwner
type instance (R.ReprFor (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)) = (R.Ptr (Std_.Just R.Struct))
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (OL.IsLabel "cap" (F.Field (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (Persistent externalRef externalOwner))) where
    fromLabel  = (GH.ptrField 0)
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (F.HasField "cap" (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (Persistent externalRef externalOwner))
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (OL.IsLabel "params" (F.Field (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveParams internalRef internalOwner))) where
    fromLabel  = (GH.ptrField 1)
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (F.HasField "params" (RealmGateway'import'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveParams internalRef internalOwner))
data RealmGateway'export'params internalRef externalRef internalOwner externalOwner
type instance (R.ReprFor (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)) = (R.Ptr (Std_.Just R.Struct))
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (OL.IsLabel "cap" (F.Field (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (Persistent internalRef internalOwner))) where
    fromLabel  = (GH.ptrField 0)
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (F.HasField "cap" (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (Persistent internalRef internalOwner))
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (OL.IsLabel "params" (F.Field (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveParams externalRef externalOwner))) where
    fromLabel  = (GH.ptrField 1)
instance ((GH.TypeParam internalRef pr_1)
         ,(GH.TypeParam externalRef pr_2)
         ,(GH.TypeParam internalOwner pr_3)
         ,(GH.TypeParam externalOwner pr_4)) => (F.HasField "params" (RealmGateway'export'params internalRef externalRef internalOwner externalOwner) (Persistent'SaveParams externalRef externalOwner))