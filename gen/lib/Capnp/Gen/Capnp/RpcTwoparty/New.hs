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
module Capnp.Gen.Capnp.RpcTwoparty.New where
import qualified Capnp.Repr as R
import qualified Capnp.Fields as F
import qualified Capnp.New.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers.New as GH
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Side 
type instance (R.ReprFor Side) = (R.Data R.Sz16)
data VatId 
type instance (R.ReprFor VatId) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "side" (F.Field (VatId) Side)) where
    fromLabel  = (GH.dataField 0 0 16 0)
instance (F.HasField "side" (VatId) Side)
data ProvisionId 
type instance (R.ReprFor ProvisionId) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "joinId" (F.Field (ProvisionId) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "joinId" (ProvisionId) Std_.Word32)
data RecipientId 
type instance (R.ReprFor RecipientId) = (R.Ptr (Std_.Just R.Struct))
data ThirdPartyCapId 
type instance (R.ReprFor ThirdPartyCapId) = (R.Ptr (Std_.Just R.Struct))
data JoinKeyPart 
type instance (R.ReprFor JoinKeyPart) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "joinId" (F.Field (JoinKeyPart) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "joinId" (JoinKeyPart) Std_.Word32)
instance (OL.IsLabel "partCount" (F.Field (JoinKeyPart) Std_.Word16)) where
    fromLabel  = (GH.dataField 32 0 16 0)
instance (F.HasField "partCount" (JoinKeyPart) Std_.Word16)
instance (OL.IsLabel "partNum" (F.Field (JoinKeyPart) Std_.Word16)) where
    fromLabel  = (GH.dataField 48 0 16 0)
instance (F.HasField "partNum" (JoinKeyPart) Std_.Word16)
data JoinResult 
type instance (R.ReprFor JoinResult) = (R.Ptr (Std_.Just R.Struct))
instance (OL.IsLabel "joinId" (F.Field (JoinResult) Std_.Word32)) where
    fromLabel  = (GH.dataField 0 0 32 0)
instance (F.HasField "joinId" (JoinResult) Std_.Word32)
instance (OL.IsLabel "succeeded" (F.Field (JoinResult) Std_.Bool)) where
    fromLabel  = (GH.dataField 32 0 1 0)
instance (F.HasField "succeeded" (JoinResult) Std_.Bool)
instance (OL.IsLabel "cap" (F.Field (JoinResult) Basics.AnyPointer)) where
    fromLabel  = (GH.ptrField 0)
instance (F.HasField "cap" (JoinResult) Basics.AnyPointer)