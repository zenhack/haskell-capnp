{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.RpcTwoparty.New where
import qualified Capnp.Repr as R
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Side 
type instance (R.ReprFor Side) = (R.Data R.Sz16)
data VatId 
type instance (R.ReprFor VatId) = (R.Ptr (Std_.Just R.Struct))
data ProvisionId 
type instance (R.ReprFor ProvisionId) = (R.Ptr (Std_.Just R.Struct))
data RecipientId 
type instance (R.ReprFor RecipientId) = (R.Ptr (Std_.Just R.Struct))
data ThirdPartyCapId 
type instance (R.ReprFor ThirdPartyCapId) = (R.Ptr (Std_.Just R.Struct))
data JoinKeyPart 
type instance (R.ReprFor JoinKeyPart) = (R.Ptr (Std_.Just R.Struct))
data JoinResult 
type instance (R.ReprFor JoinResult) = (R.Ptr (Std_.Just R.Struct))