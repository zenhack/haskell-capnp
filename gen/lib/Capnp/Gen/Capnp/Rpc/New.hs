{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.Rpc.New where
import qualified Capnp.Repr as R
import qualified Capnp.Fields as F
import qualified GHC.OverloadedLabels as OL
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Message 
type instance (R.ReprFor Message) = (R.Ptr (Std_.Just R.Struct))
data Bootstrap 
type instance (R.ReprFor Bootstrap) = (R.Ptr (Std_.Just R.Struct))
data Call 
type instance (R.ReprFor Call) = (R.Ptr (Std_.Just R.Struct))
data Call'sendResultsTo 
type instance (R.ReprFor Call'sendResultsTo) = (R.Ptr (Std_.Just R.Struct))
data Return 
type instance (R.ReprFor Return) = (R.Ptr (Std_.Just R.Struct))
data Finish 
type instance (R.ReprFor Finish) = (R.Ptr (Std_.Just R.Struct))
data Resolve 
type instance (R.ReprFor Resolve) = (R.Ptr (Std_.Just R.Struct))
data Release 
type instance (R.ReprFor Release) = (R.Ptr (Std_.Just R.Struct))
data Disembargo 
type instance (R.ReprFor Disembargo) = (R.Ptr (Std_.Just R.Struct))
data Disembargo'context 
type instance (R.ReprFor Disembargo'context) = (R.Ptr (Std_.Just R.Struct))
data Provide 
type instance (R.ReprFor Provide) = (R.Ptr (Std_.Just R.Struct))
data Accept 
type instance (R.ReprFor Accept) = (R.Ptr (Std_.Just R.Struct))
data Join 
type instance (R.ReprFor Join) = (R.Ptr (Std_.Just R.Struct))
data MessageTarget 
type instance (R.ReprFor MessageTarget) = (R.Ptr (Std_.Just R.Struct))
data Payload 
type instance (R.ReprFor Payload) = (R.Ptr (Std_.Just R.Struct))
data CapDescriptor 
type instance (R.ReprFor CapDescriptor) = (R.Ptr (Std_.Just R.Struct))
data PromisedAnswer 
type instance (R.ReprFor PromisedAnswer) = (R.Ptr (Std_.Just R.Struct))
data PromisedAnswer'Op 
type instance (R.ReprFor PromisedAnswer'Op) = (R.Ptr (Std_.Just R.Struct))
data ThirdPartyCapDescriptor 
type instance (R.ReprFor ThirdPartyCapDescriptor) = (R.Ptr (Std_.Just R.Struct))
data Exception 
type instance (R.ReprFor Exception) = (R.Ptr (Std_.Just R.Struct))
data Exception'Type 
type instance (R.ReprFor Exception'Type) = (R.Data R.Sz16)