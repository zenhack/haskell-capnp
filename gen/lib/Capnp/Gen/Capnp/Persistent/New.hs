{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.Persistent.New where
import qualified Capnp.Repr as R
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Persistent sturdyRef owner
type instance (R.ReprFor (Persistent sturdyRef owner)) = (R.Ptr (Std_.Just R.Cap))
data Persistent'SaveParams sturdyRef owner
type instance (R.ReprFor (Persistent'SaveParams sturdyRef owner)) = (R.Ptr (Std_.Just R.Struct))
data Persistent'SaveResults sturdyRef owner
type instance (R.ReprFor (Persistent'SaveResults sturdyRef owner)) = (R.Ptr (Std_.Just R.Struct))
data RealmGateway internalRef externalRef internalOwner externalOwner
type instance (R.ReprFor (RealmGateway internalRef externalRef internalOwner externalOwner)) = (R.Ptr (Std_.Just R.Cap))
data RealmGateway'import'params internalRef externalRef internalOwner externalOwner
type instance (R.ReprFor (RealmGateway'import'params internalRef externalRef internalOwner externalOwner)) = (R.Ptr (Std_.Just R.Struct))
data RealmGateway'export'params internalRef externalRef internalOwner externalOwner
type instance (R.ReprFor (RealmGateway'export'params internalRef externalRef internalOwner externalOwner)) = (R.Ptr (Std_.Just R.Struct))