{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
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
module Capnp.Gen.Capnp.Stream where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers as GH
import qualified Capnp.Classes as C
import qualified GHC.Generics as Generics
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data StreamResult 
type instance (R.ReprFor StreamResult) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId StreamResult) where
    typeId  = 11051721556433613166
instance (C.TypedStruct StreamResult) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate StreamResult) where
    type AllocHint StreamResult = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc StreamResult (C.Parsed StreamResult))
instance (C.AllocateList StreamResult) where
    type ListAllocHint StreamResult = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc StreamResult (C.Parsed StreamResult))
data instance C.Parsed StreamResult
    = StreamResult 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed StreamResult))
deriving instance (Std_.Eq (C.Parsed StreamResult))
instance (C.Parse StreamResult (C.Parsed StreamResult)) where
    parse raw_ = (Std_.pure StreamResult)
instance (C.Marshal StreamResult (C.Parsed StreamResult)) where
    marshalInto _raw (StreamResult) = (Std_.pure ())