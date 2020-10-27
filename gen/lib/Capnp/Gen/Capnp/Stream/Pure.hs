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
module Capnp.Gen.Capnp.Stream.Pure(StreamResult(..)) where
import qualified Capnp.GenHelpers.ReExports.Data.Vector as V
import qualified Capnp.GenHelpers.ReExports.Data.Text as T
import qualified Capnp.GenHelpers.ReExports.Data.ByteString as BS
import qualified Capnp.GenHelpers.ReExports.Data.Default as Default
import qualified GHC.Generics as Generics
import qualified Control.Monad.IO.Class as MonadIO
import qualified Capnp.Untyped.Pure as UntypedPure
import qualified Capnp.Untyped as Untyped
import qualified Capnp.Message as Message
import qualified Capnp.Classes as Classes
import qualified Capnp.Basics.Pure as BasicsPure
import qualified Capnp.GenHelpers.Pure as GenHelpersPure
import qualified Capnp.Gen.ById.X86c366a91393f3f8
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data StreamResult 
    = StreamResult 
        {}
    deriving(Std_.Show
            ,Std_.Eq
            ,Generics.Generic)
instance (Default.Default StreamResult) where
    def  = GenHelpersPure.defaultStruct
instance (Classes.FromStruct Message.ConstMsg StreamResult) where
    fromStruct struct = ((Classes.fromStruct struct) >>= Classes.decerialize)
instance (Classes.Decerialize StreamResult) where
    type Cerial msg StreamResult = (Capnp.Gen.ById.X86c366a91393f3f8.StreamResult msg)
    decerialize raw = (Std_.pure StreamResult)
instance (Classes.Marshal StreamResult) where
    marshalInto raw_ value_ = case value_ of
        (StreamResult) ->
            (do
                (Std_.pure ())
                )
instance (Classes.Cerialize s StreamResult)
instance (Classes.Cerialize s (V.Vector StreamResult)) where
    cerialize  = GenHelpersPure.cerializeCompositeVec
instance (Classes.Cerialize s (V.Vector (V.Vector StreamResult))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector StreamResult)))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector StreamResult))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector StreamResult)))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector StreamResult))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec
instance (Classes.Cerialize s (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector (V.Vector StreamResult)))))))) where
    cerialize  = GenHelpersPure.cerializeBasicVec