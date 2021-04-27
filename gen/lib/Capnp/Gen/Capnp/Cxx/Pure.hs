{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Capnp.Cxx.Pure() where
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
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))