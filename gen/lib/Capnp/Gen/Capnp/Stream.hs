{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Capnp.Gen.Capnp.Stream where
import qualified Capnp.Message as Message
import qualified Capnp.Untyped as Untyped
import qualified Capnp.Basics as Basics
import qualified Capnp.GenHelpers as GenHelpers
import qualified Capnp.Classes as Classes
import qualified GHC.Generics as Generics
import qualified Capnp.Bits as Std_
import qualified Data.Maybe as Std_
import qualified Capnp.GenHelpers.ReExports.Data.ByteString as BS
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
newtype StreamResult msg
    = StreamResult'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (StreamResult msg)) where
    fromStruct struct = (Std_.pure (StreamResult'newtype_ struct))
instance (Classes.ToStruct msg (StreamResult msg)) where
    toStruct (StreamResult'newtype_ struct) = struct
instance (Untyped.HasMessage (StreamResult mut) mut) where
    message (StreamResult'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (StreamResult mut) mut) where
    messageDefault msg = (StreamResult'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (StreamResult msg)) where
    fromPtr msg ptr = (StreamResult'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (StreamResult (Message.Mut s))) where
    toPtr msg (StreamResult'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (StreamResult (Message.Mut s))) where
    new msg = (StreamResult'newtype_ <$> (Untyped.allocStruct msg 0 0))
instance (Basics.ListElem mut (StreamResult mut)) where
    newtype List mut (StreamResult mut)
        = StreamResult'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (StreamResult'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (StreamResult'List_ l) = (Untyped.ListStruct l)
    length (StreamResult'List_ l) = (Untyped.length l)
    index i (StreamResult'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (StreamResult (Message.Mut s))) where
    setIndex (StreamResult'newtype_ elt) i (StreamResult'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (StreamResult'List_ <$> (Untyped.allocCompositeList msg 0 0 len))