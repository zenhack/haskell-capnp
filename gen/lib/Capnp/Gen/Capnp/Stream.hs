{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
instance (Untyped.HasMessage (StreamResult msg)) where
    type InMessage (StreamResult msg) = msg
    message (StreamResult'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (StreamResult msg)) where
    messageDefault msg = (StreamResult'newtype_ (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (StreamResult msg)) where
    fromPtr msg ptr = (StreamResult'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (StreamResult (Message.MutMsg s))) where
    toPtr msg (StreamResult'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (StreamResult (Message.MutMsg s))) where
    new msg = (StreamResult'newtype_ <$> (Untyped.allocStruct msg 0 0))
instance (Basics.ListElem msg (StreamResult msg)) where
    newtype List msg (StreamResult msg)
        = StreamResult'List_ (Untyped.ListOf msg (Untyped.Struct msg))
    listFromPtr msg ptr = (StreamResult'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (StreamResult'List_ l) = (Untyped.ListStruct l)
    length (StreamResult'List_ l) = (Untyped.length l)
    index i (StreamResult'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (StreamResult (Message.MutMsg s))) where
    setIndex (StreamResult'newtype_ elt) i (StreamResult'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (StreamResult'List_ <$> (Untyped.allocCompositeList msg 0 0 len))