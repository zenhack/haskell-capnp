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
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Capnp.Gen.Capnp.Rpc where
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
newtype Message msg
    = Message'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Message msg)) where
    fromStruct struct = (Std_.pure (Message'newtype_ struct))
instance (Classes.ToStruct msg (Message msg)) where
    toStruct (Message'newtype_ struct) = struct
instance (Untyped.HasMessage (Message mut) mut) where
    message (Message'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Message mut) mut) where
    messageDefault msg = (Message'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Message msg)) where
    fromPtr msg ptr = (Message'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Message (Message.Mut s))) where
    toPtr msg (Message'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Message (Message.Mut s))) where
    new msg = (Message'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem mut (Message mut)) where
    newtype List mut (Message mut)
        = Message'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Message'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Message'List_ l) = (Untyped.ListStruct l)
    length (Message'List_ l) = (Untyped.length l)
    index i (Message'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Message (Message.Mut s))) where
    setIndex (Message'newtype_ elt) i (Message'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Message'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
data Message' (mut :: Message.Mutability)
    = Message'unimplemented (Message mut)
    | Message'abort (Exception mut)
    | Message'call (Call mut)
    | Message'return (Return mut)
    | Message'finish (Finish mut)
    | Message'resolve (Resolve mut)
    | Message'release (Release mut)
    | Message'obsoleteSave (Std_.Maybe (Untyped.Ptr mut))
    | Message'bootstrap (Bootstrap mut)
    | Message'obsoleteDelete (Std_.Maybe (Untyped.Ptr mut))
    | Message'provide (Provide mut)
    | Message'accept (Accept mut)
    | Message'join (Join mut)
    | Message'disembargo (Disembargo mut)
    | Message'unknown' Std_.Word16
instance (Classes.FromStruct mut (Message' mut)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 0)
        case tag of
            0 ->
                (Message'unimplemented <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            1 ->
                (Message'abort <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            2 ->
                (Message'call <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            3 ->
                (Message'return <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            4 ->
                (Message'finish <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            5 ->
                (Message'resolve <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            6 ->
                (Message'release <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            7 ->
                (Message'obsoleteSave <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            8 ->
                (Message'bootstrap <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            9 ->
                (Message'obsoleteDelete <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            10 ->
                (Message'provide <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            11 ->
                (Message'accept <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            12 ->
                (Message'join <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            13 ->
                (Message'disembargo <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            _ ->
                (Std_.pure (Message'unknown' (Std_.fromIntegral tag)))
        )
get_Message' :: ((Untyped.ReadCtx m msg)
                ,(Classes.FromStruct msg (Message' msg))) => (Message msg) -> (m (Message' msg))
get_Message' (Message'newtype_ struct) = (Classes.fromStruct struct)
set_Message'unimplemented :: ((Untyped.RWCtx m s)
                             ,(Classes.ToPtr s (Message (Message.Mut s)))) => (Message (Message.Mut s)) -> (Message (Message.Mut s)) -> (m ())
set_Message'unimplemented (Message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'abort :: ((Untyped.RWCtx m s)
                     ,(Classes.ToPtr s (Exception (Message.Mut s)))) => (Message (Message.Mut s)) -> (Exception (Message.Mut s)) -> (m ())
set_Message'abort (Message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'call :: ((Untyped.RWCtx m s)
                    ,(Classes.ToPtr s (Call (Message.Mut s)))) => (Message (Message.Mut s)) -> (Call (Message.Mut s)) -> (m ())
set_Message'call (Message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'return :: ((Untyped.RWCtx m s)
                      ,(Classes.ToPtr s (Return (Message.Mut s)))) => (Message (Message.Mut s)) -> (Return (Message.Mut s)) -> (m ())
set_Message'return (Message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'finish :: ((Untyped.RWCtx m s)
                      ,(Classes.ToPtr s (Finish (Message.Mut s)))) => (Message (Message.Mut s)) -> (Finish (Message.Mut s)) -> (m ())
set_Message'finish (Message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (4 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'resolve :: ((Untyped.RWCtx m s)
                       ,(Classes.ToPtr s (Resolve (Message.Mut s)))) => (Message (Message.Mut s)) -> (Resolve (Message.Mut s)) -> (m ())
set_Message'resolve (Message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (5 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'release :: ((Untyped.RWCtx m s)
                       ,(Classes.ToPtr s (Release (Message.Mut s)))) => (Message (Message.Mut s)) -> (Release (Message.Mut s)) -> (m ())
set_Message'release (Message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (6 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'obsoleteSave :: ((Untyped.RWCtx m s)
                            ,(Classes.ToPtr s (Std_.Maybe (Untyped.Ptr (Message.Mut s))))) => (Message (Message.Mut s)) -> (Std_.Maybe (Untyped.Ptr (Message.Mut s))) -> (m ())
set_Message'obsoleteSave (Message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (7 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'bootstrap :: ((Untyped.RWCtx m s)
                         ,(Classes.ToPtr s (Bootstrap (Message.Mut s)))) => (Message (Message.Mut s)) -> (Bootstrap (Message.Mut s)) -> (m ())
set_Message'bootstrap (Message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (8 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'obsoleteDelete :: ((Untyped.RWCtx m s)
                              ,(Classes.ToPtr s (Std_.Maybe (Untyped.Ptr (Message.Mut s))))) => (Message (Message.Mut s)) -> (Std_.Maybe (Untyped.Ptr (Message.Mut s))) -> (m ())
set_Message'obsoleteDelete (Message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (9 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'provide :: ((Untyped.RWCtx m s)
                       ,(Classes.ToPtr s (Provide (Message.Mut s)))) => (Message (Message.Mut s)) -> (Provide (Message.Mut s)) -> (m ())
set_Message'provide (Message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (10 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'accept :: ((Untyped.RWCtx m s)
                      ,(Classes.ToPtr s (Accept (Message.Mut s)))) => (Message (Message.Mut s)) -> (Accept (Message.Mut s)) -> (m ())
set_Message'accept (Message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (11 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'join :: ((Untyped.RWCtx m s)
                    ,(Classes.ToPtr s (Join (Message.Mut s)))) => (Message (Message.Mut s)) -> (Join (Message.Mut s)) -> (m ())
set_Message'join (Message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (12 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'disembargo :: ((Untyped.RWCtx m s)
                          ,(Classes.ToPtr s (Disembargo (Message.Mut s)))) => (Message (Message.Mut s)) -> (Disembargo (Message.Mut s)) -> (m ())
set_Message'disembargo (Message'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (13 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Message'unknown' :: ((Untyped.RWCtx m s)) => (Message (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Message'unknown' (Message'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
newtype Bootstrap msg
    = Bootstrap'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Bootstrap msg)) where
    fromStruct struct = (Std_.pure (Bootstrap'newtype_ struct))
instance (Classes.ToStruct msg (Bootstrap msg)) where
    toStruct (Bootstrap'newtype_ struct) = struct
instance (Untyped.HasMessage (Bootstrap mut) mut) where
    message (Bootstrap'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Bootstrap mut) mut) where
    messageDefault msg = (Bootstrap'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Bootstrap msg)) where
    fromPtr msg ptr = (Bootstrap'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Bootstrap (Message.Mut s))) where
    toPtr msg (Bootstrap'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Bootstrap (Message.Mut s))) where
    new msg = (Bootstrap'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem mut (Bootstrap mut)) where
    newtype List mut (Bootstrap mut)
        = Bootstrap'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Bootstrap'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Bootstrap'List_ l) = (Untyped.ListStruct l)
    length (Bootstrap'List_ l) = (Untyped.length l)
    index i (Bootstrap'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Bootstrap (Message.Mut s))) where
    setIndex (Bootstrap'newtype_ elt) i (Bootstrap'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Bootstrap'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_Bootstrap'questionId :: ((Untyped.ReadCtx m msg)) => (Bootstrap msg) -> (m Std_.Word32)
get_Bootstrap'questionId (Bootstrap'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Bootstrap'questionId :: ((Untyped.RWCtx m s)) => (Bootstrap (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Bootstrap'questionId (Bootstrap'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
get_Bootstrap'deprecatedObjectId :: ((Untyped.ReadCtx m msg)
                                    ,(Classes.FromPtr msg (Std_.Maybe (Untyped.Ptr msg)))) => (Bootstrap msg) -> (m (Std_.Maybe (Untyped.Ptr msg)))
get_Bootstrap'deprecatedObjectId (Bootstrap'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Bootstrap'deprecatedObjectId :: ((Untyped.RWCtx m s)
                                    ,(Classes.ToPtr s (Std_.Maybe (Untyped.Ptr (Message.Mut s))))) => (Bootstrap (Message.Mut s)) -> (Std_.Maybe (Untyped.Ptr (Message.Mut s))) -> (m ())
set_Bootstrap'deprecatedObjectId (Bootstrap'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Bootstrap'deprecatedObjectId :: ((Untyped.ReadCtx m msg)) => (Bootstrap msg) -> (m Std_.Bool)
has_Bootstrap'deprecatedObjectId (Bootstrap'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
newtype Call msg
    = Call'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Call msg)) where
    fromStruct struct = (Std_.pure (Call'newtype_ struct))
instance (Classes.ToStruct msg (Call msg)) where
    toStruct (Call'newtype_ struct) = struct
instance (Untyped.HasMessage (Call mut) mut) where
    message (Call'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Call mut) mut) where
    messageDefault msg = (Call'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Call msg)) where
    fromPtr msg ptr = (Call'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Call (Message.Mut s))) where
    toPtr msg (Call'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Call (Message.Mut s))) where
    new msg = (Call'newtype_ <$> (Untyped.allocStruct msg 3 3))
instance (Basics.ListElem mut (Call mut)) where
    newtype List mut (Call mut)
        = Call'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Call'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Call'List_ l) = (Untyped.ListStruct l)
    length (Call'List_ l) = (Untyped.length l)
    index i (Call'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Call (Message.Mut s))) where
    setIndex (Call'newtype_ elt) i (Call'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Call'List_ <$> (Untyped.allocCompositeList msg 3 3 len))
get_Call'questionId :: ((Untyped.ReadCtx m msg)) => (Call msg) -> (m Std_.Word32)
get_Call'questionId (Call'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Call'questionId :: ((Untyped.RWCtx m s)) => (Call (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Call'questionId (Call'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
get_Call'target :: ((Untyped.ReadCtx m msg)
                   ,(Classes.FromPtr msg (MessageTarget msg))) => (Call msg) -> (m (MessageTarget msg))
get_Call'target (Call'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Call'target :: ((Untyped.RWCtx m s)
                   ,(Classes.ToPtr s (MessageTarget (Message.Mut s)))) => (Call (Message.Mut s)) -> (MessageTarget (Message.Mut s)) -> (m ())
set_Call'target (Call'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Call'target :: ((Untyped.ReadCtx m msg)) => (Call msg) -> (m Std_.Bool)
has_Call'target (Call'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Call'target :: ((Untyped.RWCtx m s)) => (Call (Message.Mut s)) -> (m (MessageTarget (Message.Mut s)))
new_Call'target struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Call'target struct result)
    (Std_.pure result)
    )
get_Call'interfaceId :: ((Untyped.ReadCtx m msg)) => (Call msg) -> (m Std_.Word64)
get_Call'interfaceId (Call'newtype_ struct) = (GenHelpers.getWordField struct 1 0 0)
set_Call'interfaceId :: ((Untyped.RWCtx m s)) => (Call (Message.Mut s)) -> Std_.Word64 -> (m ())
set_Call'interfaceId (Call'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word64) 1 0 0)
get_Call'methodId :: ((Untyped.ReadCtx m msg)) => (Call msg) -> (m Std_.Word16)
get_Call'methodId (Call'newtype_ struct) = (GenHelpers.getWordField struct 0 32 0)
set_Call'methodId :: ((Untyped.RWCtx m s)) => (Call (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Call'methodId (Call'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 32 0)
get_Call'params :: ((Untyped.ReadCtx m msg)
                   ,(Classes.FromPtr msg (Payload msg))) => (Call msg) -> (m (Payload msg))
get_Call'params (Call'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Call'params :: ((Untyped.RWCtx m s)
                   ,(Classes.ToPtr s (Payload (Message.Mut s)))) => (Call (Message.Mut s)) -> (Payload (Message.Mut s)) -> (m ())
set_Call'params (Call'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Call'params :: ((Untyped.ReadCtx m msg)) => (Call msg) -> (m Std_.Bool)
has_Call'params (Call'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Call'params :: ((Untyped.RWCtx m s)) => (Call (Message.Mut s)) -> (m (Payload (Message.Mut s)))
new_Call'params struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Call'params struct result)
    (Std_.pure result)
    )
get_Call'sendResultsTo :: ((Untyped.ReadCtx m msg)
                          ,(Classes.FromStruct msg (Call'sendResultsTo msg))) => (Call msg) -> (m (Call'sendResultsTo msg))
get_Call'sendResultsTo (Call'newtype_ struct) = (Classes.fromStruct struct)
get_Call'allowThirdPartyTailCall :: ((Untyped.ReadCtx m msg)) => (Call msg) -> (m Std_.Bool)
get_Call'allowThirdPartyTailCall (Call'newtype_ struct) = (GenHelpers.getWordField struct 2 0 0)
set_Call'allowThirdPartyTailCall :: ((Untyped.RWCtx m s)) => (Call (Message.Mut s)) -> Std_.Bool -> (m ())
set_Call'allowThirdPartyTailCall (Call'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 2 0 0)
newtype Call'sendResultsTo msg
    = Call'sendResultsTo'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Call'sendResultsTo msg)) where
    fromStruct struct = (Std_.pure (Call'sendResultsTo'newtype_ struct))
instance (Classes.ToStruct msg (Call'sendResultsTo msg)) where
    toStruct (Call'sendResultsTo'newtype_ struct) = struct
instance (Untyped.HasMessage (Call'sendResultsTo mut) mut) where
    message (Call'sendResultsTo'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Call'sendResultsTo mut) mut) where
    messageDefault msg = (Call'sendResultsTo'newtype_ <$> (Untyped.messageDefault msg))
data Call'sendResultsTo' (mut :: Message.Mutability)
    = Call'sendResultsTo'caller 
    | Call'sendResultsTo'yourself 
    | Call'sendResultsTo'thirdParty (Std_.Maybe (Untyped.Ptr mut))
    | Call'sendResultsTo'unknown' Std_.Word16
instance (Classes.FromStruct mut (Call'sendResultsTo' mut)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 3)
        case tag of
            0 ->
                (Std_.pure Call'sendResultsTo'caller)
            1 ->
                (Std_.pure Call'sendResultsTo'yourself)
            2 ->
                (Call'sendResultsTo'thirdParty <$> (do
                    ptr <- (Untyped.getPtr 2 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            _ ->
                (Std_.pure (Call'sendResultsTo'unknown' (Std_.fromIntegral tag)))
        )
get_Call'sendResultsTo' :: ((Untyped.ReadCtx m msg)
                           ,(Classes.FromStruct msg (Call'sendResultsTo' msg))) => (Call'sendResultsTo msg) -> (m (Call'sendResultsTo' msg))
get_Call'sendResultsTo' (Call'sendResultsTo'newtype_ struct) = (Classes.fromStruct struct)
set_Call'sendResultsTo'caller :: ((Untyped.RWCtx m s)) => (Call'sendResultsTo (Message.Mut s)) -> (m ())
set_Call'sendResultsTo'caller (Call'sendResultsTo'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 48 0)
    (Std_.pure ())
    )
set_Call'sendResultsTo'yourself :: ((Untyped.RWCtx m s)) => (Call'sendResultsTo (Message.Mut s)) -> (m ())
set_Call'sendResultsTo'yourself (Call'sendResultsTo'newtype_ struct) = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 48 0)
    (Std_.pure ())
    )
set_Call'sendResultsTo'thirdParty :: ((Untyped.RWCtx m s)
                                     ,(Classes.ToPtr s (Std_.Maybe (Untyped.Ptr (Message.Mut s))))) => (Call'sendResultsTo (Message.Mut s)) -> (Std_.Maybe (Untyped.Ptr (Message.Mut s))) -> (m ())
set_Call'sendResultsTo'thirdParty (Call'sendResultsTo'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 0 48 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 2 struct)
        )
    )
set_Call'sendResultsTo'unknown' :: ((Untyped.RWCtx m s)) => (Call'sendResultsTo (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Call'sendResultsTo'unknown' (Call'sendResultsTo'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 48 0)
newtype Return msg
    = Return'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Return msg)) where
    fromStruct struct = (Std_.pure (Return'newtype_ struct))
instance (Classes.ToStruct msg (Return msg)) where
    toStruct (Return'newtype_ struct) = struct
instance (Untyped.HasMessage (Return mut) mut) where
    message (Return'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Return mut) mut) where
    messageDefault msg = (Return'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Return msg)) where
    fromPtr msg ptr = (Return'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Return (Message.Mut s))) where
    toPtr msg (Return'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Return (Message.Mut s))) where
    new msg = (Return'newtype_ <$> (Untyped.allocStruct msg 2 1))
instance (Basics.ListElem mut (Return mut)) where
    newtype List mut (Return mut)
        = Return'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Return'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Return'List_ l) = (Untyped.ListStruct l)
    length (Return'List_ l) = (Untyped.length l)
    index i (Return'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Return (Message.Mut s))) where
    setIndex (Return'newtype_ elt) i (Return'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Return'List_ <$> (Untyped.allocCompositeList msg 2 1 len))
get_Return'answerId :: ((Untyped.ReadCtx m msg)) => (Return msg) -> (m Std_.Word32)
get_Return'answerId (Return'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Return'answerId :: ((Untyped.RWCtx m s)) => (Return (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Return'answerId (Return'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
get_Return'releaseParamCaps :: ((Untyped.ReadCtx m msg)) => (Return msg) -> (m Std_.Bool)
get_Return'releaseParamCaps (Return'newtype_ struct) = (GenHelpers.getWordField struct 0 32 1)
set_Return'releaseParamCaps :: ((Untyped.RWCtx m s)) => (Return (Message.Mut s)) -> Std_.Bool -> (m ())
set_Return'releaseParamCaps (Return'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 0 32 1)
data Return' (mut :: Message.Mutability)
    = Return'results (Payload mut)
    | Return'exception (Exception mut)
    | Return'canceled 
    | Return'resultsSentElsewhere 
    | Return'takeFromOtherQuestion Std_.Word32
    | Return'acceptFromThirdParty (Std_.Maybe (Untyped.Ptr mut))
    | Return'unknown' Std_.Word16
instance (Classes.FromStruct mut (Return' mut)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 3)
        case tag of
            0 ->
                (Return'results <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            1 ->
                (Return'exception <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            2 ->
                (Std_.pure Return'canceled)
            3 ->
                (Std_.pure Return'resultsSentElsewhere)
            4 ->
                (Return'takeFromOtherQuestion <$> (GenHelpers.getWordField struct 1 0 0))
            5 ->
                (Return'acceptFromThirdParty <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            _ ->
                (Std_.pure (Return'unknown' (Std_.fromIntegral tag)))
        )
get_Return' :: ((Untyped.ReadCtx m msg)
               ,(Classes.FromStruct msg (Return' msg))) => (Return msg) -> (m (Return' msg))
get_Return' (Return'newtype_ struct) = (Classes.fromStruct struct)
set_Return'results :: ((Untyped.RWCtx m s)
                      ,(Classes.ToPtr s (Payload (Message.Mut s)))) => (Return (Message.Mut s)) -> (Payload (Message.Mut s)) -> (m ())
set_Return'results (Return'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 48 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Return'exception :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Exception (Message.Mut s)))) => (Return (Message.Mut s)) -> (Exception (Message.Mut s)) -> (m ())
set_Return'exception (Return'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 48 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Return'canceled :: ((Untyped.RWCtx m s)) => (Return (Message.Mut s)) -> (m ())
set_Return'canceled (Return'newtype_ struct) = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 0 48 0)
    (Std_.pure ())
    )
set_Return'resultsSentElsewhere :: ((Untyped.RWCtx m s)) => (Return (Message.Mut s)) -> (m ())
set_Return'resultsSentElsewhere (Return'newtype_ struct) = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 0 48 0)
    (Std_.pure ())
    )
set_Return'takeFromOtherQuestion :: ((Untyped.RWCtx m s)) => (Return (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Return'takeFromOtherQuestion (Return'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (4 :: Std_.Word16) 0 48 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 1 0 0)
    )
set_Return'acceptFromThirdParty :: ((Untyped.RWCtx m s)
                                   ,(Classes.ToPtr s (Std_.Maybe (Untyped.Ptr (Message.Mut s))))) => (Return (Message.Mut s)) -> (Std_.Maybe (Untyped.Ptr (Message.Mut s))) -> (m ())
set_Return'acceptFromThirdParty (Return'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (5 :: Std_.Word16) 0 48 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Return'unknown' :: ((Untyped.RWCtx m s)) => (Return (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Return'unknown' (Return'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 48 0)
newtype Finish msg
    = Finish'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Finish msg)) where
    fromStruct struct = (Std_.pure (Finish'newtype_ struct))
instance (Classes.ToStruct msg (Finish msg)) where
    toStruct (Finish'newtype_ struct) = struct
instance (Untyped.HasMessage (Finish mut) mut) where
    message (Finish'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Finish mut) mut) where
    messageDefault msg = (Finish'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Finish msg)) where
    fromPtr msg ptr = (Finish'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Finish (Message.Mut s))) where
    toPtr msg (Finish'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Finish (Message.Mut s))) where
    new msg = (Finish'newtype_ <$> (Untyped.allocStruct msg 1 0))
instance (Basics.ListElem mut (Finish mut)) where
    newtype List mut (Finish mut)
        = Finish'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Finish'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Finish'List_ l) = (Untyped.ListStruct l)
    length (Finish'List_ l) = (Untyped.length l)
    index i (Finish'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Finish (Message.Mut s))) where
    setIndex (Finish'newtype_ elt) i (Finish'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Finish'List_ <$> (Untyped.allocCompositeList msg 1 0 len))
get_Finish'questionId :: ((Untyped.ReadCtx m msg)) => (Finish msg) -> (m Std_.Word32)
get_Finish'questionId (Finish'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Finish'questionId :: ((Untyped.RWCtx m s)) => (Finish (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Finish'questionId (Finish'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
get_Finish'releaseResultCaps :: ((Untyped.ReadCtx m msg)) => (Finish msg) -> (m Std_.Bool)
get_Finish'releaseResultCaps (Finish'newtype_ struct) = (GenHelpers.getWordField struct 0 32 1)
set_Finish'releaseResultCaps :: ((Untyped.RWCtx m s)) => (Finish (Message.Mut s)) -> Std_.Bool -> (m ())
set_Finish'releaseResultCaps (Finish'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 0 32 1)
newtype Resolve msg
    = Resolve'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Resolve msg)) where
    fromStruct struct = (Std_.pure (Resolve'newtype_ struct))
instance (Classes.ToStruct msg (Resolve msg)) where
    toStruct (Resolve'newtype_ struct) = struct
instance (Untyped.HasMessage (Resolve mut) mut) where
    message (Resolve'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Resolve mut) mut) where
    messageDefault msg = (Resolve'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Resolve msg)) where
    fromPtr msg ptr = (Resolve'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Resolve (Message.Mut s))) where
    toPtr msg (Resolve'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Resolve (Message.Mut s))) where
    new msg = (Resolve'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem mut (Resolve mut)) where
    newtype List mut (Resolve mut)
        = Resolve'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Resolve'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Resolve'List_ l) = (Untyped.ListStruct l)
    length (Resolve'List_ l) = (Untyped.length l)
    index i (Resolve'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Resolve (Message.Mut s))) where
    setIndex (Resolve'newtype_ elt) i (Resolve'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Resolve'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_Resolve'promiseId :: ((Untyped.ReadCtx m msg)) => (Resolve msg) -> (m Std_.Word32)
get_Resolve'promiseId (Resolve'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Resolve'promiseId :: ((Untyped.RWCtx m s)) => (Resolve (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Resolve'promiseId (Resolve'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
data Resolve' (mut :: Message.Mutability)
    = Resolve'cap (CapDescriptor mut)
    | Resolve'exception (Exception mut)
    | Resolve'unknown' Std_.Word16
instance (Classes.FromStruct mut (Resolve' mut)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 2)
        case tag of
            0 ->
                (Resolve'cap <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            1 ->
                (Resolve'exception <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            _ ->
                (Std_.pure (Resolve'unknown' (Std_.fromIntegral tag)))
        )
get_Resolve' :: ((Untyped.ReadCtx m msg)
                ,(Classes.FromStruct msg (Resolve' msg))) => (Resolve msg) -> (m (Resolve' msg))
get_Resolve' (Resolve'newtype_ struct) = (Classes.fromStruct struct)
set_Resolve'cap :: ((Untyped.RWCtx m s)
                   ,(Classes.ToPtr s (CapDescriptor (Message.Mut s)))) => (Resolve (Message.Mut s)) -> (CapDescriptor (Message.Mut s)) -> (m ())
set_Resolve'cap (Resolve'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 32 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Resolve'exception :: ((Untyped.RWCtx m s)
                         ,(Classes.ToPtr s (Exception (Message.Mut s)))) => (Resolve (Message.Mut s)) -> (Exception (Message.Mut s)) -> (m ())
set_Resolve'exception (Resolve'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 32 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_Resolve'unknown' :: ((Untyped.RWCtx m s)) => (Resolve (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Resolve'unknown' (Resolve'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 32 0)
newtype Release msg
    = Release'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Release msg)) where
    fromStruct struct = (Std_.pure (Release'newtype_ struct))
instance (Classes.ToStruct msg (Release msg)) where
    toStruct (Release'newtype_ struct) = struct
instance (Untyped.HasMessage (Release mut) mut) where
    message (Release'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Release mut) mut) where
    messageDefault msg = (Release'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Release msg)) where
    fromPtr msg ptr = (Release'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Release (Message.Mut s))) where
    toPtr msg (Release'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Release (Message.Mut s))) where
    new msg = (Release'newtype_ <$> (Untyped.allocStruct msg 1 0))
instance (Basics.ListElem mut (Release mut)) where
    newtype List mut (Release mut)
        = Release'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Release'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Release'List_ l) = (Untyped.ListStruct l)
    length (Release'List_ l) = (Untyped.length l)
    index i (Release'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Release (Message.Mut s))) where
    setIndex (Release'newtype_ elt) i (Release'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Release'List_ <$> (Untyped.allocCompositeList msg 1 0 len))
get_Release'id :: ((Untyped.ReadCtx m msg)) => (Release msg) -> (m Std_.Word32)
get_Release'id (Release'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Release'id :: ((Untyped.RWCtx m s)) => (Release (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Release'id (Release'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
get_Release'referenceCount :: ((Untyped.ReadCtx m msg)) => (Release msg) -> (m Std_.Word32)
get_Release'referenceCount (Release'newtype_ struct) = (GenHelpers.getWordField struct 0 32 0)
set_Release'referenceCount :: ((Untyped.RWCtx m s)) => (Release (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Release'referenceCount (Release'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 32 0)
newtype Disembargo msg
    = Disembargo'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Disembargo msg)) where
    fromStruct struct = (Std_.pure (Disembargo'newtype_ struct))
instance (Classes.ToStruct msg (Disembargo msg)) where
    toStruct (Disembargo'newtype_ struct) = struct
instance (Untyped.HasMessage (Disembargo mut) mut) where
    message (Disembargo'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Disembargo mut) mut) where
    messageDefault msg = (Disembargo'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Disembargo msg)) where
    fromPtr msg ptr = (Disembargo'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Disembargo (Message.Mut s))) where
    toPtr msg (Disembargo'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Disembargo (Message.Mut s))) where
    new msg = (Disembargo'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem mut (Disembargo mut)) where
    newtype List mut (Disembargo mut)
        = Disembargo'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Disembargo'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Disembargo'List_ l) = (Untyped.ListStruct l)
    length (Disembargo'List_ l) = (Untyped.length l)
    index i (Disembargo'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Disembargo (Message.Mut s))) where
    setIndex (Disembargo'newtype_ elt) i (Disembargo'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Disembargo'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_Disembargo'target :: ((Untyped.ReadCtx m msg)
                         ,(Classes.FromPtr msg (MessageTarget msg))) => (Disembargo msg) -> (m (MessageTarget msg))
get_Disembargo'target (Disembargo'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Disembargo'target :: ((Untyped.RWCtx m s)
                         ,(Classes.ToPtr s (MessageTarget (Message.Mut s)))) => (Disembargo (Message.Mut s)) -> (MessageTarget (Message.Mut s)) -> (m ())
set_Disembargo'target (Disembargo'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Disembargo'target :: ((Untyped.ReadCtx m msg)) => (Disembargo msg) -> (m Std_.Bool)
has_Disembargo'target (Disembargo'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Disembargo'target :: ((Untyped.RWCtx m s)) => (Disembargo (Message.Mut s)) -> (m (MessageTarget (Message.Mut s)))
new_Disembargo'target struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Disembargo'target struct result)
    (Std_.pure result)
    )
get_Disembargo'context :: ((Untyped.ReadCtx m msg)
                          ,(Classes.FromStruct msg (Disembargo'context msg))) => (Disembargo msg) -> (m (Disembargo'context msg))
get_Disembargo'context (Disembargo'newtype_ struct) = (Classes.fromStruct struct)
newtype Disembargo'context msg
    = Disembargo'context'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Disembargo'context msg)) where
    fromStruct struct = (Std_.pure (Disembargo'context'newtype_ struct))
instance (Classes.ToStruct msg (Disembargo'context msg)) where
    toStruct (Disembargo'context'newtype_ struct) = struct
instance (Untyped.HasMessage (Disembargo'context mut) mut) where
    message (Disembargo'context'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Disembargo'context mut) mut) where
    messageDefault msg = (Disembargo'context'newtype_ <$> (Untyped.messageDefault msg))
data Disembargo'context' (mut :: Message.Mutability)
    = Disembargo'context'senderLoopback Std_.Word32
    | Disembargo'context'receiverLoopback Std_.Word32
    | Disembargo'context'accept 
    | Disembargo'context'provide Std_.Word32
    | Disembargo'context'unknown' Std_.Word16
instance (Classes.FromStruct mut (Disembargo'context' mut)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 2)
        case tag of
            0 ->
                (Disembargo'context'senderLoopback <$> (GenHelpers.getWordField struct 0 0 0))
            1 ->
                (Disembargo'context'receiverLoopback <$> (GenHelpers.getWordField struct 0 0 0))
            2 ->
                (Std_.pure Disembargo'context'accept)
            3 ->
                (Disembargo'context'provide <$> (GenHelpers.getWordField struct 0 0 0))
            _ ->
                (Std_.pure (Disembargo'context'unknown' (Std_.fromIntegral tag)))
        )
get_Disembargo'context' :: ((Untyped.ReadCtx m msg)
                           ,(Classes.FromStruct msg (Disembargo'context' msg))) => (Disembargo'context msg) -> (m (Disembargo'context' msg))
get_Disembargo'context' (Disembargo'context'newtype_ struct) = (Classes.fromStruct struct)
set_Disembargo'context'senderLoopback :: ((Untyped.RWCtx m s)) => (Disembargo'context (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Disembargo'context'senderLoopback (Disembargo'context'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 32 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
    )
set_Disembargo'context'receiverLoopback :: ((Untyped.RWCtx m s)) => (Disembargo'context (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Disembargo'context'receiverLoopback (Disembargo'context'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 32 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
    )
set_Disembargo'context'accept :: ((Untyped.RWCtx m s)) => (Disembargo'context (Message.Mut s)) -> (m ())
set_Disembargo'context'accept (Disembargo'context'newtype_ struct) = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 0 32 0)
    (Std_.pure ())
    )
set_Disembargo'context'provide :: ((Untyped.RWCtx m s)) => (Disembargo'context (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Disembargo'context'provide (Disembargo'context'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 0 32 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
    )
set_Disembargo'context'unknown' :: ((Untyped.RWCtx m s)) => (Disembargo'context (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Disembargo'context'unknown' (Disembargo'context'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 32 0)
newtype Provide msg
    = Provide'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Provide msg)) where
    fromStruct struct = (Std_.pure (Provide'newtype_ struct))
instance (Classes.ToStruct msg (Provide msg)) where
    toStruct (Provide'newtype_ struct) = struct
instance (Untyped.HasMessage (Provide mut) mut) where
    message (Provide'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Provide mut) mut) where
    messageDefault msg = (Provide'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Provide msg)) where
    fromPtr msg ptr = (Provide'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Provide (Message.Mut s))) where
    toPtr msg (Provide'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Provide (Message.Mut s))) where
    new msg = (Provide'newtype_ <$> (Untyped.allocStruct msg 1 2))
instance (Basics.ListElem mut (Provide mut)) where
    newtype List mut (Provide mut)
        = Provide'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Provide'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Provide'List_ l) = (Untyped.ListStruct l)
    length (Provide'List_ l) = (Untyped.length l)
    index i (Provide'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Provide (Message.Mut s))) where
    setIndex (Provide'newtype_ elt) i (Provide'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Provide'List_ <$> (Untyped.allocCompositeList msg 1 2 len))
get_Provide'questionId :: ((Untyped.ReadCtx m msg)) => (Provide msg) -> (m Std_.Word32)
get_Provide'questionId (Provide'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Provide'questionId :: ((Untyped.RWCtx m s)) => (Provide (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Provide'questionId (Provide'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
get_Provide'target :: ((Untyped.ReadCtx m msg)
                      ,(Classes.FromPtr msg (MessageTarget msg))) => (Provide msg) -> (m (MessageTarget msg))
get_Provide'target (Provide'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Provide'target :: ((Untyped.RWCtx m s)
                      ,(Classes.ToPtr s (MessageTarget (Message.Mut s)))) => (Provide (Message.Mut s)) -> (MessageTarget (Message.Mut s)) -> (m ())
set_Provide'target (Provide'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Provide'target :: ((Untyped.ReadCtx m msg)) => (Provide msg) -> (m Std_.Bool)
has_Provide'target (Provide'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Provide'target :: ((Untyped.RWCtx m s)) => (Provide (Message.Mut s)) -> (m (MessageTarget (Message.Mut s)))
new_Provide'target struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Provide'target struct result)
    (Std_.pure result)
    )
get_Provide'recipient :: ((Untyped.ReadCtx m msg)
                         ,(Classes.FromPtr msg (Std_.Maybe (Untyped.Ptr msg)))) => (Provide msg) -> (m (Std_.Maybe (Untyped.Ptr msg)))
get_Provide'recipient (Provide'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Provide'recipient :: ((Untyped.RWCtx m s)
                         ,(Classes.ToPtr s (Std_.Maybe (Untyped.Ptr (Message.Mut s))))) => (Provide (Message.Mut s)) -> (Std_.Maybe (Untyped.Ptr (Message.Mut s))) -> (m ())
set_Provide'recipient (Provide'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Provide'recipient :: ((Untyped.ReadCtx m msg)) => (Provide msg) -> (m Std_.Bool)
has_Provide'recipient (Provide'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
newtype Accept msg
    = Accept'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Accept msg)) where
    fromStruct struct = (Std_.pure (Accept'newtype_ struct))
instance (Classes.ToStruct msg (Accept msg)) where
    toStruct (Accept'newtype_ struct) = struct
instance (Untyped.HasMessage (Accept mut) mut) where
    message (Accept'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Accept mut) mut) where
    messageDefault msg = (Accept'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Accept msg)) where
    fromPtr msg ptr = (Accept'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Accept (Message.Mut s))) where
    toPtr msg (Accept'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Accept (Message.Mut s))) where
    new msg = (Accept'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem mut (Accept mut)) where
    newtype List mut (Accept mut)
        = Accept'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Accept'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Accept'List_ l) = (Untyped.ListStruct l)
    length (Accept'List_ l) = (Untyped.length l)
    index i (Accept'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Accept (Message.Mut s))) where
    setIndex (Accept'newtype_ elt) i (Accept'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Accept'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_Accept'questionId :: ((Untyped.ReadCtx m msg)) => (Accept msg) -> (m Std_.Word32)
get_Accept'questionId (Accept'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Accept'questionId :: ((Untyped.RWCtx m s)) => (Accept (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Accept'questionId (Accept'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
get_Accept'provision :: ((Untyped.ReadCtx m msg)
                        ,(Classes.FromPtr msg (Std_.Maybe (Untyped.Ptr msg)))) => (Accept msg) -> (m (Std_.Maybe (Untyped.Ptr msg)))
get_Accept'provision (Accept'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Accept'provision :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Std_.Maybe (Untyped.Ptr (Message.Mut s))))) => (Accept (Message.Mut s)) -> (Std_.Maybe (Untyped.Ptr (Message.Mut s))) -> (m ())
set_Accept'provision (Accept'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Accept'provision :: ((Untyped.ReadCtx m msg)) => (Accept msg) -> (m Std_.Bool)
has_Accept'provision (Accept'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
get_Accept'embargo :: ((Untyped.ReadCtx m msg)) => (Accept msg) -> (m Std_.Bool)
get_Accept'embargo (Accept'newtype_ struct) = (GenHelpers.getWordField struct 0 32 0)
set_Accept'embargo :: ((Untyped.RWCtx m s)) => (Accept (Message.Mut s)) -> Std_.Bool -> (m ())
set_Accept'embargo (Accept'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 0 32 0)
newtype Join msg
    = Join'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Join msg)) where
    fromStruct struct = (Std_.pure (Join'newtype_ struct))
instance (Classes.ToStruct msg (Join msg)) where
    toStruct (Join'newtype_ struct) = struct
instance (Untyped.HasMessage (Join mut) mut) where
    message (Join'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Join mut) mut) where
    messageDefault msg = (Join'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Join msg)) where
    fromPtr msg ptr = (Join'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Join (Message.Mut s))) where
    toPtr msg (Join'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Join (Message.Mut s))) where
    new msg = (Join'newtype_ <$> (Untyped.allocStruct msg 1 2))
instance (Basics.ListElem mut (Join mut)) where
    newtype List mut (Join mut)
        = Join'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Join'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Join'List_ l) = (Untyped.ListStruct l)
    length (Join'List_ l) = (Untyped.length l)
    index i (Join'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Join (Message.Mut s))) where
    setIndex (Join'newtype_ elt) i (Join'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Join'List_ <$> (Untyped.allocCompositeList msg 1 2 len))
get_Join'questionId :: ((Untyped.ReadCtx m msg)) => (Join msg) -> (m Std_.Word32)
get_Join'questionId (Join'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Join'questionId :: ((Untyped.RWCtx m s)) => (Join (Message.Mut s)) -> Std_.Word32 -> (m ())
set_Join'questionId (Join'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
get_Join'target :: ((Untyped.ReadCtx m msg)
                   ,(Classes.FromPtr msg (MessageTarget msg))) => (Join msg) -> (m (MessageTarget msg))
get_Join'target (Join'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Join'target :: ((Untyped.RWCtx m s)
                   ,(Classes.ToPtr s (MessageTarget (Message.Mut s)))) => (Join (Message.Mut s)) -> (MessageTarget (Message.Mut s)) -> (m ())
set_Join'target (Join'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Join'target :: ((Untyped.ReadCtx m msg)) => (Join msg) -> (m Std_.Bool)
has_Join'target (Join'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Join'target :: ((Untyped.RWCtx m s)) => (Join (Message.Mut s)) -> (m (MessageTarget (Message.Mut s)))
new_Join'target struct = (do
    result <- (Classes.new (Untyped.message struct))
    (set_Join'target struct result)
    (Std_.pure result)
    )
get_Join'keyPart :: ((Untyped.ReadCtx m msg)
                    ,(Classes.FromPtr msg (Std_.Maybe (Untyped.Ptr msg)))) => (Join msg) -> (m (Std_.Maybe (Untyped.Ptr msg)))
get_Join'keyPart (Join'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Join'keyPart :: ((Untyped.RWCtx m s)
                    ,(Classes.ToPtr s (Std_.Maybe (Untyped.Ptr (Message.Mut s))))) => (Join (Message.Mut s)) -> (Std_.Maybe (Untyped.Ptr (Message.Mut s))) -> (m ())
set_Join'keyPart (Join'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Join'keyPart :: ((Untyped.ReadCtx m msg)) => (Join msg) -> (m Std_.Bool)
has_Join'keyPart (Join'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
newtype MessageTarget msg
    = MessageTarget'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (MessageTarget msg)) where
    fromStruct struct = (Std_.pure (MessageTarget'newtype_ struct))
instance (Classes.ToStruct msg (MessageTarget msg)) where
    toStruct (MessageTarget'newtype_ struct) = struct
instance (Untyped.HasMessage (MessageTarget mut) mut) where
    message (MessageTarget'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (MessageTarget mut) mut) where
    messageDefault msg = (MessageTarget'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (MessageTarget msg)) where
    fromPtr msg ptr = (MessageTarget'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (MessageTarget (Message.Mut s))) where
    toPtr msg (MessageTarget'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (MessageTarget (Message.Mut s))) where
    new msg = (MessageTarget'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem mut (MessageTarget mut)) where
    newtype List mut (MessageTarget mut)
        = MessageTarget'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (MessageTarget'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (MessageTarget'List_ l) = (Untyped.ListStruct l)
    length (MessageTarget'List_ l) = (Untyped.length l)
    index i (MessageTarget'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (MessageTarget (Message.Mut s))) where
    setIndex (MessageTarget'newtype_ elt) i (MessageTarget'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (MessageTarget'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
data MessageTarget' (mut :: Message.Mutability)
    = MessageTarget'importedCap Std_.Word32
    | MessageTarget'promisedAnswer (PromisedAnswer mut)
    | MessageTarget'unknown' Std_.Word16
instance (Classes.FromStruct mut (MessageTarget' mut)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 2)
        case tag of
            0 ->
                (MessageTarget'importedCap <$> (GenHelpers.getWordField struct 0 0 0))
            1 ->
                (MessageTarget'promisedAnswer <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            _ ->
                (Std_.pure (MessageTarget'unknown' (Std_.fromIntegral tag)))
        )
get_MessageTarget' :: ((Untyped.ReadCtx m msg)
                      ,(Classes.FromStruct msg (MessageTarget' msg))) => (MessageTarget msg) -> (m (MessageTarget' msg))
get_MessageTarget' (MessageTarget'newtype_ struct) = (Classes.fromStruct struct)
set_MessageTarget'importedCap :: ((Untyped.RWCtx m s)) => (MessageTarget (Message.Mut s)) -> Std_.Word32 -> (m ())
set_MessageTarget'importedCap (MessageTarget'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 32 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
    )
set_MessageTarget'promisedAnswer :: ((Untyped.RWCtx m s)
                                    ,(Classes.ToPtr s (PromisedAnswer (Message.Mut s)))) => (MessageTarget (Message.Mut s)) -> (PromisedAnswer (Message.Mut s)) -> (m ())
set_MessageTarget'promisedAnswer (MessageTarget'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 32 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_MessageTarget'unknown' :: ((Untyped.RWCtx m s)) => (MessageTarget (Message.Mut s)) -> Std_.Word16 -> (m ())
set_MessageTarget'unknown' (MessageTarget'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 32 0)
newtype Payload msg
    = Payload'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Payload msg)) where
    fromStruct struct = (Std_.pure (Payload'newtype_ struct))
instance (Classes.ToStruct msg (Payload msg)) where
    toStruct (Payload'newtype_ struct) = struct
instance (Untyped.HasMessage (Payload mut) mut) where
    message (Payload'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Payload mut) mut) where
    messageDefault msg = (Payload'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Payload msg)) where
    fromPtr msg ptr = (Payload'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Payload (Message.Mut s))) where
    toPtr msg (Payload'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Payload (Message.Mut s))) where
    new msg = (Payload'newtype_ <$> (Untyped.allocStruct msg 0 2))
instance (Basics.ListElem mut (Payload mut)) where
    newtype List mut (Payload mut)
        = Payload'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Payload'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Payload'List_ l) = (Untyped.ListStruct l)
    length (Payload'List_ l) = (Untyped.length l)
    index i (Payload'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Payload (Message.Mut s))) where
    setIndex (Payload'newtype_ elt) i (Payload'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Payload'List_ <$> (Untyped.allocCompositeList msg 0 2 len))
get_Payload'content :: ((Untyped.ReadCtx m msg)
                       ,(Classes.FromPtr msg (Std_.Maybe (Untyped.Ptr msg)))) => (Payload msg) -> (m (Std_.Maybe (Untyped.Ptr msg)))
get_Payload'content (Payload'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Payload'content :: ((Untyped.RWCtx m s)
                       ,(Classes.ToPtr s (Std_.Maybe (Untyped.Ptr (Message.Mut s))))) => (Payload (Message.Mut s)) -> (Std_.Maybe (Untyped.Ptr (Message.Mut s))) -> (m ())
set_Payload'content (Payload'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Payload'content :: ((Untyped.ReadCtx m msg)) => (Payload msg) -> (m Std_.Bool)
has_Payload'content (Payload'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
get_Payload'capTable :: ((Untyped.ReadCtx m msg)
                        ,(Classes.FromPtr msg (Basics.List msg (CapDescriptor msg)))) => (Payload msg) -> (m (Basics.List msg (CapDescriptor msg)))
get_Payload'capTable (Payload'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 1 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Payload'capTable :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Basics.List (Message.Mut s) (CapDescriptor (Message.Mut s))))) => (Payload (Message.Mut s)) -> (Basics.List (Message.Mut s) (CapDescriptor (Message.Mut s))) -> (m ())
set_Payload'capTable (Payload'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 1 struct)
    )
has_Payload'capTable :: ((Untyped.ReadCtx m msg)) => (Payload msg) -> (m Std_.Bool)
has_Payload'capTable (Payload'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 1 struct))
new_Payload'capTable :: ((Untyped.RWCtx m s)) => Std_.Int -> (Payload (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (CapDescriptor (Message.Mut s))))
new_Payload'capTable len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_Payload'capTable struct result)
    (Std_.pure result)
    )
newtype CapDescriptor msg
    = CapDescriptor'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (CapDescriptor msg)) where
    fromStruct struct = (Std_.pure (CapDescriptor'newtype_ struct))
instance (Classes.ToStruct msg (CapDescriptor msg)) where
    toStruct (CapDescriptor'newtype_ struct) = struct
instance (Untyped.HasMessage (CapDescriptor mut) mut) where
    message (CapDescriptor'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (CapDescriptor mut) mut) where
    messageDefault msg = (CapDescriptor'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (CapDescriptor msg)) where
    fromPtr msg ptr = (CapDescriptor'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (CapDescriptor (Message.Mut s))) where
    toPtr msg (CapDescriptor'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (CapDescriptor (Message.Mut s))) where
    new msg = (CapDescriptor'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem mut (CapDescriptor mut)) where
    newtype List mut (CapDescriptor mut)
        = CapDescriptor'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (CapDescriptor'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (CapDescriptor'List_ l) = (Untyped.ListStruct l)
    length (CapDescriptor'List_ l) = (Untyped.length l)
    index i (CapDescriptor'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (CapDescriptor (Message.Mut s))) where
    setIndex (CapDescriptor'newtype_ elt) i (CapDescriptor'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (CapDescriptor'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_CapDescriptor'attachedFd :: ((Untyped.ReadCtx m msg)) => (CapDescriptor msg) -> (m Std_.Word8)
get_CapDescriptor'attachedFd (CapDescriptor'newtype_ struct) = (GenHelpers.getWordField struct 0 16 255)
set_CapDescriptor'attachedFd :: ((Untyped.RWCtx m s)) => (CapDescriptor (Message.Mut s)) -> Std_.Word8 -> (m ())
set_CapDescriptor'attachedFd (CapDescriptor'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word8) 0 16 255)
data CapDescriptor' (mut :: Message.Mutability)
    = CapDescriptor'none 
    | CapDescriptor'senderHosted Std_.Word32
    | CapDescriptor'senderPromise Std_.Word32
    | CapDescriptor'receiverHosted Std_.Word32
    | CapDescriptor'receiverAnswer (PromisedAnswer mut)
    | CapDescriptor'thirdPartyHosted (ThirdPartyCapDescriptor mut)
    | CapDescriptor'unknown' Std_.Word16
instance (Classes.FromStruct mut (CapDescriptor' mut)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 0)
        case tag of
            0 ->
                (Std_.pure CapDescriptor'none)
            1 ->
                (CapDescriptor'senderHosted <$> (GenHelpers.getWordField struct 0 32 0))
            2 ->
                (CapDescriptor'senderPromise <$> (GenHelpers.getWordField struct 0 32 0))
            3 ->
                (CapDescriptor'receiverHosted <$> (GenHelpers.getWordField struct 0 32 0))
            4 ->
                (CapDescriptor'receiverAnswer <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            5 ->
                (CapDescriptor'thirdPartyHosted <$> (do
                    ptr <- (Untyped.getPtr 0 struct)
                    (Classes.fromPtr (Untyped.message struct) ptr)
                    ))
            _ ->
                (Std_.pure (CapDescriptor'unknown' (Std_.fromIntegral tag)))
        )
get_CapDescriptor' :: ((Untyped.ReadCtx m msg)
                      ,(Classes.FromStruct msg (CapDescriptor' msg))) => (CapDescriptor msg) -> (m (CapDescriptor' msg))
get_CapDescriptor' (CapDescriptor'newtype_ struct) = (Classes.fromStruct struct)
set_CapDescriptor'none :: ((Untyped.RWCtx m s)) => (CapDescriptor (Message.Mut s)) -> (m ())
set_CapDescriptor'none (CapDescriptor'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_CapDescriptor'senderHosted :: ((Untyped.RWCtx m s)) => (CapDescriptor (Message.Mut s)) -> Std_.Word32 -> (m ())
set_CapDescriptor'senderHosted (CapDescriptor'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 32 0)
    )
set_CapDescriptor'senderPromise :: ((Untyped.RWCtx m s)) => (CapDescriptor (Message.Mut s)) -> Std_.Word32 -> (m ())
set_CapDescriptor'senderPromise (CapDescriptor'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (2 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 32 0)
    )
set_CapDescriptor'receiverHosted :: ((Untyped.RWCtx m s)) => (CapDescriptor (Message.Mut s)) -> Std_.Word32 -> (m ())
set_CapDescriptor'receiverHosted (CapDescriptor'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (3 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 32 0)
    )
set_CapDescriptor'receiverAnswer :: ((Untyped.RWCtx m s)
                                    ,(Classes.ToPtr s (PromisedAnswer (Message.Mut s)))) => (CapDescriptor (Message.Mut s)) -> (PromisedAnswer (Message.Mut s)) -> (m ())
set_CapDescriptor'receiverAnswer (CapDescriptor'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (4 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_CapDescriptor'thirdPartyHosted :: ((Untyped.RWCtx m s)
                                      ,(Classes.ToPtr s (ThirdPartyCapDescriptor (Message.Mut s)))) => (CapDescriptor (Message.Mut s)) -> (ThirdPartyCapDescriptor (Message.Mut s)) -> (m ())
set_CapDescriptor'thirdPartyHosted (CapDescriptor'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (5 :: Std_.Word16) 0 0 0)
    (do
        ptr <- (Classes.toPtr (Untyped.message struct) value)
        (Untyped.setPtr ptr 0 struct)
        )
    )
set_CapDescriptor'unknown' :: ((Untyped.RWCtx m s)) => (CapDescriptor (Message.Mut s)) -> Std_.Word16 -> (m ())
set_CapDescriptor'unknown' (CapDescriptor'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
newtype PromisedAnswer msg
    = PromisedAnswer'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (PromisedAnswer msg)) where
    fromStruct struct = (Std_.pure (PromisedAnswer'newtype_ struct))
instance (Classes.ToStruct msg (PromisedAnswer msg)) where
    toStruct (PromisedAnswer'newtype_ struct) = struct
instance (Untyped.HasMessage (PromisedAnswer mut) mut) where
    message (PromisedAnswer'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (PromisedAnswer mut) mut) where
    messageDefault msg = (PromisedAnswer'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (PromisedAnswer msg)) where
    fromPtr msg ptr = (PromisedAnswer'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (PromisedAnswer (Message.Mut s))) where
    toPtr msg (PromisedAnswer'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (PromisedAnswer (Message.Mut s))) where
    new msg = (PromisedAnswer'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem mut (PromisedAnswer mut)) where
    newtype List mut (PromisedAnswer mut)
        = PromisedAnswer'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (PromisedAnswer'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (PromisedAnswer'List_ l) = (Untyped.ListStruct l)
    length (PromisedAnswer'List_ l) = (Untyped.length l)
    index i (PromisedAnswer'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (PromisedAnswer (Message.Mut s))) where
    setIndex (PromisedAnswer'newtype_ elt) i (PromisedAnswer'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (PromisedAnswer'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_PromisedAnswer'questionId :: ((Untyped.ReadCtx m msg)) => (PromisedAnswer msg) -> (m Std_.Word32)
get_PromisedAnswer'questionId (PromisedAnswer'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_PromisedAnswer'questionId :: ((Untyped.RWCtx m s)) => (PromisedAnswer (Message.Mut s)) -> Std_.Word32 -> (m ())
set_PromisedAnswer'questionId (PromisedAnswer'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
get_PromisedAnswer'transform :: ((Untyped.ReadCtx m msg)
                                ,(Classes.FromPtr msg (Basics.List msg (PromisedAnswer'Op msg)))) => (PromisedAnswer msg) -> (m (Basics.List msg (PromisedAnswer'Op msg)))
get_PromisedAnswer'transform (PromisedAnswer'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_PromisedAnswer'transform :: ((Untyped.RWCtx m s)
                                ,(Classes.ToPtr s (Basics.List (Message.Mut s) (PromisedAnswer'Op (Message.Mut s))))) => (PromisedAnswer (Message.Mut s)) -> (Basics.List (Message.Mut s) (PromisedAnswer'Op (Message.Mut s))) -> (m ())
set_PromisedAnswer'transform (PromisedAnswer'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_PromisedAnswer'transform :: ((Untyped.ReadCtx m msg)) => (PromisedAnswer msg) -> (m Std_.Bool)
has_PromisedAnswer'transform (PromisedAnswer'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_PromisedAnswer'transform :: ((Untyped.RWCtx m s)) => Std_.Int -> (PromisedAnswer (Message.Mut s)) -> (m (Basics.List (Message.Mut s) (PromisedAnswer'Op (Message.Mut s))))
new_PromisedAnswer'transform len struct = (do
    result <- (Classes.newList (Untyped.message struct) len)
    (set_PromisedAnswer'transform struct result)
    (Std_.pure result)
    )
newtype PromisedAnswer'Op msg
    = PromisedAnswer'Op'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (PromisedAnswer'Op msg)) where
    fromStruct struct = (Std_.pure (PromisedAnswer'Op'newtype_ struct))
instance (Classes.ToStruct msg (PromisedAnswer'Op msg)) where
    toStruct (PromisedAnswer'Op'newtype_ struct) = struct
instance (Untyped.HasMessage (PromisedAnswer'Op mut) mut) where
    message (PromisedAnswer'Op'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (PromisedAnswer'Op mut) mut) where
    messageDefault msg = (PromisedAnswer'Op'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (PromisedAnswer'Op msg)) where
    fromPtr msg ptr = (PromisedAnswer'Op'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (PromisedAnswer'Op (Message.Mut s))) where
    toPtr msg (PromisedAnswer'Op'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (PromisedAnswer'Op (Message.Mut s))) where
    new msg = (PromisedAnswer'Op'newtype_ <$> (Untyped.allocStruct msg 1 0))
instance (Basics.ListElem mut (PromisedAnswer'Op mut)) where
    newtype List mut (PromisedAnswer'Op mut)
        = PromisedAnswer'Op'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (PromisedAnswer'Op'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (PromisedAnswer'Op'List_ l) = (Untyped.ListStruct l)
    length (PromisedAnswer'Op'List_ l) = (Untyped.length l)
    index i (PromisedAnswer'Op'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (PromisedAnswer'Op (Message.Mut s))) where
    setIndex (PromisedAnswer'Op'newtype_ elt) i (PromisedAnswer'Op'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (PromisedAnswer'Op'List_ <$> (Untyped.allocCompositeList msg 1 0 len))
data PromisedAnswer'Op' (mut :: Message.Mutability)
    = PromisedAnswer'Op'noop 
    | PromisedAnswer'Op'getPointerField Std_.Word16
    | PromisedAnswer'Op'unknown' Std_.Word16
instance (Classes.FromStruct mut (PromisedAnswer'Op' mut)) where
    fromStruct struct = (do
        tag <- (GenHelpers.getTag struct 0)
        case tag of
            0 ->
                (Std_.pure PromisedAnswer'Op'noop)
            1 ->
                (PromisedAnswer'Op'getPointerField <$> (GenHelpers.getWordField struct 0 16 0))
            _ ->
                (Std_.pure (PromisedAnswer'Op'unknown' (Std_.fromIntegral tag)))
        )
get_PromisedAnswer'Op' :: ((Untyped.ReadCtx m msg)
                          ,(Classes.FromStruct msg (PromisedAnswer'Op' msg))) => (PromisedAnswer'Op msg) -> (m (PromisedAnswer'Op' msg))
get_PromisedAnswer'Op' (PromisedAnswer'Op'newtype_ struct) = (Classes.fromStruct struct)
set_PromisedAnswer'Op'noop :: ((Untyped.RWCtx m s)) => (PromisedAnswer'Op (Message.Mut s)) -> (m ())
set_PromisedAnswer'Op'noop (PromisedAnswer'Op'newtype_ struct) = (do
    (GenHelpers.setWordField struct (0 :: Std_.Word16) 0 0 0)
    (Std_.pure ())
    )
set_PromisedAnswer'Op'getPointerField :: ((Untyped.RWCtx m s)) => (PromisedAnswer'Op (Message.Mut s)) -> Std_.Word16 -> (m ())
set_PromisedAnswer'Op'getPointerField (PromisedAnswer'Op'newtype_ struct) value = (do
    (GenHelpers.setWordField struct (1 :: Std_.Word16) 0 0 0)
    (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 16 0)
    )
set_PromisedAnswer'Op'unknown' :: ((Untyped.RWCtx m s)) => (PromisedAnswer'Op (Message.Mut s)) -> Std_.Word16 -> (m ())
set_PromisedAnswer'Op'unknown' (PromisedAnswer'Op'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 0 0)
newtype ThirdPartyCapDescriptor msg
    = ThirdPartyCapDescriptor'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (ThirdPartyCapDescriptor msg)) where
    fromStruct struct = (Std_.pure (ThirdPartyCapDescriptor'newtype_ struct))
instance (Classes.ToStruct msg (ThirdPartyCapDescriptor msg)) where
    toStruct (ThirdPartyCapDescriptor'newtype_ struct) = struct
instance (Untyped.HasMessage (ThirdPartyCapDescriptor mut) mut) where
    message (ThirdPartyCapDescriptor'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (ThirdPartyCapDescriptor mut) mut) where
    messageDefault msg = (ThirdPartyCapDescriptor'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (ThirdPartyCapDescriptor msg)) where
    fromPtr msg ptr = (ThirdPartyCapDescriptor'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (ThirdPartyCapDescriptor (Message.Mut s))) where
    toPtr msg (ThirdPartyCapDescriptor'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (ThirdPartyCapDescriptor (Message.Mut s))) where
    new msg = (ThirdPartyCapDescriptor'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem mut (ThirdPartyCapDescriptor mut)) where
    newtype List mut (ThirdPartyCapDescriptor mut)
        = ThirdPartyCapDescriptor'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (ThirdPartyCapDescriptor'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (ThirdPartyCapDescriptor'List_ l) = (Untyped.ListStruct l)
    length (ThirdPartyCapDescriptor'List_ l) = (Untyped.length l)
    index i (ThirdPartyCapDescriptor'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (ThirdPartyCapDescriptor (Message.Mut s))) where
    setIndex (ThirdPartyCapDescriptor'newtype_ elt) i (ThirdPartyCapDescriptor'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (ThirdPartyCapDescriptor'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_ThirdPartyCapDescriptor'id :: ((Untyped.ReadCtx m msg)
                                  ,(Classes.FromPtr msg (Std_.Maybe (Untyped.Ptr msg)))) => (ThirdPartyCapDescriptor msg) -> (m (Std_.Maybe (Untyped.Ptr msg)))
get_ThirdPartyCapDescriptor'id (ThirdPartyCapDescriptor'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_ThirdPartyCapDescriptor'id :: ((Untyped.RWCtx m s)
                                  ,(Classes.ToPtr s (Std_.Maybe (Untyped.Ptr (Message.Mut s))))) => (ThirdPartyCapDescriptor (Message.Mut s)) -> (Std_.Maybe (Untyped.Ptr (Message.Mut s))) -> (m ())
set_ThirdPartyCapDescriptor'id (ThirdPartyCapDescriptor'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_ThirdPartyCapDescriptor'id :: ((Untyped.ReadCtx m msg)) => (ThirdPartyCapDescriptor msg) -> (m Std_.Bool)
has_ThirdPartyCapDescriptor'id (ThirdPartyCapDescriptor'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
get_ThirdPartyCapDescriptor'vineId :: ((Untyped.ReadCtx m msg)) => (ThirdPartyCapDescriptor msg) -> (m Std_.Word32)
get_ThirdPartyCapDescriptor'vineId (ThirdPartyCapDescriptor'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_ThirdPartyCapDescriptor'vineId :: ((Untyped.RWCtx m s)) => (ThirdPartyCapDescriptor (Message.Mut s)) -> Std_.Word32 -> (m ())
set_ThirdPartyCapDescriptor'vineId (ThirdPartyCapDescriptor'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word32) 0 0 0)
newtype Exception msg
    = Exception'newtype_ (Untyped.Struct msg)
instance (Classes.FromStruct msg (Exception msg)) where
    fromStruct struct = (Std_.pure (Exception'newtype_ struct))
instance (Classes.ToStruct msg (Exception msg)) where
    toStruct (Exception'newtype_ struct) = struct
instance (Untyped.HasMessage (Exception mut) mut) where
    message (Exception'newtype_ struct) = (Untyped.message struct)
instance (Untyped.MessageDefault (Exception mut) mut) where
    messageDefault msg = (Exception'newtype_ <$> (Untyped.messageDefault msg))
instance (Classes.FromPtr msg (Exception msg)) where
    fromPtr msg ptr = (Exception'newtype_ <$> (Classes.fromPtr msg ptr))
instance (Classes.ToPtr s (Exception (Message.Mut s))) where
    toPtr msg (Exception'newtype_ struct) = (Classes.toPtr msg struct)
instance (Classes.Allocate s (Exception (Message.Mut s))) where
    new msg = (Exception'newtype_ <$> (Untyped.allocStruct msg 1 1))
instance (Basics.ListElem mut (Exception mut)) where
    newtype List mut (Exception mut)
        = Exception'List_ (Untyped.ListOf mut (Untyped.Struct mut))
    listFromPtr msg ptr = (Exception'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Exception'List_ l) = (Untyped.ListStruct l)
    length (Exception'List_ l) = (Untyped.length l)
    index i (Exception'List_ l) = (do
        elt <- (Untyped.index i l)
        (Classes.fromStruct elt)
        )
instance (Basics.MutListElem s (Exception (Message.Mut s))) where
    setIndex (Exception'newtype_ elt) i (Exception'List_ l) = (Untyped.setIndex elt i l)
    newList msg len = (Exception'List_ <$> (Untyped.allocCompositeList msg 1 1 len))
get_Exception'reason :: ((Untyped.ReadCtx m msg)
                        ,(Classes.FromPtr msg (Basics.Text msg))) => (Exception msg) -> (m (Basics.Text msg))
get_Exception'reason (Exception'newtype_ struct) = (do
    ptr <- (Untyped.getPtr 0 struct)
    (Classes.fromPtr (Untyped.message struct) ptr)
    )
set_Exception'reason :: ((Untyped.RWCtx m s)
                        ,(Classes.ToPtr s (Basics.Text (Message.Mut s)))) => (Exception (Message.Mut s)) -> (Basics.Text (Message.Mut s)) -> (m ())
set_Exception'reason (Exception'newtype_ struct) value = (do
    ptr <- (Classes.toPtr (Untyped.message struct) value)
    (Untyped.setPtr ptr 0 struct)
    )
has_Exception'reason :: ((Untyped.ReadCtx m msg)) => (Exception msg) -> (m Std_.Bool)
has_Exception'reason (Exception'newtype_ struct) = (Std_.isJust <$> (Untyped.getPtr 0 struct))
new_Exception'reason :: ((Untyped.RWCtx m s)) => Std_.Int -> (Exception (Message.Mut s)) -> (m (Basics.Text (Message.Mut s)))
new_Exception'reason len struct = (do
    result <- (Basics.newText (Untyped.message struct) len)
    (set_Exception'reason struct result)
    (Std_.pure result)
    )
get_Exception'obsoleteIsCallersFault :: ((Untyped.ReadCtx m msg)) => (Exception msg) -> (m Std_.Bool)
get_Exception'obsoleteIsCallersFault (Exception'newtype_ struct) = (GenHelpers.getWordField struct 0 0 0)
set_Exception'obsoleteIsCallersFault :: ((Untyped.RWCtx m s)) => (Exception (Message.Mut s)) -> Std_.Bool -> (m ())
set_Exception'obsoleteIsCallersFault (Exception'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word1) 0 0 0)
get_Exception'obsoleteDurability :: ((Untyped.ReadCtx m msg)) => (Exception msg) -> (m Std_.Word16)
get_Exception'obsoleteDurability (Exception'newtype_ struct) = (GenHelpers.getWordField struct 0 16 0)
set_Exception'obsoleteDurability :: ((Untyped.RWCtx m s)) => (Exception (Message.Mut s)) -> Std_.Word16 -> (m ())
set_Exception'obsoleteDurability (Exception'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 16 0)
get_Exception'type_ :: ((Untyped.ReadCtx m msg)) => (Exception msg) -> (m Exception'Type)
get_Exception'type_ (Exception'newtype_ struct) = (GenHelpers.getWordField struct 0 32 0)
set_Exception'type_ :: ((Untyped.RWCtx m s)) => (Exception (Message.Mut s)) -> Exception'Type -> (m ())
set_Exception'type_ (Exception'newtype_ struct) value = (GenHelpers.setWordField struct ((Std_.fromIntegral (Classes.toWord value)) :: Std_.Word16) 0 32 0)
data Exception'Type 
    = Exception'Type'failed 
    | Exception'Type'overloaded 
    | Exception'Type'disconnected 
    | Exception'Type'unimplemented 
    | Exception'Type'unknown' Std_.Word16
    deriving(Std_.Show
            ,Std_.Read
            ,Std_.Eq
            ,Generics.Generic)
instance (Classes.IsWord Exception'Type) where
    fromWord n = case ((Std_.fromIntegral n) :: Std_.Word16) of
        0 ->
            Exception'Type'failed
        1 ->
            Exception'Type'overloaded
        2 ->
            Exception'Type'disconnected
        3 ->
            Exception'Type'unimplemented
        tag ->
            (Exception'Type'unknown' tag)
    toWord (Exception'Type'failed) = 0
    toWord (Exception'Type'overloaded) = 1
    toWord (Exception'Type'disconnected) = 2
    toWord (Exception'Type'unimplemented) = 3
    toWord (Exception'Type'unknown' tag) = (Std_.fromIntegral tag)
instance (Std_.Enum Exception'Type) where
    fromEnum x = (Std_.fromIntegral (Classes.toWord x))
    toEnum x = (Classes.fromWord (Std_.fromIntegral x))
instance (Basics.ListElem mut Exception'Type) where
    newtype List mut Exception'Type
        = Exception'Type'List_ (Untyped.ListOf mut Std_.Word16)
    index i (Exception'Type'List_ l) = (Classes.fromWord <$> (Std_.fromIntegral <$> (Untyped.index i l)))
    listFromPtr msg ptr = (Exception'Type'List_ <$> (Classes.fromPtr msg ptr))
    toUntypedList (Exception'Type'List_ l) = (Untyped.List16 l)
    length (Exception'Type'List_ l) = (Untyped.length l)
instance (Classes.MutListElem s Exception'Type) where
    setIndex elt i (Exception'Type'List_ l) = (Untyped.setIndex (Std_.fromIntegral (Classes.toWord elt)) i l)
    newList msg size = (Exception'Type'List_ <$> (Untyped.allocList16 msg size))