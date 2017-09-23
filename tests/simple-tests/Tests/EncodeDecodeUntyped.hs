{-# LANGUAGE QuasiQuotes #-}
module Tests.EncodeDecodeUntyped
    (encodeDecodeUntypedTests)
  where

-- These tests verify Control.Monad.CapNProto.MessageBuilder and
-- Data.CapNProto.Untyped against eachother and against capnp
-- encode/decode. They are not schema-aware.

import Prelude hiding (length)

import Control.Monad.CapNProto.MessageBuilder
import Control.Monad.Quota
import Data.CapNProto.Schema
import Data.CapNProto.Untyped
import Data.Word
import Tests.Util

import Control.Monad           (void)
import Control.Monad.Primitive (RealWorld)
import Data.ByteString         (ByteString)
import Test.Framework          (Test)
import Test.HUnit              (Assertion, assertEqual)
import Text.Heredoc            (here)

import qualified Data.CapNProto.Message as M

-- Cross check capnp encode/decode, builder, and the readers. Verifies that:
--
-- * The reader doesn't choke on the output of capnp encode
-- * The reader doesn't choke on the output of the builder
-- * capnp decode on the output of the builder yields the message text.
encodeDecodeUntypedTest
    :: ( MsgMetaData -- schema and type name
       , String -- message in textual form
       , BuilderT p RealWorld IO () -- Builder for the message
       , Maybe (Ptr ByteString) -> QuotaT IO () -- reader.
       , Quota -- Quota to run the reader with.
       , Quota -- Remaining quota for the reader.
       )
    -> Assertion
encodeDecodeUntypedTest (meta, msgText, builder, reader, startQuota, endQuota) = do
    (blobSlice, ()) <- runBuilderT (frameSegment builder)
    bytes <- freezeAsByteString blobSlice
    actualText <- capnpDecode bytes meta
    assertEqual "Builder to text" msgText actualText
    checkReader bytes
    checkReader =<< capnpEncode msgText meta
  where
    checkReader bytes = do
        msg <- M.decode bytes
        readerResult <- runQuotaT (rootPtr msg >>= reader) startQuota
        assertEqual (show (meta, msgText)) ((), endQuota) readerResult


encodeDecodeUntypedTests :: Test
encodeDecodeUntypedTests =
    assertionsToTest "encode-decode-untyped tests" $ map encodeDecodeUntypedTest
    [ ( MsgMetaData
         { msgSchema = [here|@0xaa6fda3b444d262c;
                          struct A {
                             a @0 :UInt64;
                             b @1 :Bool;
                          }
                       |]
         , msgType = "A"
         }
      , "(a = 72, b = true)\n"
      , void $ withParent 1 $ buildStruct 2 0 $ do
            DataField 0 0 %~ (72 :: Word64)
            DataField 1 0 %~ ( 1 :: Word64)
      , \(Just (PtrStruct root)) -> do
            dataWords <- dataSection root
            2 <- length dataWords
            72 <- index 0 dataWords
            1 <- index 1 dataWords
            ptrs <- ptrSection root
            0 <- length ptrs
            return ()
      , 128
      , 125
      )
    ]
