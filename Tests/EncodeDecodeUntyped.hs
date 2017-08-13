{-# LANGUAGE QuasiQuotes #-}
module Tests.EncodeDecodeUntyped
    (encodeDecodeUntypedTests)
  where

-- These tests verify Control.Monad.CapNProto.MessageBuilder and
-- Data.CapNProto.Untyped against eachother and against capnp
-- encode/decode. They are not schema-aware.

import Prelude hiding (length)

import Control.Monad (void)
import Control.Monad.Primitive (RealWorld)
import Control.Monad.Quota
import Data.ByteString (ByteString)
import Data.Word
import Text.Heredoc (here)

import Test.HUnit (Assertion, assertEqual)

import Control.Monad.CapNProto.MessageBuilder
import Data.CapNProto.Blob (BlobSlice(..))
import Data.CapNProto.Schema
import qualified Data.CapNProto.Message as M
import Data.CapNProto.Untyped
import Tests.Util

-- Cross check capnp encode/decode, builder, and the readers. Verifies that:
--
-- * The reader doesn't choke on the output of capnp encode
-- * The reader doesn't choke on the output of the builder
-- * capnp decode on the output of the builder yields the message text.
encodeDecodeUntypedTest
    :: ( MsgMetaData -- schema and type name
       , String -- message in textual form
       , BuilderT p RealWorld IO () -- Builder for the message
       , (Maybe (Ptr (BlobSlice ByteString)) -> QuotaT IO ()) -- reader.
       , Int -- Quota to run the reader with.
       , Int -- Remaining quota for the reader.
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
        readerResult <- runQuotaT (rootPtr msg >>= reader) (Quota startQuota)
        assertEqual (show (meta, msgText)) ((), Quota endQuota) readerResult


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
      , "( a = 72, b = true )"
      , void $ withParent 1 $ buildStruct 2 0 $ do
            DataField 0 0 %~ (72 :: Word64)
            DataField 1 0 %~ ( 1 :: Word64)
      , \(Just (PtrStruct root)) -> do
            s <- get root
            words <- dataSection s
            2 <- length words
            72 <- get =<< index 0 words
            1 <- get =<< index 1 words
            ptrs <- ptrSection s
            0 <- length ptrs
            return ()
      , 128
      , 122
      )
    ]
